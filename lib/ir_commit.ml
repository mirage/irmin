(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Ir_misc.OP
open Lwt
open Ir_merge.OP

module Log = Log.Make(struct let section = "COMMIT" end)

module type S = sig
  include Tc.S0
  type commit
  type node
  val create: Ir_task.t -> ?node:node -> parents:commit list -> t
  val node: t -> node option
  val parents: t -> commit list
  val task: t -> Ir_task.t
end

module Make (C: Tc.S0) (N: Tc.S0) = struct

  module T = Ir_task
  type node = N.t

  type t = {
    node   : N.t option;
    parents: C.t list;
    task : Ir_task.t;
  }

  let parents t = t.parents
  let node t = t.node
  let task t = t.task
  let create task ?node ~parents = { node; parents; task }

  let to_json t =
    `O [
      ("node"   , Ezjsonm.option N.to_json t.node);
      ("parents", Ezjsonm.list C.to_json t.parents);
      ("task"   , T.to_json t.task);
    ]

  let of_json j =
    let node    = Ezjsonm.find j ["node"]    |> Ezjsonm.get_option N.of_json in
    let parents = Ezjsonm.find j ["parents"] |> Ezjsonm.get_list C.of_json in
    let task    = Ezjsonm.find j ["task"]    |> T.of_json in
    { node; parents; task }

  module X = Tc.Triple(Tc.Option(N))(Tc.List(C))(T)
  let explode t = t.node, t.parents, t.task
  let implode (node, parents, task) = { node; parents; task }
  let x = Tc.biject (module X) implode explode

  let hash = Tc.hash x
  let compare = Tc.compare x
  let equal = Tc.equal x
  let size_of = Tc.size_of x
  let write = Tc.write x
  let read = Tc.read x

end

module type STORE = sig

  include Ir_ao.STORE

  module Key: Ir_hash.S with type t = key
  (** Base functions over keys. *)

  module Val: S
    with type t = value
     and type commit := key
  (** Base functions over values. *)

end

module type HISTORY = sig
  type t
  type node
  type commit
  val create: t -> ?node:node -> parents:commit list -> commit Lwt.t
  val node: t -> commit -> node option Lwt.t
  val parents: t -> commit -> commit list Lwt.t
  val merge: t -> commit Ir_merge.t
  val lca: t -> ?max_depth:int -> ?n:int -> commit -> commit ->
    [`Ok of commit list | `Max_depth_reached | `Too_many_lcas] Lwt.t
  val closure: t -> min:commit list -> max:commit list -> commit list Lwt.t
  module Store: Ir_contents.STORE with type t = t and type key = commit
end

module History (N: Ir_contents.STORE) (S: STORE with type Val.node = N.key) =
struct

  type commit = S.key
  type node = N.key

  module Store = struct

    type t = N.t * S.t
    type key = S.key
    type value = S.value

    let create config task =
      N.create config task >>= fun n ->
      S.create config task >>= fun s ->
      return (fun a -> (n a, s a))

    let task (_, t) = S.task t
    let add (_, t) = S.add t
    let mem (_, t) = S.mem t
    let read (_, t) = S.read t
    let read_exn (_, t) = S.read_exn t
    let merge_node path (n, _) = N.merge path n

    let merge_commit path t ~old k1 k2 =
      read_exn t k1  >>= fun v1   ->
      read_exn t k2  >>= fun v2   ->
      if List.mem k1 (S.Val.parents v2) then ok k2
      else if List.mem k2 (S.Val.parents v1) then ok k1
      else
        (* FIXME: check that old<>k1 and old<>k2 and don't create a
           new commit in that case. *)
        let old () =
          old () >>| function
          | None     -> ok None
          | Some old ->
            read_exn t old >>= fun vold ->
            ok (Some (S.Val.node vold))
        in
        merge_node path t ~old (S.Val.node v1) (S.Val.node v2)
        >>| fun node ->
        let parents = [k1; k2] in
        let commit = S.Val.create ?node ~parents (task t) in
        add t commit >>= fun key ->
        ok key

    let merge path t = Ir_merge.option (module S.Key) (merge_commit path t)

    module Key = S.Key
    module Val = struct
      include S.Val
      let merge _path ~old:_ _ _ = conflict "Commit.Val"
      module Path = N.Path
    end
    module Path = N.Path
  end

  type t = Store.t
  let merge = Store.merge_commit N.Path.empty

  let node t c =
    Log.debug "node %a" force (show (module S.Key) c);
    Store.read t c >>= function
    | None   -> return_none
    | Some n -> return (S.Val.node n)

  let create (_, t) ?node ~parents =
    let commit = S.Val.create ?node ~parents (S.task t) in
    S.add t commit >>= fun key ->
    return key

  let parents t c =
    Log.debug "parents %a" force (show (module S.Key) c);
    Store.read t c >>= function
    | None   -> return_nil
    | Some c -> return (S.Val.parents c)

  module Graph = Ir_graph.Make(Ir_hum.Unit)(N.Key)(S.Key)(Ir_hum.Unit)

  let edges t =
    Log.debug "edges";
    (match S.Val.node t with
      | None   -> []
      | Some k -> [`Node k])
    @ List.map (fun k -> `Commit k) (S.Val.parents t)

  let closure t ~min ~max =
    Log.debug "closure";
    let pred = function
      | `Commit k -> Store.read_exn t k >>= fun r -> return (edges r)
      | _         -> return_nil in
    let min = List.map (fun k -> `Commit k) min in
    let max = List.map (fun k -> `Commit k) max in
    Graph.closure ~pred ~min ~max () >>= fun g ->
    let keys =
      Ir_misc.list_filter_map
        (function `Commit k -> Some k | _ -> None)
        (Graph.vertex g)
    in
    return keys

  module KSet = Ir_misc.Set(S.Key)
  let (++) = KSet.union
  let (--) = KSet.diff
  let ( ** ) = KSet.inter

  let proj g suffix =
    let g' = Graph.create ~size:(KSet.cardinal suffix) () in
    KSet.iter (fun k -> Graph.add_vertex g' (`Commit k)) suffix;
    KSet.iter (fun k ->
        let succ = Graph.succ g (`Commit k) in
        let succ =
          List.filter (function
              | `Commit k -> KSet.mem k suffix
              | _ -> false
            ) succ
        in
        List.iter (fun s -> Graph.add_edge g' (`Commit k) s) succ
      ) suffix;
    g'

  let add g (x, y) = Graph.add_edge g (`Commit x) (`Commit y)
  let output edges = edges |> List.map snd |> KSet.of_list
  let next t edges =
    let read_parents k =
      Store.read_exn t k >>= fun v ->
      S.Val.parents v
      |> List.map (fun p -> (k, p))
      |> Lwt.return
    in
    let edges = KSet.to_list edges in
    Lwt_list.map_p read_parents edges >>= fun edges ->
    Lwt.return (List.flatten edges)

  type prefix = {
    n: int;
    g: Graph.t;
    seen1 : KSet.t;
    seen2 : KSet.t;
    shared: KSet.t;
    todo1 : KSet.t;
    todo2 : KSet.t;
  }

  let pr_keys keys =
    let key x = String.sub (S.Key.to_hum x) 0 4 in
    let keys = KSet.to_list keys in
    String.concat " " (List.map key keys)

  let empty_prefix () = {
    n = 0;
    g = Graph.create ();
    seen1  = KSet.empty;
    seen2  = KSet.empty;
    shared = KSet.empty;
    todo1  = KSet.empty;
    todo2  = KSet.empty;
  }

  (* compute the shared frontier

     An element is in the shared frontier if
     - it is in the active shared frontier (prefix.shared)
     - it is seen by both vertices
     - it is on the frontier (ie. it doens't have predecessors in the
       shared frontier)
  *)
  let shared_frontier p =
    let seen = p.shared ++ (p.seen1 ** p.seen2) in
    let proj = proj p.g seen in
    Graph.min proj
    |> List.fold_left (fun acc ->
        function `Commit k -> KSet.add k acc | _ -> acc
      ) KSet.empty

  let pr_prefix ({n; seen1; seen2; shared; todo1; todo2; _ } as p) =
    Printf.sprintf "n:%d seen1:%s, seen2:%s, s:%s, 1:%s, 2:%s, res:%s" n
      (pr_keys seen1) (pr_keys seen2) (pr_keys shared)
      (pr_keys todo1) (pr_keys todo2) (pr_keys (shared_frontier p))

  let show_prefix p = lazy (pr_prefix p)

  (* compute the next prefix state.

     Invariants:
       - at step n: [n] is at least the depth of [g]
       - seen1: the prefix of [g] known by [p1]
       - seen2: the prefix of [g] known by [p2]
       - shared: max([g]) dominated by both [p1] and [p2]
       - todo1: max([g]) dominated by [p1]
       - todo2: max([g]) dominated by [p2]
  *)
  let check_prefix p =
    assert (p.todo1 ** p.todo2 = KSet.empty);
    assert (p.todo1 ** p.shared = KSet.empty);
    assert (p.todo1 ** p.seen1 = KSet.empty);
    assert (p.todo1 ** p.seen2 = KSet.empty);
    assert (p.todo2 ** p.shared = KSet.empty);
    assert (p.todo2 ** p.shared = KSet.empty);
    assert (p.todo2 ** p.seen1 = KSet.empty);
    assert (p.todo2 ** p.seen2 = KSet.empty)

  let next_prefix t p =
    check_prefix p;

    next t p.todo1  >>= fun edges1 ->
    next t p.todo2  >>= fun edges2 ->
    next t p.shared >>= fun edgess ->
    List.iter (add p.g) edges1;
    List.iter (add p.g) edges2;
    List.iter (add p.g) edgess;

    let output1 = output edges1 in
    let output2 = output edges2 in
    let outputs = output edgess in

    let seen1_ = p.shared ++ p.todo1 ++ p.seen1 in
    let seen2_ = p.shared ++ p.todo2 ++ p.seen2 in
    let not_fresh = outputs ++ seen1_ ++ seen2_ in

    (* enforce invariants *)
    let todo1 = output1 -- output2 -- not_fresh in
    let todo2 = output2 -- output1 -- not_fresh in
    let seen1 = seen1_ ++ (output1 ** not_fresh) in
    let seen2 = seen2_ ++ (output2 ** not_fresh) in

    let shared =
      outputs ++ output1 ++ output2 -- todo1 -- todo2 -- seen1 -- seen2
    in
    Lwt.return { p with n = p.n + 1; seen1; seen2; shared; todo1; todo2 }

  let lca_calls = ref 0
  let lca t ?(max_depth=256) ?n c1 c2 =
    incr lca_calls;
    let ok set = Lwt.return (`Ok (KSet.to_list set)) in
    let rec aux prefix =
      Log.debug "lca %d %a" !lca_calls force (show_prefix prefix);
      if prefix.n > max_depth then Lwt.return `Max_depth_reached
      else if KSet.is_empty prefix.todo1 && KSet.is_empty prefix.todo2 then
        ok (shared_frontier prefix)
      else
        match n with
        | None   -> next_prefix t prefix >>= aux
        | Some n ->
          let r = shared_frontier prefix in
          let c = KSet.cardinal r in
          if c = n then ok r
          else if c > n then Lwt.return `Too_many_lcas
          else next_prefix t prefix >>= aux
    in
    let prefix = empty_prefix () in
    let prefix =
      { prefix with todo1 = KSet.singleton c1; todo2 = KSet.singleton c2 }
    in
    aux prefix

end
