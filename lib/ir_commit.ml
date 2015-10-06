(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  val create: t -> ?node:node -> parents:commit list -> task:Ir_task.t -> commit Lwt.t
  val node: t -> commit -> node option Lwt.t
  val parents: t -> commit -> commit list Lwt.t
  val merge: t -> task:Ir_task.t -> commit Ir_merge.t
  val lcas: t -> ?max_depth:int -> ?n:int -> commit -> commit ->
    [`Ok of commit list | `Max_depth_reached | `Too_many_lcas] Lwt.t
  val lca: t -> task:Ir_task.t -> ?max_depth:int -> ?n:int -> commit list -> commit option Ir_merge.result Lwt.t
  val three_way_merge: t -> task:Ir_task.t -> ?max_depth:int -> ?n:int -> commit -> commit -> commit Ir_merge.result Lwt.t
  val closure: t -> min:commit list -> max:commit list -> commit list Lwt.t
  module Store: sig
    include STORE with type t = t and type key = commit
    module Path: Ir_path.S
    val merge: Path.t -> t -> task:Ir_task.t -> key option Ir_merge.t
  end
end

module History (N: Ir_contents.STORE) (S: STORE with type Val.node = N.key) =
struct

  type commit = S.key
  type node = N.key

  module Store = struct

    type t = N.t * S.t
    type key = S.key
    type value = S.value

    let create config =
      N.create config >>= fun n ->
      S.create config >>= fun s ->
      return (n, s)

    let add (_, t) = S.add t
    let mem (_, t) = S.mem t
    let read (_, t) = S.read t
    let read_exn (_, t) = S.read_exn t
    let merge_node path (n, _) = N.merge path n

    let merge_commit path t ~task ~old k1 k2 =
      read_exn t k1  >>= fun v1   ->
      read_exn t k2  >>= fun v2   ->
      if List.mem k1 (S.Val.parents v2) then ok k2
      else if List.mem k2 (S.Val.parents v1) then ok k1
      else
        (* If we get an error while looking the the lca, then we
           assume that there is no common ancestor. Maybe we want to
           expose this to the user in a more structured way. But maybe
           that's too much low-level details. *)
        begin old () >>= function
          | `Conflict msg ->
            Log.debug "old: conflict %s" msg;
            Lwt.return None
          | `Ok o -> Lwt.return o
        end >>= fun old ->
        if Tc.O1.equal S.Key.equal old (Some k1) then ok k2
        else if Tc.O1.equal S.Key.equal old (Some k2) then ok k1
        else
          let old () = match old with
            | None     -> ok None
            | Some old ->
              read_exn t old >>= fun vold ->
              ok (Some (S.Val.node vold))
          in
        merge_node path t ~old (S.Val.node v1) (S.Val.node v2)
        >>| fun node ->
        let parents = [k1; k2] in
        let commit = S.Val.create ?node ~parents task in
        add t commit >>= fun key ->
        ok key

    let merge path t ~task = Ir_merge.option (module S.Key) (merge_commit path t ~task)

    let iter (_, t) fn = S.iter t fn

    module Key = S.Key
    module Val = struct
      include S.Val
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

  let create (_, t) ?node ~parents ~task =
    let commit = S.Val.create ?node ~parents task in
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
  module KHashtbl = Hashtbl.Make(S.Key)

  let read_parents t commit =
    Store.read_exn t commit >|= fun c ->
    KSet.of_list (S.Val.parents c)

  let pp_key k = String.sub (S.Key.to_hum k) 0 4
  let pp_keys keys =
    let keys = KSet.to_list keys in
    Printf.sprintf "[%s]" @@ String.concat " " (List.map pp_key keys)

  let lca_calls = ref 0

  let rec unqueue todo seen =
    if Queue.is_empty todo then None
    else
      let (_ , commit as pop) = Queue.pop todo in
      if KSet.mem commit seen then unqueue todo seen
      else Some pop

  (* Traverse the graph of commits using a breadth first search
     strategy. Start by visiting the commits in [init] and stops
     either when [check] returns [`Stop] or when all the ancestors of
     [init] have been visited. *)
  let traverse_bfs t ~f ~pp:_ ~check ~init ~return =
    let todo = Queue.create () in
    let add_todo d x = Queue.add (d, x) todo in
    KSet.iter (add_todo 0) init;
    let rec aux seen = match check () with
      | `Too_many_lcas | `Max_depth_reached as x -> Lwt.return x
      | `Stop -> return ()
      | `Continue ->
        match unqueue todo seen with
        | None  -> return ()
        | Some (depth, commit) ->
          (* Log.debug "lca %d: %s.%d %a"
             !lca_calls (pp_key commit) depth force (pp ()); *)
          let seen = KSet.add commit seen in
          read_parents t commit >>= fun parents ->
          let () = f depth commit parents in
          let parents = KSet.diff parents seen in
          KSet.iter (add_todo (depth+1)) parents;
          aux seen
    in
    aux KSet.empty

  type mark = Seen1 | Seen2 | SeenBoth | LCA

  let _pp_mark = function
    | Seen1 -> "seen1" | Seen2 -> "seen2"
    | SeenBoth -> "seenBoth" | LCA -> "LCA"

  (* Exploration state *)
  type state = {
    marks  : mark KHashtbl.t;            (* marks of commits already explored *)
    parents: KSet.t KHashtbl.t;        (* parents of commits already explored *)
    layers : (int, KSet.t) Hashtbl.t;    (* layers of commit, sorted by depth *)
    c1: S.key;                                             (* initial state 1 *)
    c2: S.key;                                             (* initial state 2 *)
    mutable depth: int;                      (* the current exploration depth *)
    mutable lcas : int;                   (* number of commit marked with LCA *)
    mutable complete : bool;                  (* is the exploration complete? *)
  }

  let pp_state t = lazy (
    let pp m =
      KHashtbl.fold (fun k v acc -> if v = m then pp_key k :: acc else acc)
        t.marks []
      |> String.concat " "
    in
    Printf.sprintf "d: %d, seen1: %s, seen2: %s, seenboth: %s, lcas: %s (%d) %s"
      t.depth (pp Seen1) (pp Seen2) (pp SeenBoth) (pp LCA) t.lcas
      (String.concat " | " (
          (Hashtbl.fold (fun d ks acc ->
               Printf.sprintf "(%d: %s)" d (pp_keys ks) :: acc
             ) t.layers [])))
  )

  let get_mark_exn t elt = KHashtbl.find t.marks elt
  let get_mark t elt = try Some (get_mark_exn t elt) with Not_found -> None
  let set_mark t elt mark = KHashtbl.replace t.marks elt mark
  let get_layer t d = try Hashtbl.find t.layers d with Not_found -> KSet.empty
  let add_to_layer t d k = Hashtbl.replace t.layers d (KSet.add k (get_layer t d))
  let add_parent t c p = KHashtbl.add t.parents c p
  let get_parent t c = try KHashtbl.find t.parents c with Not_found -> KSet.empty
  let incr_lcas t = t.lcas <- t.lcas + 1
  let decr_lcas t = t.lcas <- t.lcas - 1

  let both_seen t k = match get_mark t k with
    | None | Some Seen1 | Some Seen2 -> false
    | _ -> true

  let empty_state c1 c2 =
    let t =  {
      marks   = KHashtbl.create 10;
      parents = KHashtbl.create 10;
      layers  = Hashtbl.create 10;
      c1; c2; depth = 0; lcas  = 0; complete = false;
    } in
    set_mark t c1 Seen1;
    set_mark t c2 Seen2;
    t

  (* update the parent mark and keep the number of lcas up-to-date. *)
  let update_mark t mark commit =
    let new_mark = match mark, get_mark t commit with
      | Seen1, Some Seen1 | Seen1, None -> Seen1
      | Seen2, Some Seen2 | Seen2, None -> Seen2
      | SeenBoth, Some LCA -> decr_lcas t; SeenBoth
      | SeenBoth, _ -> SeenBoth
      | Seen1, Some Seen2 | Seen2, Some Seen1 -> incr_lcas t; LCA
      | _ ,Some LCA -> LCA
      | _ -> SeenBoth
    in
    (* check for fast-forwards *)
    let is_init () = S.Key.equal commit t.c1 || S.Key.equal commit t.c2 in
    let is_shared () = new_mark = SeenBoth || new_mark = LCA in
    if is_shared () && is_init () then (
      Log.debug "fast-forward";
      t.complete <- true;
    );
    set_mark t commit new_mark;
    new_mark

  (* update the ancestors which have already been visisted. *)
  let update_ancestors_marks t mark commit =
    let todo = Queue.create () in
    Queue.add commit todo;
    let rec loop mark =
      if Queue.is_empty todo then ()
      else
        let a = Queue.pop todo in
        let old_mark = get_mark t a  in
        let mark = update_mark t mark a in
        let () = match old_mark with
          | Some (SeenBoth | LCA) -> ()
          | _ -> KSet.iter (fun x -> Queue.push x todo) (get_parent t a)
        in
        loop (if mark = LCA then SeenBoth else mark)
    in
    loop mark

  let update_parents t depth commit parents =
    add_parent t commit parents;
    add_to_layer t depth commit;
    if depth <> t.depth then (
      assert (depth = t.depth + 1);
      (* before starting to explore a new layer, check if we really
         have some work to do, ie. do we still have a commit seen only
         by one node? *)
      let layer = get_layer t t.depth in
      let complete = KSet.for_all (both_seen t) layer in
      if complete then t.complete <- true else t.depth <- depth
    );
    let mark = get_mark_exn t commit in
    KSet.iter (update_ancestors_marks t mark) parents

  let lcas t =
    KHashtbl.fold
      (fun k v acc -> if v = LCA then k :: acc else acc)
      t.marks []

  let check ~max_depth ~n t =
    if t.depth > max_depth then `Max_depth_reached
    else if t.lcas > n then `Too_many_lcas
    else if t.lcas = n || t.complete then `Stop
    else `Continue

  let lcas t ?(max_depth=max_int) ?(n=max_int) c1 c2 =
    incr lca_calls;
    if max_depth < 0 then Lwt.return `Max_depth_reached
    else if n <= 0 then Lwt.return `Too_many_lcas
    else if S.Key.equal c1 c2 then Lwt.return (`Ok [c1])
    else (
      let init = KSet.of_list [c1; c2] in
      let s = empty_state c1 c2 in
      let check () = check ~max_depth ~n s in
      let pp () = pp_state s in
      let return () = Lwt.return (`Ok (lcas s)) in
      let t0 = Sys.time () in
      Lwt.finalize
        (fun () -> traverse_bfs t ~f:(update_parents s) ~pp ~check ~init ~return)
        (fun () ->
           let t1 = Sys.time () -. t0 in
           Log.debug "lcas %d: depth=%d time=%.4fs" !lca_calls s.depth t1;
           Lwt.return_unit)
    )

  let rec three_way_merge t ~task ?max_depth ?n c1 c2 =
    Log.debug "3-way merge between %a and %a"
      force (show (module S.Key) c1)
      force (show (module S.Key) c2);
    if S.Key.equal c1 c2 then ok c1
    else (
      lcas t ?max_depth ?n c1 c2 >>= fun lcas ->
      let old () = match lcas with
        | `Too_many_lcas     -> conflict "Too many lcas"
        | `Max_depth_reached -> conflict "Max depth reached"
        | `Ok []             -> ok None (* no common ancestor *)
        | `Ok (old :: olds)  ->
        let rec aux acc = function
          | []        -> ok (Some acc)
          | old::olds ->
            three_way_merge t ~task acc old >>| fun acc ->
            aux acc olds
        in
        aux old olds
      in
      try merge t ~task ~old c1 c2
      with Ir_merge.Conflict msg ->
        conflict "Recursive merging of common ancestors: %s" msg
    )

  let lca_aux t ~task ?max_depth ?n c1 c2 =
    if S.Key.equal c1 c2 then ok (Some c1)
    else (
      lcas t ?max_depth ?n c1 c2 >>= function
      | `Too_many_lcas     -> conflict "Too many lcas"
      | `Max_depth_reached -> conflict "Max depth reached"
      | `Ok []             -> ok None (* no common ancestor *)
      | `Ok [x]            -> ok (Some x)
      | `Ok (c :: cs)  ->
        let rec aux acc = function
          | []    -> ok (Some acc)
          | c::cs ->
            three_way_merge t ~task ?max_depth ?n acc c >>= function
            | `Conflict _ -> ok None
            | `Ok acc     -> aux acc cs
        in
        aux c cs
    )

  let rec lca t ~task ?max_depth ?n = function
    | []  -> conflict "History.lca: empty"
    | [c] -> ok (Some c)
    | c1::c2::cs ->
      lca_aux t ~task ?max_depth ?n c1 c2 >>| function
      | None   -> ok None
      | Some c -> lca t ~task ?max_depth ?n (c::cs)

end
