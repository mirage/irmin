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

module Log = Log.Make(struct let section = "COMMIT" end)

open Lwt
open Ir_merge.OP
open Ir_misc.OP
open Printf

module type S = sig
  include Tc.S0
  type commit
  type node
  val create: Ir_task.t -> ?node:node -> parents:commit list -> t
  val node: t -> node option
  val parents: t -> commit list
  val task: t -> Ir_task.t
  val edges: t -> [> `Node of node | `Commit of commit] list
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

  let to_sexp t =
    let open Sexplib.Type in
    let open Sexplib.Conv in
    List [
      List [ Atom "node"   ; sexp_of_option N.to_sexp t.node ];
      List [ Atom "parents"; sexp_of_list C.to_sexp t.parents ];
      List [ Atom "task"   ; T.to_sexp t.task ]
    ]

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

  let edges t =
    begin match t.node with
      | None   -> []
      | Some k -> [`Node k]
    end
    @ List.map (fun k -> `Commit k) t.parents

  module X = Tc.Triple(Tc.Option(N))(Tc.List(C))(T)

  let explode t = t.node, t.parents, t.task
  let implode (node, parents, task) = { node; parents; task }

  let hash t = X.hash (explode t)
  let compare x y = X.compare (explode x) (explode y)
  let equal x y = X.equal (explode x) (explode y)
  let size_of t = X.size_of (explode t)
  let write t b = X.write (explode t) b
  let read b = implode (X.read b)

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

module type STORE_EXT = sig
  module Node: Ir_node.STORE
  include STORE with type Val.node = Node.key
  type node = Node.value
  val commit: t -> ?node:node -> parents:value list -> (key * value) Lwt.t
  val node: t -> value -> node Lwt.t option
  val parents: t -> value -> value Lwt.t list
  val merge: t -> key Ir_merge.t
  val find_common_ancestor: t -> key -> key -> key option Lwt.t
  val find_common_ancestor_exn: t -> key -> key -> key Lwt.t
  val node_t: t -> Node.t
end

module Make_ext
    (C: Ir_contents.STORE)
    (N: Ir_node.STORE with type Val.contents = C.key)
    (S: STORE with type Val.node = N.key) =
struct

  module Val = S.Val
  module Key = S.Key

  type key = Key.t
  type value = Val.t
  type t = C.t * N.t * S.t
  type node = N.value

  module Node = Ir_node.Store(C)(N)

  let node_t: t -> Node.t = function (c, n, _) -> (c, n)

  let create config task =
    C.create config task >>= fun c ->
    N.create config task >>= fun n ->
    S.create config task >>= fun s ->
    return (c, n, s)

  let task (_, _, t) =
    S.task t

  let config (_, _, t) =
    S.config t

  let add (_, _, t) c =
    S.add t c

  let read (_, _, t) c =
    S.read t c

  let read_exn (_, _, t) c =
    S.read_exn t c

  let mem (_, _, t) c =
    S.mem t c

  let node (_, t, _) c =
    match Val.node c with
    | None   -> None
    | Some k -> Some (N.read_exn t k)

  let commit (_, n, t) ?node ~parents =
    begin match node with
      | None      -> return_none
      | Some node -> N.add n node >>= fun k -> return (Some k)
    end
    >>= fun node ->
    Lwt_list.map_p (S.add t) parents
    >>= fun parents ->
    let commit = Val.create ?node ~parents (S.task t) in
    S.add t commit >>= fun key ->
    return (key, commit)

  let parents t c =
    List.map (read_exn t) (Val.parents c)

  module Graph = Ir_graph.Make(C.Key)(N.Key)(Key)(Tc.Unit)

  let list t keys =
    Log.debugf "list %a" force (shows (module Key) keys);
    let pred = function
      | `Commit k -> read_exn t k >>= fun r -> return (Val.edges r)
      | _         -> return_nil in
    let max = List.map (fun k -> `Commit k) keys in
    Graph.closure max ~pred >>= fun g ->
    let keys =
      Ir_misc.list_filter_map
        (function `Commit k -> Some k | _ -> None)
        (Graph.vertex g)
    in
    return keys

  let dump (_, _, t) =
    S.dump t

  let merge_node t =
    Ir_merge.some (module N.Key) (Node.merge (node_t t))

  let merge t ~old k1 k2 =
    read_exn t old >>= fun vold ->
    read_exn t k1  >>= fun v1   ->
    read_exn t k2  >>= fun v2   ->
    merge_node t ~old:(Val.node vold) (Val.node v1) (Val.node v2) >>|
    fun node ->
    let parents = [k1; k2] in
    let commit = Val.create ?node ~parents (task t) in
    add t commit >>= fun key ->
    ok key

  module KSet = Ir_misc.Set(Key)

  let find_common_ancestor t c1 c2 =
    let rec aux (seen1, todo1) (seen2, todo2) =
      if KSet.is_empty todo1 && KSet.is_empty todo2 then
        return_none
      else
        let seen1' = KSet.union seen1 todo1 in
        let seen2' = KSet.union seen2 todo2 in
        match KSet.to_list (KSet.inter seen1' seen2') with
        | []  ->
          (* Compute the immediate parents *)
          let parents todo =
            let parents_of_commit seen c =
              read_exn t c >>= fun v ->
              let parents = KSet.of_list (Val.parents v) in
              return (KSet.diff parents seen) in
            Lwt_list.fold_left_s parents_of_commit todo (KSet.to_list todo)
          in
          parents todo1 >>= fun todo1' ->
          parents todo2 >>= fun todo2' ->
          aux (seen1', todo1') (seen2', todo2')
        | [r] -> return (Some r)
        | rs  -> fail (Failure (sprintf "Multiple common ancestor: %s"
                                  (Tc.shows (module Key) rs))) in
    aux
      (KSet.empty, KSet.singleton c1)
      (KSet.empty, KSet.singleton c2)

  let find_common_ancestor_exn t c1 c2 =
    find_common_ancestor t c1 c2 >>= function
    | Some r -> return r
    | None   -> fail Not_found

end
