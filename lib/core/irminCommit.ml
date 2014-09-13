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
open IrminCore
open IrminMerge.OP
open Printf
open Sexplib.Std
open Bin_prot.Std

type origin = IrminOrigin.t

module T_ = struct
  type ('origin, 'key) t = {
    node   : 'key option;
    parents: 'key list;
    origin : 'origin;
  } with bin_io, compare, sexp
end
include T_
module T = I2(T_)

let edges t =
  begin match t.node with
    | None   -> []
    | Some k -> [`Node k]
  end
  @ List.map ~f:(fun k -> `Commit k) t.parents

module type S = sig
  type key
  include IrminContents.S with type t = (origin, key) t
end

module S (K: IrminKey.S) = struct
  type key = K.t
  module S = App2(T)(IrminOrigin)(K)
  include S
  let merge = IrminMerge.default (module S)
end

module SHA1 = S(IrminKey.SHA1)

module type STORE = sig
  type key
  type value = (origin, key) t
  include IrminStore.AO with type key := key
                         and type value := value
  type node = key IrminNode.t
  val commit: t -> origin -> ?node:key IrminNode.t -> parents:value list -> (key * value) Lwt.t
  val node: t -> value -> key IrminNode.t Lwt.t option
  val parents: t -> value -> value Lwt.t list
  val merge: t -> key IrminMerge.t
  val find_common_ancestor: t -> key -> key -> key option Lwt.t
  val find_common_ancestor_exn: t -> key -> key -> key Lwt.t
  val list: t -> ?depth:int -> key list -> key list Lwt.t
  module Key: IrminKey.S with type t = key
  module Value: S with type key = key
end

module Make
    (K     : IrminKey.S)
    (Node  : IrminNode.STORE with type key   = K.t
                              and type value = K.t IrminNode.t)
    (Commit: IrminStore.AO   with type key   = K.t
                              and type value = (origin, K.t) t)
= struct

  type key = K.t
  type value = (origin, key) t
  type t = Node.t * Commit.t
  type node = key IrminNode.t

  module Key = K
  module Value = S(K)

  let create () =
    Node.create ()   >>= fun n ->
    Commit.create () >>= fun c ->
    return (n, c)

  let add (_, t) c =
    Commit.add t c

  let read (_, t) c =
    Commit.read t c

  let read_exn (_, t) c =
    Commit.read_exn t c

  let mem (_, t) c =
    Commit.mem t c

  let node (n, _) c =
    match c.node with
    | None   -> None
    | Some k -> Some (Node.read_exn n k)

  let commit (n, c) origin ?node ~parents =
    begin match node with
      | None      -> return_none
      | Some node -> Node.add n node >>= fun k -> return (Some k)
    end
    >>= fun node ->
    Lwt_list.map_p (Commit.add c) parents
    >>= fun parents ->
    let commit = { node; parents; origin } in
    Commit.add c commit >>= fun key ->
    return (key, commit)

  let parents t c =
    List.map ~f:(read_exn t) c.parents

  module Graph = IrminGraph.Make(K)(IrminTag.String)

  let list t ?depth keys =
    Log.debugf "list %a" force (prettys (module K) keys);
    let pred = function
      | `Commit k -> read_exn t k >>= fun r -> return (edges r)
      | _         -> return_nil in
    let max = IrminGraph.of_commits keys in
    Graph.closure ?depth max ~pred >>= fun g ->
    let keys = IrminGraph.to_commits (Graph.vertex g) in
    return keys

  let dump (_, t) =
    Commit.dump t

  let merge_node n =
    IrminMerge.some (Node.merge n)

  let merge (n, _ as t) =
    let merge ~origin ~old k1 k2 =
      read_exn t old >>= fun vold ->
      read_exn t k1  >>= fun v1   ->
      read_exn t k2  >>= fun v2   ->
      IrminMerge.merge (merge_node n) ~origin ~old:vold.node v1.node v2.node >>| fun node ->
      let parents = [k1; k2] in
      let commit = { node; parents; origin } in
      add t commit >>= fun key ->
      ok key
    in
    IrminMerge.create' (module K) merge

  module KSet = Set.Make(K)

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
              let parents = KSet.of_list v.parents in
              return (KSet.diff parents seen) in
            Lwt_list.fold_left_s parents_of_commit todo (KSet.to_list todo)
          in
          parents todo1 >>= fun todo1' ->
          parents todo2 >>= fun todo2' ->
          aux (seen1', todo1') (seen2', todo2')
        | [r] -> return (Some r)
        | rs  -> fail (Failure (sprintf "Multiple common ancestor: %s"
                                  (List.pretty K.pretty rs))) in
    aux
      (KSet.empty, KSet.singleton c1)
      (KSet.empty, KSet.singleton c2)

  let find_common_ancestor_exn t c1 c2 =
    find_common_ancestor t c1 c2 >>= function
    | Some r -> return r
    | None   -> fail Not_found

end

module Rec (S: STORE) = struct
  include S.Key
  let merge =
    let merge ~origin ~old k1 k2 =
      S.create ()  >>= fun t  ->
      IrminMerge.merge (S.merge t) ~origin ~old k1 k2
    in
    IrminMerge.create' (module S.Key) merge
end
