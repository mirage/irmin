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
open Sexplib.Std
open Bin_prot.Std

type ('origin, 'key) t = {
  node   : 'key option;
  parents: 'key list;
  origin : 'origin;
}

let edges t =
  begin match t.node with
    | None   -> []
    | Some k -> [`Node k]
  end
  @ List.map (fun k -> `Commit k) t.parents

module type S = sig
  type key
  type origin
  include Ir_contents.S with type t = (origin, key) t
end

module type STORE = sig
  type key
  type origin
  type value = (origin, key) t
  include Ir_ao.S with type key := key and type value := value
  type node = key Ir_node.t
  val commit: t -> origin -> ?node:key Ir_node.t -> parents:value list -> (key * value) Lwt.t
  val node: t -> value -> key Ir_node.t Lwt.t option
  val parents: t -> value -> value Lwt.t list
  val merge: t -> key Ir_merge.t
  val find_common_ancestor: t -> key -> key -> key option Lwt.t
  val find_common_ancestor_exn: t -> key -> key -> key Lwt.t
  val list: t -> ?depth:int -> key list -> key list Lwt.t
  module Key: Ir_uid.S with type t = key
  module Value: S with type key = key
end

module S (K: Ir_uid.S) = struct
  type key = K.t
  type origin = Ir_origin.t
  module S = Tc.App2(T)(Ir_origin)(K)
  include S
  let merge = Ir_merge.default (module S)
end

module Make (N: Ir_node.STORE) (C: Ir_ao.S with type key = N.key)
= struct

  module K = N.Key
  type key = K.t
  type origin = Ir_origin.t
  type value = (origin, key) t
  type t = N.t * C.t
  type node = key Ir_node.t

  module Key = K
  module Value = S(K)

  let create () =
    N.create () >>= fun n ->
    C.create () >>= fun c ->
    return (n, c)

  let add (_, t) c =
    C.add t c

  let read (_, t) c =
    C.read t c

  let read_exn (_, t) c =
    C.read_exn t c

  let mem (_, t) c =
    C.mem t c

  let node (n, _) c =
    match c.node with
    | None   -> None
    | Some k -> Some (N.read_exn n k)

  let commit (n, c) origin ?node ~parents =
    begin match node with
      | None      -> return_none
      | Some node -> N.add n node >>= fun k -> return (Some k)
    end
    >>= fun node ->
    Lwt_list.map_p (C.add c) parents
    >>= fun parents ->
    let commit = { node; parents; origin } in
    C.add c commit >>= fun key ->
    return (key, commit)

  let parents t c =
    List.map (read_exn t) c.parents

  module Graph = Ir_graph.Make(K)(Ir_tag.String)

  let list t ?depth keys =
    Log.debugf "list %a" force (shows (module K) keys);
    let pred = function
      | `Commit k -> read_exn t k >>= fun r -> return (edges r)
      | _         -> return_nil in
    let max = Ir_graph.of_commits keys in
    Graph.closure ?depth max ~pred >>= fun g ->
    let keys = Ir_graph.to_commits (Graph.vertex g) in
    return keys

  let dump (_, t) =
    C.dump t

  let merge_node n =
    Ir_merge.some (N.merge n)

  let merge (n, _ as t) =
    let merge ~origin ~old k1 k2 =
      read_exn t old >>= fun vold ->
      read_exn t k1  >>= fun v1   ->
      read_exn t k2  >>= fun v2   ->
      Ir_merge.merge (merge_node n) ~origin ~old:vold.node v1.node v2.node >>| fun node ->
      let parents = [k1; k2] in
      let commit = { node; parents; origin } in
      add t commit >>= fun key ->
      ok key
    in
    Ir_merge.create' (module K) merge

  module KSet = Ir_misc.Set(K)

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
                                  (Tc.shows (module K) rs))) in
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
      Ir_merge.merge (S.merge t) ~origin ~old k1 k2
    in
    Ir_merge.create' (module S.Key) merge
end

module SHA1 = S(Ir_uid.SHA1)
