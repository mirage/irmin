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

module type S = sig
  include Ir_contents.S
  type commit
  type node
  val create: origin -> ?node:node -> parents:commit list -> t
  val node: t -> node option
  val parents: t -> commit list
  val origin: t -> origin
  val edges: t -> [`Node of node | `Commit of commit] list
end

module type STORE = sig
  include Ir_ao.STORE
  type node
  val commit: t -> origin -> ?node:node -> parents:value list -> (key * value) Lwt.t
  val node: t -> origin -> value -> node Lwt.t option
  val parents: t -> origin -> value -> value Lwt.t list
  val merge: t -> (key, origin) Ir_merge.t
  val find_common_ancestor: t -> origin -> key -> key -> key option Lwt.t
  val find_common_ancestor_exn: t -> origin -> key -> key -> key Lwt.t
  val list: t -> origin -> ?depth:int -> key list -> key list Lwt.t
  module Node: Ir_node.STORE
    with type value = node
     and type origin = origin
  val node_t: t -> Node.t
  module Key: Ir_uid.S with type t = key
  module Value: S
    with type t = value
     and type commit = key
     and type origin = origin
     and type node = Node.key

end

module type MAKER =
  functor (K: Ir_uid.S) ->
  functor (N: Ir_node.STORE) ->
    STORE with type key = K.t
           and type origin = N.origin
           and type node = N.value
           and module Node = N

module Make (C: Ir_ao.MAKER) (K: Ir_uid.S) (N: Ir_node.STORE) = struct

  module O = N.Contents.Origin
  module KN = N.Key

  module Value = struct

    type t = {
      node   : KN.t option;
      parents: K.t list;
      origin : O.t;
    } with compare

    type origin = O.t
    type node = KN.t
    type commit = K.t

    let parents t = t.parents
    let node t = t.node
    let origin t = t.origin
    let create origin ?node ~parents = { node; parents; origin }

    let hash = Hashtbl.hash
    let equal x y =
      (match x.node, y.node with
        | None, None -> true
        | Some x, Some y -> KN.equal x y
        | _ -> false)
      && (try List.for_all2 K.equal x.parents y.parents
          with Invalid_argument _ -> false)
      && O.equal x.origin y.origin

    let to_sexp t =
      let open Sexplib.Type in
      let open Sexplib.Conv in
      List [
        List [ Atom "node"   ; sexp_of_option KN.to_sexp t.node ];
        List [ Atom "parents"; sexp_of_list K.to_sexp t.parents ];
        List [ Atom "origin" ; O.to_sexp t.origin ]
      ]

    let to_json t =
      `O [
        ("node"   , Ezjsonm.option KN.to_json t.node);
        ("parents", Ezjsonm.list K.to_json t.parents);
        ("origin" , O.to_json t.origin);
      ]

    let of_json j =
      let node =
        try Some (Ezjsonm.find j ["node"] |> KN.of_json)
        with Not_found -> None
      in
      let parents = Ezjsonm.(find j ["parents"] |> get_list K.of_json) in
      let origin = Ezjsonm.find j ["origin"] |> O.of_json in
      { node; parents; origin }

    let edges t =
      begin match t.node with
        | None   -> []
        | Some k -> [`Node k]
      end
      @ List.map (fun k -> `Commit k) t.parents

    let write t =
      let open Bin_prot.Write in
      let bin_write_k = Tc.Writer.to_bin_prot K.write in
      let bin_write_kn = Tc.Writer.to_bin_prot KN.write in
      let bin_write_o = Tc.Writer.to_bin_prot O.write in
      Tc.Writer.of_bin_prot (
        bin_write_triple
          (bin_write_option bin_write_kn) (bin_write_list bin_write_k) bin_write_o
      ) (t.node, t.parents, t.origin)

    let read buf =
      let open Bin_prot.Read in
      let bin_read_k = Tc.Reader.to_bin_prot K.read in
      let bin_read_kn = Tc.Reader.to_bin_prot KN.read in
      let bin_read_o = Tc.Reader.to_bin_prot O.read in
      let node, parents, origin =
        Tc.Reader.of_bin_prot (
          bin_read_triple
            (bin_read_option bin_read_kn) (bin_read_list bin_read_k) bin_read_o
        ) buf
      in
      { node; parents; origin }

    let size_of t =
      let open Bin_prot.Size in
      bin_size_triple
        (bin_size_option KN.size_of) (bin_size_list K.size_of) O.size_of
        (t.node, t.parents, t.origin)

    let merge _ ~old:_ _ _ = conflict "commit"

  end

  module C = C(K)(Value)(O)

  type key = K.t
  type origin = N.origin
  type value = Value.t
  type t = N.t * C.t
  type node = N.value

  let node_t (t:t) = fst t

  module Key = K

  let create () =
    N.create () >>= fun n ->
    C.create () >>= fun c ->
    return (n, c)

  let add (_, t) origin c =
    C.add t origin c

  let read (_, t) origin c =
    C.read t origin c

  let read_exn (_, t) origin c =
    C.read_exn t origin c

  let mem (_, t) origin c =
    C.mem t origin c

  let node (n, _) origin c =
    match c.Value.node with
    | None   -> None
    | Some k -> Some (N.read_exn n origin k)

  let commit (n, c) origin ?node ~parents =
    begin match node with
      | None      -> return_none
      | Some node -> N.add n origin node >>= fun k -> return (Some k)
    end
    >>= fun node ->
    Lwt_list.map_p (C.add c origin) parents
    >>= fun parents ->
    let commit = { Value.node; parents; origin } in
    C.add c origin commit >>= fun key ->
    return (key, commit)

  let parents t origin c =
    List.map (read_exn t origin) c.Value.parents

  module Graph = Ir_graph.Make(N.Contents.Key)(KN)(K)(Ir_tag.String)

  let list t origin ?depth keys =
    Log.debugf "list %a" force (shows (module K) keys);
    let pred = function
      | `Commit k -> read_exn t origin k >>= fun r -> return (Value.edges r)
      | _         -> return_nil in
    let max = List.map (fun k -> `Commit k) keys in
    Graph.closure ?depth max ~pred >>= fun g ->
    let keys =
      Ir_misc.list_filter_map
        (function `Commit k -> Some k | _ -> None)
        (Graph.vertex g)
    in
    return keys

  let dump (_, t) =
    C.dump t

  let merge_node n =
    Ir_merge.some (N.merge n)

  let merge (n, _ as t) origin ~old k1 k2 =
    read_exn t origin old >>= fun vold ->
    read_exn t origin k1  >>= fun v1   ->
    read_exn t origin k2  >>= fun v2   ->
    merge_node n origin ~old:vold.Value.node v1.Value.node v2.Value.node >>|
    fun node ->
    let parents = [k1; k2] in
    let commit = { Value.node; parents; origin } in
    add t origin commit >>= fun key ->
    ok key

  module KSet = Ir_misc.Set(K)

  let find_common_ancestor t origin c1 c2 =
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
              read_exn t origin c >>= fun v ->
              let parents = KSet.of_list v.Value.parents in
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

  let find_common_ancestor_exn t origin c1 c2 =
    find_common_ancestor t origin c1 c2 >>= function
    | Some r -> return r
    | None   -> fail Not_found

  module Node = N

end

module Rec (S: STORE) = struct

  include S.Key

  type origin = S.origin

  let merge origin ~old k1 k2 =
    S.create () >>= fun t  ->
    S.merge t origin ~old k1 k2

end
