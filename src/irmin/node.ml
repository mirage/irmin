(*
 * Copyright (c) 2013      Louis Gesbert     <louis.gesbert@ocamlpro.com>
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

let src = Logs.Src.create "irmin.node" ~doc:"Irmin trees/nodes"
module Log = (val Logs.src_log src : Logs.LOG)

let node ~default c m n =
  let open Type in
  record "node" (fun contents metadata node ->
      match contents, metadata, node with
      | Some c, None  , None   -> `Contents (c, default)
      | Some c, Some m, None   -> `Contents (c, m)
      | None  , None  , Some n -> `Node n
      | _ -> failwith "invalid node")
  |+ field "contents" (option c) (function
      | `Contents (x, _) -> Some x
      | _ -> None)
  |+ field "metadata" (option m) (function
      | `Contents (_, x) when not (equal m default x) -> Some x
      | _ -> None)
  |+ field "node" (option n) (function
      | `Node n -> Some n
      | _ -> None)
  |> sealr

module No_metadata = struct
  type t = unit
  let t = Type.unit
  let default = ()
  let merge = Merge.v t (fun ~old:_ () () -> Merge.ok ())
end

module Make (K: Type.S) (P: S.PATH) (M: S.METADATA) =
struct

  type hash = K.t
  type step = P.step
  type metadata = M.t

  module StepMap =
    Map.Make(struct type t = P.step let compare = Type.compare P.step_t end)

  type value = [ `Contents of hash * metadata | `Node of hash ]

  type k =
    | Map  : value StepMap.t -> k
    | List: (step * value) list -> k
    | Both: value StepMap.t * (step * value) list -> k

  type t = { mutable k: k }
  let of_list l = { k = List l }
  let of_map m = { k = Map m }

  let list t = match t.k with
    | List l | Both (_, l) -> l
    | Map m ->
      let alist = StepMap.bindings m in
      t.k <- Both (m, alist);
      alist

  let map t = match t.k with
    | Map m | Both (m, _) -> m
    | List l ->
      let map =
        List.fold_left (fun acc (l, x) -> StepMap.add l x acc) StepMap.empty l
      in
      t.k <- Both (map, l);
      map

  let v = of_list

  let find t s =
    try Some (StepMap.find s (map t))
    with Not_found -> None

  let empty = of_list []
  let is_empty e = list e = []

  let update t k v =
    let map = map t in
    let map' = StepMap.add k v map in
    if map == map' then t else of_map map'

  let remove t k =
    let map = map t in
    let map' = StepMap.remove k map in
    if map == map' then t else of_map map'

  let value_t = node ~default:M.default K.t M.t K.t
  let step_t = P.step_t
  let hash_t = K.t
  let metadata_t = M.t
  let t = Type.like Type.(list (pair P.step_t value_t)) of_list list

end

module Store
    (C: S.CONTENTS_STORE)
    (P: S.PATH)
    (M: S.METADATA)
    (S: sig
       include S.CONTENT_ADDRESSABLE_STORE with type key = C.key
       module Key: S.HASH with type t = key
       module Val: S.NODE with type t = value
                           and type hash = key
                           and type metadata = M.t
                           and type step = P.step
     end) =
struct

  module Contents = C
  module Key = S.Key
  module Path = P
  module Metadata = M

  type 'a t = 'a C.t * 'a S.t
  type key = S.key
  type value = S.value

  let mem (_, t) = S.mem t
  let find (_, t) = S.find t
  let add (_, t) = S.add t

  let all_contents t =
    let kvs = S.Val.list t in
    List.fold_left (fun acc -> function
        | k, `Contents c -> (k, c) :: acc
        | _ -> acc
      ) [] kvs

  let all_succ t =
    let kvs = S.Val.list t in
    List.fold_left (fun acc -> function
        | k, `Node n -> (k, n) :: acc
        | _ -> acc
      ) [] kvs

  let contents_t = C.Key.t
  let metadata_t = M.t
  let step_t = Path.step_t

  (* [Merge.alist] expects us to return an option. [C.merge] does
     that, but we need to consider the metadata too... *)
  let merge_contents_meta c =
    (* This gets us [C.t option, S.Val.Metadata.t]. We want [(C.t *
       S.Val.Metadata.t) option]. *)
    let explode = function
      | None        -> None, M.default
      | Some (c, m) -> Some c, m
    in
    let implode = function
      | None  , _ -> None
      | Some c, m -> Some (c, m)
    in
    Merge.like Type.(option (pair contents_t metadata_t))
      (Merge.pair (C.merge c) M.merge)
      explode implode

  let merge_contents_meta c =
    Merge.alist step_t
      Type.(pair contents_t metadata_t)
      (fun _step -> merge_contents_meta c)

  let merge_parents merge_key =
    Merge.alist step_t S.Key.t (fun _step -> merge_key)

  let merge_value (c, _) merge_key =
    let explode t = all_contents t, all_succ t in
    let implode (contents, succ) =
      let xs = List.map (fun (s, c) -> s, `Contents c) contents in
      let ys = List.map (fun (s, n) -> s, `Node n) succ in
      S.Val.v (xs @ ys)
    in
    let merge =
      Merge.pair (merge_contents_meta c) (merge_parents merge_key)
    in
    Merge.like S.Val.t merge explode implode

  let rec merge t =
    let merge_key =
      Merge.v
        (Type.option S.Key.t)
        (fun ~old x y -> Merge.(f (merge t)) ~old x y)
    in
    let merge = merge_value t merge_key in
    let read = function
      | None   -> Lwt.return S.Val.empty
      | Some k -> find t k >|= function None -> S.Val.empty | Some v -> v
    in
    let add v =
      if S.Val.is_empty v then Lwt.return_none
      else add t v >>= fun k -> Lwt.return (Some k)
    in
    Merge.like_lwt Type.(option S.Key.t) merge read add

  module Val = S.Val

end

module Graph (S: S.NODE_STORE) = struct

  module Path = S.Path
  module Contents = S.Contents.Key
  module Metadata = S.Metadata

  type step = Path.step
  type metadata = Metadata.t
  type contents = Contents.t
  type node = S.key
  type path = Path.t
  type 'a t = 'a S.t

  type value = [ `Contents of contents * metadata | `Node of node ]

  let empty t = S.add t S.Val.empty

  let list t n =
    Log.debug (fun f -> f "steps");
    S.find t n >|= function
    | None   -> []
    | Some n -> S.Val.list n


  module U = struct type t = unit let t = Type.unit end
  module Graph = Object_graph.Make(Contents)(Metadata)(S.Key)(U)(U)

  let edges t =
    List.map (function
      | _, `Node n     -> `Node n
      | _, `Contents c -> `Contents c
    ) (S.Val.list t)

  let pp_key = Type.pp S.Key.t
  let pp_keys = Fmt.(Dump.list pp_key)
  let pp_path = Type.pp S.Path.t

  let closure t ~min ~max =
    Log.debug (fun f -> f "closure min=%a max=%a" pp_keys min pp_keys max);
    let pred = function
      | `Node k -> (S.find t k >|= function None -> [] | Some v -> edges v)
      | _       -> Lwt.return_nil
    in
    let min = List.map (fun x -> `Node x) min in
    let max = List.map (fun x -> `Node x) max in
    Graph.closure ~pred ~min ~max () >>= fun g ->
    let keys =
      List.fold_left (fun acc -> function
          | `Node x -> x :: acc
          | _ -> acc
        ) [] (Graph.vertex g)
    in
    Lwt.return keys

  let v t xs = S.add t (S.Val.v xs)

  let find_step t node step =
    Log.debug (fun f -> f "contents %a" pp_key node);
    S.find t node >|= function
    | None   -> None
    | Some n -> S.Val.find n step

  let find t node path =
    Log.debug (fun f -> f "read_node_exn %a %a" pp_key node pp_path path);
    let rec aux node path =
      match Path.decons path with
      | None         -> Lwt.return (Some (`Node node))
      | Some (h, tl) ->
        find_step t node h >>= function
        | None | Some (`Contents _) as x -> Lwt.return x
        | Some (`Node node)  -> aux node tl
    in
    aux node path

  let err_empty_path () = invalid_arg "Irmin.node: empty path"

  let map_one t node f label =
    Log.debug (fun f -> f "map_one %a" Type.(pp Path.step_t) label);
    let old_key = S.Val.find node label in
    begin match old_key with
      | None | Some (`Contents _) -> Lwt.return S.Val.empty
      | Some (`Node k) -> S.find t k >|= function
        | None   -> S.Val.empty
        | Some v -> v
    end >>= fun old_node ->
    f old_node >>= fun new_node ->
    if Type.equal S.Val.t old_node new_node then
      Lwt.return node
    else (
      if S.Val.is_empty new_node then (
        let node = S.Val.remove node label in
        if S.Val.is_empty node then Lwt.return S.Val.empty
        else Lwt.return node
      ) else
        S.add t new_node >>= fun k ->
        let node = S.Val.update node label (`Node k) in
        Lwt.return node
    )

  let map t node path f =
    Log.debug (fun f -> f "map %a %a" pp_key node pp_path path);
    let rec aux node path =
      match Path.decons path with
      | None         -> Lwt.return (f node)
      | Some (h, tl) -> map_one t node (fun node -> aux node tl) h
    in
    begin S.find t node >|= function
      | None   -> S.Val.empty
      | Some n -> n
    end >>= fun node ->
    aux node path >>=
    S.add t

  let update t node path n =
    Log.debug (fun f -> f "update %a %a" pp_key node pp_path path);
    match Path.rdecons path with
    | Some (path, file) ->
      map t node path (fun node -> S.Val.update node file n)
    | None ->
      match n with
      | `Node n     -> Lwt.return n
      | `Contents _ -> failwith "TODO: Node.update"

  let rdecons_exn path =
    match Path.rdecons path with
    | Some (l,t) -> l, t
    | None       -> err_empty_path ()

  let remove t node path =
    let path, file = rdecons_exn path in
    map t node path (fun node -> S.Val.remove node file)

  let path_t = Path.t
  let node_t = S.Key.t
  let value_t  = node ~default:Metadata.default Contents.t Metadata.t S.Key.t
  let metadata_t = Metadata.t
  let step_t = Path.step_t
  let contents_t = Contents.t

end
