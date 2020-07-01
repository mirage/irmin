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

module No_metadata = struct
  type t = unit

  let t = Type.unit

  let default = ()

  let merge = Merge.v t (fun ~old:_ () () -> Merge.ok ())
end

module Make
    (K : Type.S) (P : sig
      type step

      val step_t : step Type.t
    end)
    (M : S.METADATA) =
struct
  type hash = K.t

  type step = P.step

  type metadata = M.t

  type kind = [ `Node | `Contents of M.t ]

  type entry = { kind : kind; name : P.step; node : K.t }

  let kind_t =
    let open Type in
    variant "Tree.kind" (fun node contents contents_m ->
      function
      | `Node -> node
      | `Contents m ->
          if Type.equal M.t m M.default then contents else contents_m m)
    |~ case0 "node" `Node
    |~ case0 "contents" (`Contents M.default)
    |~ case1 "contents" M.t (fun m -> `Contents m)
    |> sealv

  let entry_t : entry Type.t =
    let open Type in
    record "Tree.entry" (fun kind name node -> { kind; name; node })
    |+ field "kind" kind_t (function { kind; _ } -> kind)
    |+ field "name" P.step_t (fun { name; _ } -> name)
    |+ field "node" K.t (fun { node; _ } -> node)
    |> sealr

  let to_entry (k, v) =
    match v with
    | `Node h -> { name = k; kind = `Node; node = h }
    | `Contents (h, m) -> { name = k; kind = `Contents m; node = h }

  let of_entry n =
    ( n.name,
      match n.kind with
      | `Node -> `Node n.node
      | `Contents m -> `Contents (n.node, m) )

  module StepMap = Map.Make (struct
    type t = P.step

    let compare (x : t) (y : t) = Type.compare P.step_t y x
  end)

  type value = [ `Contents of hash * metadata | `Node of hash ]

  type t = entry StepMap.t

  let v l =
    List.fold_left
      (fun acc x -> StepMap.add (fst x) (to_entry x) acc)
      StepMap.empty l

  let list t = List.rev_map (fun (_, e) -> of_entry e) (StepMap.bindings t)

  let find t s =
    try
      let _, v = of_entry (StepMap.find s t) in
      Some v
    with Not_found -> None

  let empty = StepMap.empty

  let is_empty e = list e = []

  let add t k v =
    let e = to_entry (k, v) in
    StepMap.update k
      (fun e' ->
        if Type.equal (Type.option entry_t) (Some e) e' then e' else Some e)
      t

  let remove t k = StepMap.remove k t

  let step_t = P.step_t

  let hash_t = K.t

  let metadata_t = M.t

  let default = M.default

  let value_t =
    let open Type in
    variant "value" (fun n c x ->
      function
      | `Node h -> n h
      | `Contents (h, m) -> if Type.equal M.t m M.default then c h else x (h, m))
    |~ case1 "node" K.t (fun k -> `Node k)
    |~ case1 "contents" K.t (fun h -> `Contents (h, M.default))
    |~ case1 "contents-x" (pair K.t M.t) (fun (h, m) -> `Contents (h, m))
    |> sealv

  let of_entries e = v (List.rev_map of_entry e)

  let entries e = List.rev_map (fun (_, e) -> e) (StepMap.bindings e)

  let t = Type.map Type.(list entry_t) of_entries entries
end

module Store
    (C : S.CONTENTS_STORE)
    (P : S.PATH)
    (M : S.METADATA) (S : sig
      include S.CONTENT_ADDRESSABLE_STORE with type key = C.key

      module Key : S.HASH with type t = key

      module Val :
        S.NODE
          with type t = value
           and type hash = key
           and type metadata = M.t
           and type step = P.step
    end) =
struct
  module Contents = C
  module Key = Hash.Typed (S.Key) (S.Val)
  module Path = P
  module Metadata = M

  type 'a t = 'a C.t * 'a S.t

  type key = S.key

  type value = S.value

  let mem (_, t) = S.mem t

  let find (_, t) = S.find t

  let clear (_, t) = S.clear t

  let add (_, t) = S.add t

  let unsafe_add (_, t) = S.unsafe_add t

  let all_contents t =
    let kvs = S.Val.list t in
    List.fold_left
      (fun acc -> function k, `Contents c -> (k, c) :: acc | _ -> acc)
      [] kvs

  let all_succ t =
    let kvs = S.Val.list t in
    List.fold_left
      (fun acc -> function k, `Node n -> (k, n) :: acc | _ -> acc)
      [] kvs

  let contents_t = C.Key.t

  let metadata_t = M.t

  let step_t = Path.step_t

  (* [Merge.alist] expects us to return an option. [C.merge] does
     that, but we need to consider the metadata too... *)
  let merge_contents_meta c =
    (* This gets us [C.t option, S.Val.Metadata.t]. We want [(C.t *
       S.Val.Metadata.t) option]. *)
    let explode = function
      | None -> (None, M.default)
      | Some (c, m) -> (Some c, m)
    in
    let implode = function None, _ -> None | Some c, m -> Some (c, m) in
    Merge.like
      Type.(option (pair contents_t metadata_t))
      (Merge.pair (C.merge c) M.merge)
      explode implode

  let merge_contents_meta c =
    Merge.alist step_t
      Type.(pair contents_t metadata_t)
      (fun _step -> merge_contents_meta c)

  let merge_parents merge_key =
    Merge.alist step_t S.Key.t (fun _step -> merge_key)

  let merge_value (c, _) merge_key =
    let explode t = (all_contents t, all_succ t) in
    let implode (contents, succ) =
      let xs = List.rev_map (fun (s, c) -> (s, `Contents c)) contents in
      let ys = List.rev_map (fun (s, n) -> (s, `Node n)) succ in
      S.Val.v (xs @ ys)
    in
    let merge = Merge.pair (merge_contents_meta c) (merge_parents merge_key) in
    Merge.like S.Val.t merge explode implode

  let rec merge t =
    let merge_key =
      Merge.v (Type.option S.Key.t) (fun ~old x y ->
          Merge.(f (merge t)) ~old x y)
    in
    let merge = merge_value t merge_key in
    let read = function
      | None -> Lwt.return S.Val.empty
      | Some k -> ( find t k >|= function None -> S.Val.empty | Some v -> v )
    in
    let add v =
      if S.Val.is_empty v then Lwt.return_none
      else add t v >>= fun k -> Lwt.return_some k
    in
    Merge.like_lwt Type.(option S.Key.t) merge read add

  module Val = S.Val
end

module Graph (S : S.NODE_STORE) = struct
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
    S.find t n >|= function None -> [] | Some n -> S.Val.list n

  module U = struct
    type t = unit

    let t = Type.unit
  end

  module Graph = Object_graph.Make (Contents) (Metadata) (S.Key) (U) (U)

  let edges t =
    List.rev_map
      (function _, `Node n -> `Node n | _, `Contents c -> `Contents c)
      (S.Val.list t)

  let pp_key = Type.pp S.Key.t

  let pp_keys = Fmt.(Dump.list pp_key)

  let pp_path = Type.pp S.Path.t

  let pred t = function
    | `Node k -> ( S.find t k >|= function None -> [] | Some v -> edges v )
    | _ -> Lwt.return_nil

  let closure t ~min ~max =
    Log.debug (fun f -> f "closure min=%a max=%a" pp_keys min pp_keys max);
    let min = List.rev_map (fun x -> `Node x) min in
    let max = List.rev_map (fun x -> `Node x) max in
    Graph.closure ~pred:(pred t) ~min ~max () >|= fun g ->
    List.fold_left
      (fun acc -> function `Node x -> x :: acc | _ -> acc)
      [] (Graph.vertex g)

  let ignore_lwt _ = Lwt.return_unit

  let iter t ~min ~max ?(node = ignore_lwt) ?(edge = fun _ -> ignore_lwt)
      ?(skip = fun _ -> Lwt.return_false) ?(rev = true) () =
    Log.debug (fun f ->
        f "iter on closure min=%a max=%a" pp_keys min pp_keys max);
    let min = List.rev_map (fun x -> `Node x) min in
    let max = List.rev_map (fun x -> `Node x) max in
    let node = function `Node x -> node x | _ -> Lwt.return_unit in
    let edge n pred =
      match (n, pred) with
      | `Node src, `Node dst -> edge src dst
      | _ -> Lwt.return_unit
    in
    let skip = function `Node x -> skip x | _ -> Lwt.return_false in
    Graph.iter ~pred:(pred t) ~min ~max ~node ~edge ~skip ~rev ()

  let v t xs = S.add t (S.Val.v xs)

  let find_step t node step =
    Log.debug (fun f -> f "contents %a" pp_key node);
    S.find t node >|= function None -> None | Some n -> S.Val.find n step

  let find t node path =
    Log.debug (fun f -> f "read_node_exn %a %a" pp_key node pp_path path);
    let rec aux node path =
      match Path.decons path with
      | None -> Lwt.return_some (`Node node)
      | Some (h, tl) -> (
          find_step t node h >>= function
          | (None | Some (`Contents _)) as x -> Lwt.return x
          | Some (`Node node) -> aux node tl )
    in
    aux node path

  let err_empty_path () = invalid_arg "Irmin.node: empty path"

  let map_one t node f label =
    Log.debug (fun f -> f "map_one %a" Type.(pp Path.step_t) label);
    let old_key = S.Val.find node label in
    ( match old_key with
    | None | Some (`Contents _) -> Lwt.return S.Val.empty
    | Some (`Node k) -> (
        S.find t k >|= function None -> S.Val.empty | Some v -> v ) )
    >>= fun old_node ->
    f old_node >>= fun new_node ->
    if Type.equal S.Val.t old_node new_node then Lwt.return node
    else if S.Val.is_empty new_node then
      let node = S.Val.remove node label in
      if S.Val.is_empty node then Lwt.return S.Val.empty else Lwt.return node
    else S.add t new_node >|= fun k -> S.Val.add node label (`Node k)

  let map t node path f =
    Log.debug (fun f -> f "map %a %a" pp_key node pp_path path);
    let rec aux node path =
      match Path.decons path with
      | None -> Lwt.return (f node)
      | Some (h, tl) -> map_one t node (fun node -> aux node tl) h
    in
    (S.find t node >|= function None -> S.Val.empty | Some n -> n)
    >>= fun node -> aux node path >>= S.add t

  let add t node path n =
    Log.debug (fun f -> f "add %a %a" pp_key node pp_path path);
    match Path.rdecons path with
    | Some (path, file) -> map t node path (fun node -> S.Val.add node file n)
    | None -> (
        match n with
        | `Node n -> Lwt.return n
        | `Contents _ -> failwith "TODO: Node.add" )

  let rdecons_exn path =
    match Path.rdecons path with
    | Some (l, t) -> (l, t)
    | None -> err_empty_path ()

  let remove t node path =
    let path, file = rdecons_exn path in
    map t node path (fun node -> S.Val.remove node file)

  let path_t = Path.t

  let node_t = S.Key.t

  let metadata_t = Metadata.t

  let step_t = Path.step_t

  let contents_t = Contents.t

  let value_t = S.Val.value_t
end

module V1 (N : S.NODE) = struct
  module K = struct
    let h = Type.string_of `Int64

    let size_of ?headers x =
      Type.size_of ?headers h (Type.to_bin_string N.hash_t x)

    let encode_bin ?headers e k =
      Type.encode_bin ?headers h (Type.to_bin_string N.hash_t e) k

    let decode_bin ?headers buf off =
      let n, v = Type.decode_bin ?headers h buf off in
      ( n,
        match Type.of_bin_string N.hash_t v with
        | Ok v -> v
        | Error (`Msg e) -> Fmt.failwith "decode_bin: %s" e )

    let t = Type.like N.hash_t ~bin:(encode_bin, decode_bin, size_of)
  end

  type step = N.step

  type hash = N.hash

  type metadata = N.metadata

  type value = N.value

  let hash_t = N.hash_t

  let metadata_t = N.metadata_t

  type t = { n : N.t; entries : (step * value) list }

  let import n = { n; entries = N.list n }

  let export t = t.n

  let v entries =
    let n = N.v entries in
    { n; entries }

  let list t = t.entries

  let empty = { n = N.empty; entries = [] }

  let is_empty t = t.entries = []

  let default = N.default

  let find t k = N.find t.n k

  let add t k v =
    let n = N.add t.n k v in
    if t.n == n then t else { n; entries = N.list n }

  let remove t k =
    let n = N.remove t.n k in
    if t.n == n then t else { n; entries = N.list n }

  let step_t : step Type.t =
    let to_string p = Type.to_bin_string N.step_t p in
    let of_string s =
      Type.of_bin_string N.step_t s |> function
      | Ok x -> x
      | Error (`Msg e) -> Fmt.failwith "Step.of_string: %s" e
    in
    Type.(map (string_of `Int64)) of_string to_string

  let value_t =
    let open Type in
    record "node" (fun contents metadata node ->
        match (contents, metadata, node) with
        | Some c, None, None -> `Contents (c, N.default)
        | Some c, Some m, None -> `Contents (c, m)
        | None, None, Some n -> `Node n
        | _ -> failwith "invalid node")
    |+ field "contents" (option K.t) (function
         | `Contents (x, _) -> Some x
         | _ -> None)
    |+ field "metadata" (option N.metadata_t) (function
         | `Contents (_, x) when not (equal N.metadata_t N.default x) -> Some x
         | _ -> None)
    |+ field "node" (option K.t) (function `Node n -> Some n | _ -> None)
    |> sealr

  let t : t Type.t =
    Type.map Type.(list ~len:`Int64 (pair step_t value_t)) v list
end
