(*
 * Copyright (c) 2013      Louis Gesbert     <louis.gesbert@ocamlpro.com>
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
include Node_intf

let src = Logs.Src.create "irmin.node" ~doc:"Irmin trees/nodes"

module Log = (val Logs.src_log src : Logs.LOG)

module Make
    (H : Type.S) (P : sig
      type step [@@deriving irmin]
    end)
    (M : Metadata.S) =
struct
  type hash = H.t [@@deriving irmin]
  type step = P.step [@@deriving irmin]
  type metadata = M.t [@@deriving irmin]
  type kind = [ `Node | `Contents of M.t ]

  let equal_metadata = Type.(unstage (equal M.t))

  let kind_t =
    let open Type in
    variant "Tree.kind" (fun node contents contents_m -> function
      | `Node -> node
      | `Contents m ->
          if equal_metadata m M.default then contents else contents_m m)
    |~ case0 "node" `Node
    |~ case0 "contents" (`Contents M.default)
    |~ case1 "contents" M.t (fun m -> `Contents m)
    |> sealv

  type entry = { kind : kind; name : P.step; node : H.t } [@@deriving irmin]

  let equal_entry_opt = Type.(unstage (equal [%typ: entry option]))

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

    let compare = Type.(unstage (compare P.step_t))
  end)

  type value = [ `Contents of hash * metadata | `Node of hash ]
  type t = entry StepMap.t

  let of_seq l =
    Seq.fold_left
      (fun acc x -> StepMap.add (fst x) (to_entry x) acc)
      StepMap.empty l

  let of_list l = of_seq (List.to_seq l)

  let seq ?(offset = 0) ?length ?cache:_ (t : t) =
    let take seq = match length with None -> seq | Some n -> Seq.take n seq in
    StepMap.to_seq t
    |> Seq.drop offset
    |> take
    |> Seq.map (fun (_, e) -> of_entry e)

  let list ?offset ?length ?cache:_ t = List.of_seq (seq ?offset ?length t)

  let find ?cache:_ t s =
    try
      let _, v = of_entry (StepMap.find s t) in
      Some v
    with Not_found -> None

  let empty = StepMap.empty
  let is_empty e = StepMap.is_empty e
  let length e = StepMap.cardinal e
  let clear _ = ()

  let add t k v =
    let e = to_entry (k, v) in
    StepMap.update k
      (fun e' -> if equal_entry_opt (Some e) e' then e' else Some e)
      t

  let remove t k = StepMap.remove k t
  let default = M.default

  let value_t =
    let open Type in
    variant "value" (fun n c x -> function
      | `Node h -> n h
      | `Contents (h, m) -> if equal_metadata m M.default then c h else x (h, m))
    |~ case1 "node" H.t (fun k -> `Node k)
    |~ case1 "contents" H.t (fun h -> `Contents (h, M.default))
    |~ case1 "contents-x" (pair H.t M.t) (fun (h, m) -> `Contents (h, m))
    |> sealv

  let of_entries e = of_list (List.rev_map of_entry e)
  let entries e = List.rev_map (fun (_, e) -> e) (StepMap.bindings e)
  let t = Type.map Type.(list entry_t) of_entries entries

  (** Merges *)

  let all_contents t =
    let kvs = list t in
    List.fold_left
      (fun acc -> function k, `Contents c -> (k, c) :: acc | _ -> acc)
      [] kvs

  let all_succ t =
    let kvs = list t in
    List.fold_left
      (fun acc -> function k, `Node n -> (k, n) :: acc | _ -> acc)
      [] kvs

  let metadata_t = M.t
  let step_t = P.step_t

  (* [Merge.alist] expects us to return an option. [C.merge] does
     that, but we need to consider the metadata too... *)
  let merge_metadata merge_contents =
    (* This gets us [C.t option, S.Val.Metadata.t]. We want [(C.t *
       S.Val.Metadata.t) option]. *)
    let explode = function
      | None -> (None, M.default)
      | Some (c, m) -> (Some c, m)
    in
    let implode = function None, _ -> None | Some c, m -> Some (c, m) in
    Merge.like [%typ: (hash * metadata) option]
      (Merge.pair merge_contents M.merge)
      explode implode

  let merge_contents merge_hash =
    Merge.alist step_t [%typ: hash * metadata] (fun _step ->
        merge_metadata merge_hash)

  let merge_node merge_hash = Merge.alist step_t H.t (fun _step -> merge_hash)

  let merge ~contents ~node =
    let explode t = (all_contents t, all_succ t) in
    let implode (contents, succ) =
      let xs = List.rev_map (fun (s, c) -> (s, `Contents c)) contents in
      let ys = List.rev_map (fun (s, n) -> (s, `Node n)) succ in
      of_list (xs @ ys)
    in
    let merge = Merge.pair (merge_contents contents) (merge_node node) in
    Merge.like t merge explode implode
end

module Store
    (C : Contents.Store)
    (S : Content_addressable.S with type key = C.key)
    (K : Hash.S with type t = S.key)
    (V : S with type t = S.value and type hash = S.key)
    (M : Metadata.S with type t = V.metadata)
    (P : Path.S with type step = V.step) =
struct
  module Contents = C
  module Val = V
  module Key = Hash.Typed (K) (Val)
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
  let batch (c, s) f = C.batch c (fun n -> S.batch s (fun s -> f (n, s)))

  let close (c, s) =
    let* () = C.close c in
    let+ () = S.close s in
    ()

  let rec merge t =
    let merge_key =
      Merge.v [%typ: Key.t option] (fun ~old x y ->
          Merge.(f (merge t)) ~old x y)
    in
    let merge = Val.merge ~contents:C.(merge (fst t)) ~node:merge_key in
    let read = function
      | None -> Lwt.return Val.empty
      | Some k -> ( find t k >|= function None -> Val.empty | Some v -> v)
    in
    let add v =
      if Val.is_empty v then Lwt.return_none else add t v >>= Lwt.return_some
    in
    Merge.like_lwt [%typ: Key.t option] merge read add
end

module Graph (S : Store) = struct
  module Path = S.Path
  module Contents = S.Contents.Key
  module Metadata = S.Metadata

  type step = Path.step [@@deriving irmin]
  type metadata = Metadata.t [@@deriving irmin]
  type contents = Contents.t [@@deriving irmin]
  type node = S.Key.t [@@deriving irmin]
  type path = Path.t [@@deriving irmin]
  type 'a t = 'a S.t
  type value = [ `Contents of contents * metadata | `Node of node ]

  let empty t = S.add t S.Val.empty

  let list t n =
    Log.debug (fun f -> f "steps");
    S.find t n >|= function None -> [] | Some n -> S.Val.list n

  module U = struct
    type t = unit [@@deriving irmin]
  end

  module Graph = Object_graph.Make (S.Key) (U)

  let edges t =
    List.rev_map
      (function _, `Node n -> `Node n | _, `Contents (c, _) -> `Contents c)
      (S.Val.list t)

  let pp_key = Type.pp S.Key.t
  let pp_keys = Fmt.(Dump.list pp_key)
  let pp_path = Type.pp S.Path.t
  let equal_val = Type.(unstage (equal S.Val.t))

  let pred t = function
    | `Node k -> ( S.find t k >|= function None -> [] | Some v -> edges v)
    | _ -> Lwt.return_nil

  let closure t ~min ~max =
    Log.debug (fun f -> f "closure min=%a max=%a" pp_keys min pp_keys max);
    let min = List.rev_map (fun x -> `Node x) min in
    let max = List.rev_map (fun x -> `Node x) max in
    let+ g = Graph.closure ~pred:(pred t) ~min ~max () in
    List.fold_left
      (fun acc -> function `Node x -> x :: acc | _ -> acc)
      [] (Graph.vertex g)

  let ignore_lwt _ = Lwt.return_unit

  let iter t ~min ~max ?(node = ignore_lwt) ?(contents = ignore_lwt) ?edge
      ?(skip_node = fun _ -> Lwt.return_false)
      ?(skip_contents = fun _ -> Lwt.return_false) ?(rev = true) () =
    let min = List.rev_map (fun x -> `Node x) min in
    let max = List.rev_map (fun x -> `Node x) max in
    let node = function
      | `Node x -> node x
      | `Contents c -> contents c
      | `Branch _ | `Commit _ -> Lwt.return_unit
    in
    let edge =
      Option.map
        (fun edge n pred ->
          match (n, pred) with
          | `Node src, `Node dst -> edge src dst
          | _ -> Lwt.return_unit)
        edge
    in
    let skip = function
      | `Node x -> skip_node x
      | `Contents c -> skip_contents c
      | _ -> Lwt.return_false
    in
    Graph.iter ~pred:(pred t) ~min ~max ~node ?edge ~skip ~rev ()

  let v t xs = S.add t (S.Val.of_list xs)

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
          | Some (`Node node) -> aux node tl)
    in
    aux node path

  let err_empty_path () = invalid_arg "Irmin.node: empty path"

  let map_one t node f label =
    Log.debug (fun f -> f "map_one %a" Type.(pp Path.step_t) label);
    let old_key = S.Val.find node label in
    let* old_node =
      match old_key with
      | None | Some (`Contents _) -> Lwt.return S.Val.empty
      | Some (`Node k) -> (
          S.find t k >|= function None -> S.Val.empty | Some v -> v)
    in
    let* new_node = f old_node in
    if equal_val old_node new_node then Lwt.return node
    else if S.Val.is_empty new_node then
      let node = S.Val.remove node label in
      if S.Val.is_empty node then Lwt.return S.Val.empty else Lwt.return node
    else
      let+ k = S.add t new_node in
      S.Val.add node label (`Node k)

  let map t node path f =
    Log.debug (fun f -> f "map %a %a" pp_key node pp_path path);
    let rec aux node path =
      match Path.decons path with
      | None -> Lwt.return (f node)
      | Some (h, tl) -> map_one t node (fun node -> aux node tl) h
    in
    let* node =
      S.find t node >|= function None -> S.Val.empty | Some n -> n
    in
    aux node path >>= S.add t

  let add t node path n =
    Log.debug (fun f -> f "add %a %a" pp_key node pp_path path);
    match Path.rdecons path with
    | Some (path, file) -> map t node path (fun node -> S.Val.add node file n)
    | None -> (
        match n with
        | `Node n -> Lwt.return n
        | `Contents _ -> failwith "TODO: Node.add")

  let rdecons_exn path =
    match Path.rdecons path with
    | Some (l, t) -> (l, t)
    | None -> err_empty_path ()

  let remove t node path =
    let path, file = rdecons_exn path in
    map t node path (fun node -> S.Val.remove node file)

  let value_t = S.Val.value_t
end

module V1 (N : S with type step = string) = struct
  module K = struct
    let h = Type.string_of `Int64
    let to_bin_string = Type.(unstage (to_bin_string N.hash_t))
    let of_bin_string = Type.(unstage (of_bin_string N.hash_t))
    let size_of = Type.Size.using to_bin_string (Type.Size.t h)

    let encode_bin =
      let encode_bin = Type.(unstage (encode_bin h)) in
      Type.stage @@ fun e k -> encode_bin (to_bin_string e) k

    let decode_bin =
      let decode_bin = Type.(unstage (decode_bin h)) in
      Type.stage @@ fun buf off ->
      let n, v = decode_bin buf off in
      ( n,
        match of_bin_string v with
        | Ok v -> v
        | Error (`Msg e) -> Fmt.failwith "decode_bin: %s" e )

    let t = Type.like N.hash_t ~bin:(encode_bin, decode_bin, size_of)
  end

  type step = N.step
  type hash = N.hash [@@deriving irmin]
  type metadata = N.metadata [@@deriving irmin]
  type value = N.value
  type t = { n : N.t; entries : (step * value) list }

  let import n = { n; entries = N.list n }
  let export t = t.n

  let of_seq entries =
    let n = N.of_seq entries in
    let entries = List.of_seq entries in
    { n; entries }

  let of_list entries =
    let n = N.of_list entries in
    { n; entries }

  let seq ?(offset = 0) ?length ?cache:_ t =
    let take seq = match length with None -> seq | Some n -> Seq.take n seq in
    List.to_seq t.entries |> Seq.drop offset |> take

  let list ?offset ?length ?cache t = List.of_seq (seq ?offset ?length ?cache t)
  let empty = { n = N.empty; entries = [] }
  let is_empty t = t.entries = []
  let length e = N.length e.n
  let clear _ = ()
  let default = N.default
  let find ?cache t k = N.find ?cache t.n k

  let add t k v =
    let n = N.add t.n k v in
    if t.n == n then t else { n; entries = N.list n }

  let remove t k =
    let n = N.remove t.n k in
    if t.n == n then t else { n; entries = N.list n }

  let v1_step = Type.string_of `Int64
  let step_to_bin_string = Type.(unstage (to_bin_string v1_step))
  let step_of_bin_string = Type.(unstage (of_bin_string v1_step))

  let step_t : step Type.t =
    let to_string p = step_to_bin_string p in
    let of_string s =
      step_of_bin_string s |> function
      | Ok x -> x
      | Error (`Msg e) -> Fmt.failwith "Step.of_string: %s" e
    in
    Type.(map (string_of `Int64)) of_string to_string

  let is_default = Type.(unstage (equal N.metadata_t)) N.default

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
         | `Contents (_, x) when not (is_default x) -> Some x
         | _ -> None)
    |+ field "node" (option K.t) (function `Node n -> Some n | _ -> None)
    |> sealr

  let t : t Type.t =
    Type.map Type.(list ~len:`Int64 (pair step_t value_t)) of_list list

  let merge ~contents ~node =
    let merge = N.merge ~contents ~node in
    let f ~old x y =
      let old = Merge.map_promise (fun old -> old.n) old in
      let+ r = Merge.f merge ~old x.n y.n in
      match r with Ok r -> Ok (import r) | Error e -> Error e
    in
    Merge.v t f
end
