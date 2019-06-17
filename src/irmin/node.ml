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

module type Conf = sig
  val max_inodes : int

  val max_values : int
end

module Tree
    (Conf : Conf)
    (K : S.HASH) (P : sig
        type step

        val step_t : step Type.t
    end)
    (M : S.METADATA) =
struct
  type hash = K.t

  type step = P.step

  module StepMap = struct
    include Map.Make (struct
      type t = step

      let compare = Type.compare P.step_t
    end)

    let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
  end

  type metadata = M.t

  type entry =
    | Node of { name : P.step; node : K.t }
    | Contents of { metadata : M.t; name : P.step; node : K.t }
    | Inode of { index : int; node : K.t }

  let entry_t : entry Type.t =
    let open Type in
    variant "Tree.entry" (fun node contents inode -> function
      | Node n -> node (n.name, n.node)
      | Contents c -> contents (c.metadata, c.name, c.node)
      | Inode i -> inode (i.index, i.node) )
    |~ case1 "Node" (pair P.step_t K.t) (fun (name, node) ->
           Node { name; node } )
    |~ case1 "Contents" (triple M.t P.step_t K.t)
         (fun (metadata, name, node) -> Contents { metadata; name; node })
    |~ case1 "Inode" (pair int K.t) (fun (index, node) -> Inode { index; node })
    |> sealv

  let entry_of_value name v =
    match v with
    | `Node node -> Node { name; node }
    | `Contents (node, metadata) -> Contents { metadata; name; node }

  let entries_of_values m =
    StepMap.fold (fun s v acc -> entry_of_value s v :: acc) m [] |> List.rev

  let entry_of_inode index node = Inode { index; node }

  type value = [ `Contents of hash * metadata | `Node of hash ]

  let value_t =
    let open Type in
    variant "value" (fun n c -> function
      | `Node h -> n h | `Contents (h, m) -> c (h, m) )
    |~ case1 "node" K.t (fun k -> `Node k)
    |~ case1 "contents" (pair K.t M.t) (fun (h, m) -> `Contents (h, m))
    |> sealv

  type t = Values of value StepMap.t | Nodes of t array | Empty

  type inode = entry list

  let inode_t : inode Type.t = Type.(list entry_t)

  let list (t : t) =
    let rec aux acc = function
      | Empty -> acc
      | Values t ->
          List.fold_left (fun acc e -> e :: acc) acc (StepMap.bindings t)
      | Nodes n -> Array.fold_left aux acc n
    in
    aux [] t

  let index ~seed k = abs (Type.hash P.step_t ~seed k) mod Conf.max_inodes

  let find (t : t) s =
    let rec aux seed = function
      | Empty -> None
      | Values t -> ( try Some (StepMap.find s t) with Not_found -> None )
      | Nodes n ->
          let i = index ~seed s in
          aux (seed + 1) n.(i)
    in
    aux 0 t

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let singleton s v = Values (StepMap.singleton s v)

  let rec add_values : type a. seed:_ -> _ -> _ -> _ -> _ -> (t -> a) -> a =
   fun ~seed t vs s v k ->
    let values = StepMap.add s v vs in
    if values == vs then k t
    else if StepMap.cardinal values <= Conf.max_values then k (Values values)
    else (nodes_of_values [@tailcall]) ~seed values (fun n -> k (Nodes n))

  and nodes_of_values : type a. seed:_ -> _ -> (t array -> a) -> a =
   fun ~seed entries k ->
    let n = Array.make Conf.max_inodes Empty in
    StepMap.iter (fun s e -> (update_nodes [@tailcall]) ~seed n s e) entries;
    k n

  and with_node : type a.
      seed:_ -> copy:_ -> _ -> _ -> _ -> (int -> t -> t -> a) -> a =
   fun ~seed ~copy n s v k ->
    let i = index ~seed s in
    let x = n.(i) in
    match x with
    | Empty -> k i x (singleton s v)
    | Values vs as t ->
        (add_values [@tailcall]) ~seed:(seed + 1) t vs s v (fun y -> k i x y)
    | Nodes n as t ->
        (add_nodes [@tailcall]) ~seed:(seed + 1) ~copy t n s v (fun y ->
            k i x y )

  and update_nodes ~seed n s e =
    with_node ~seed ~copy:false n s e @@ fun i x y -> if x != y then n.(i) <- y

  and add_nodes : type a. seed:_ -> copy:_ -> _ -> _ -> _ -> _ -> (t -> a) -> a
      =
   fun ~seed ~copy t n s e k ->
    with_node ~seed ~copy n s e @@ fun i x y ->
    if x == y then k t
    else
      let n = if copy then Array.copy n else n in
      n.(i) <- y;
      k (Nodes n)

  let add (t : t) s v =
    match t with
    | Empty -> singleton s v
    | Values vs as t -> (add_values [@tailcall]) ~seed:0 t vs s v (fun x -> x)
    | Nodes n as t ->
        (add_nodes [@tailcall]) ~seed:0 ~copy:true t n s v (fun x -> x)

  let remove_values t x s k =
    let y = StepMap.remove s x in
    if x == y then k t
    else if StepMap.is_empty y then k Empty
    else k (Values y)

  let fold f init t =
    let rec aux acc = function
      | Empty -> acc
      | Values vs -> f vs acc
      | Nodes n -> Array.fold_left aux acc n
    in
    aux init t

  exception Break

  let length t =
    try
      fold
        (fun es acc ->
          let n = StepMap.cardinal es + acc in
          if n > Conf.max_values then raise_notrace Break else n )
        0 t
    with Break -> Conf.max_values + 1

  let values t =
    fold
      (fun es acc -> StepMap.union (fun _ -> assert false) es acc)
      StepMap.empty t

  let rec remove_nodes ~seed t n s k =
    let i = index ~seed s in
    let x = n.(i) in
    let return y =
      if x == y then k t
      else
        (* FIXME(samoht): avoid copy if Entries isreturned *)
        let n = Array.copy n in
        n.(i) <- y;
        let t = Nodes n in
        if length t <= Conf.max_values then
          let vs = values t in
          k (Values vs)
        else k (Nodes n)
    in
    match x with
    | Empty -> return x
    | Values es as t -> (remove_values [@tailcall]) t es s return
    | Nodes n as t -> (remove_nodes [@tailcall]) ~seed:(seed + 1) t n s return

  let remove (t : t) s =
    match t with
    | Empty -> t
    | Values x -> (remove_values [@tailcall]) t x s (fun x -> x)
    | Nodes x -> (remove_nodes [@tailcall]) ~seed:0 t x s (fun x -> x)

  let load ~find h =
    let rec inode ~seed h k =
      find h >>= function
      | None -> k None
      | Some [] -> k (Some Empty)
      | Some i ->
          let vs, is =
            List.fold_left
              (fun (vs, is) -> function
                | Node n -> (StepMap.add n.name (`Node n.node) vs, is)
                | Contents c ->
                    (StepMap.add c.name (`Contents (c.node, c.metadata)) vs, is)
                | Inode i -> (vs, (i.index, i.node) :: is) )
              (StepMap.empty, []) i
          in
          if is = [] then k (Some (Values vs))
          else
            (nodes_of_values [@tailcall]) ~seed vs @@ fun n ->
            (inodes [@tailcall]) ~seed n is @@ fun n -> k (Some (Nodes n))
    and inodes ~seed n l k =
      match l with
      | [] -> k n
      | (i, h) :: t -> (
          (inode [@tailcall]) ~seed:(seed + 1) h @@ fun v ->
          (inodes [@tailcall]) ~seed n t @@ fun n ->
          match v with
          | None -> k n
          | Some v ->
              assert (i < Conf.max_inodes);
              assert (n.(i) = Empty);
              n.(i) <- v;
              k n )
    in
    inode ~seed:0 h (function
      | None -> Lwt.return None
      | Some x -> Lwt.return (Some x) )

  let fold f t init =
    let rec inode t k =
      match t with
      | Empty -> k []
      | Values vs -> k (entries_of_values vs)
      | Nodes n -> (inodes [@tailcall]) n 0 k
    and inodes n i k =
      if i >= Array.length n then k []
      else
        match n.(i) with
        | Empty -> inodes n (i + 1) k
        | Values es when StepMap.cardinal es = 1 ->
            let s, v = StepMap.choose es in
            (inodes [@tailcall]) n (i + 1) @@ fun inodes ->
            k (entry_of_value s v :: inodes)
        | x ->
            (inode [@tailcall]) x @@ fun t ->
            (inodes [@tailcall]) n (i + 1) @@ fun inodes ->
            f t @@ fun h -> k (entry_of_inode i h :: inodes)
    in
    inode t init

  let save ~add t = fold (fun x f -> add x >>= f) t add

  let pre_digest t =
    let pre_digest t = Type.pre_digest inode_t t in
    let digest t = K.digest (pre_digest t) in
    fold (fun t f -> f (digest t)) t pre_digest

  let pre_t : t Type.t =
    let open Type in
    mu @@ fun t ->
    variant "Node.t" (fun values nodes empty -> function
      | Values x -> values (StepMap.bindings x)
      | Nodes x -> nodes x
      | Empty -> empty )
    |~ case1 "Values"
         (list (pair P.step_t value_t))
         (fun e -> Values (StepMap.of_list e))
    |~ case1 "Nodes" (array t) (fun n -> Nodes n)
    |~ case0 "Empty" Empty |> sealv

  let v l : t =
    if List.length l < Conf.max_values then Values (StepMap.of_list l)
    else
      let aux t (s, v) =
        match t with
        | Empty -> singleton s v
        | Values vs as t ->
            (add_values [@tailcall]) ~seed:0 t vs s v (fun x -> x)
        | Nodes n as t ->
            (add_nodes [@tailcall]) ~seed:0 ~copy:false t n s v (fun x -> x)
      in
      List.fold_left aux empty l

  let t : t Type.t = Type.like ~pre_digest pre_t

  let step_t = P.step_t

  let hash_t = K.t

  let metadata_t = M.t

  let default = M.default
end

module Flat
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
    variant "Tree.kind" (fun node contents contents_m -> function
      | `Node -> node
      | `Contents m ->
          if Type.equal M.t m M.default then contents else contents_m m )
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

    let compare = Type.compare P.step_t
  end)

  type value = [ `Contents of hash * metadata | `Node of hash ]

  type t = entry StepMap.t

  type inode = t

  let v l =
    List.fold_left
      (fun acc x -> StepMap.add (fst x) (to_entry x) acc)
      StepMap.empty l

  let list t = List.map (fun (_, e) -> of_entry e) (StepMap.bindings t)

  let find t s =
    try
      let _, v = of_entry (StepMap.find s t) in
      Some v
    with Not_found -> None

  let empty = StepMap.empty

  let is_empty e = list e = []

  let add t k v =
    let e = to_entry (k, v) in
    StepMap.add k e t

  let remove t k = StepMap.remove k t

  let step_t = P.step_t

  let hash_t = K.t

  let metadata_t = M.t

  let default = M.default

  let value_t =
    let open Type in
    variant "value" (fun n c -> function
      | `Node h -> n h | `Contents (h, m) -> c (h, m) )
    |~ case1 "node" K.t (fun k -> `Node k)
    |~ case1 "contents" (pair K.t M.t) (fun (h, m) -> `Contents (h, m))
    |> sealv

  let of_entries e = v (List.map of_entry e)

  let entries e = List.map to_entry (list e)

  let t = Type.map Type.(list entry_t) of_entries entries

  let inode_t = t

  let load ~find t = find t

  let save ~add t = add t
end

module Store
    (C : S.CONTENTS_STORE)
    (P : S.PATH)
    (M : S.METADATA) (S : sig
        include S.CONTENT_ADDRESSABLE_STORE with type key = C.key

        module Key : S.HASH with type t = key

        module Val :
          S.NODE
          with type inode = value
           and type hash = key
           and type metadata = M.t
           and type step = P.step
    end) =
struct
  module Contents = C
  module Key = Hash.With_digest (S.Key) (S.Val)
  module Path = P
  module Metadata = M

  type 'a t = 'a C.t * 'a S.t

  type key = S.key

  type value = S.Val.t

  let mem (_, t) = S.mem t

  let find (_, t) k = S.Val.load ~find:(S.find t) k

  let add (_, t) v = S.Val.save ~add:(S.add t) v

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

  let merge_node (c, _) merge_key =
    let explode t = (all_contents t, all_succ t) in
    let implode (contents, succ) =
      let xs = List.map (fun (s, c) -> (s, `Contents c)) contents in
      let ys = List.map (fun (s, n) -> (s, `Node n)) succ in
      S.Val.v (xs @ ys)
    in
    let merge = Merge.pair (merge_contents_meta c) (merge_parents merge_key) in
    Merge.like S.Val.t merge explode implode

  let rec merge t =
    let merge_key =
      Merge.v (Type.option S.Key.t) (fun ~old x y ->
          Merge.(f (merge t)) ~old x y )
    in
    let merge = merge_node t merge_key in
    let read = function
      | None -> Lwt.return S.Val.empty
      | Some k -> ( find t k >|= function None -> S.Val.empty | Some v -> v )
    in
    let add v =
      if S.Val.is_empty v then Lwt.return_none
      else add t v >>= fun k -> Lwt.return (Some k)
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
    List.map
      (function _, `Node n -> `Node n | _, `Contents c -> `Contents c)
      (S.Val.list t)

  let pp_key = Type.pp S.Key.t

  let pp_keys = Fmt.(Dump.list pp_key)

  let pp_path = Type.pp S.Path.t

  let closure t ~min ~max =
    Log.debug (fun f -> f "closure min=%a max=%a" pp_keys min pp_keys max);
    let pred = function
      | `Node k -> ( S.find t k >|= function None -> [] | Some v -> edges v )
      | _ -> Lwt.return_nil
    in
    let min = List.map (fun x -> `Node x) min in
    let max = List.map (fun x -> `Node x) max in
    Graph.closure ~pred ~min ~max () >>= fun g ->
    let keys =
      List.fold_left
        (fun acc -> function `Node x -> x :: acc | _ -> acc)
        [] (Graph.vertex g)
    in
    Lwt.return keys

  let v t xs = S.add t (S.Val.v xs)

  let find_step t node step =
    Log.debug (fun f -> f "contents %a" pp_key node);
    S.find t node >|= function None -> None | Some n -> S.Val.find n step

  let find t node path =
    Log.debug (fun f -> f "read_node_exn %a %a" pp_key node pp_path path);
    let rec aux node path =
      match Path.decons path with
      | None -> Lwt.return (Some (`Node node))
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

  let value_t =
    let open Type in
    variant "value" (fun n c -> function
      | `Node h -> n h | `Contents (h, m) -> c (h, m) )
    |~ case1 "node" node_t (fun k -> `Node k)
    |~ case1 "contents" (pair contents_t metadata_t) (fun (h, m) ->
           `Contents (h, m) )
    |> sealv
end

module V1 (N : S.NODE) = struct
  module K = struct
    let h = Type.string_of `Int64

    let size_of ?headers x =
      Type.size_of ?headers h (Type.to_bin_string N.hash_t x)

    let encode_bin ?headers buf e =
      Type.encode_bin ?headers h buf (Type.to_bin_string N.hash_t e)

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

  type inode = N.inode

  let inode_t = N.inode_t

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
        | _ -> failwith "invalid node" )
    |+ field "contents" (option K.t) (function
         | `Contents (x, _) -> Some x
         | _ -> None )
    |+ field "metadata" (option N.metadata_t) (function
         | `Contents (_, x) when not (equal N.metadata_t N.default x) -> Some x
         | _ -> None )
    |+ field "node" (option K.t) (function `Node n -> Some n | _ -> None)
    |> sealr

  let t : t Type.t =
    Type.map Type.(list ~len:`Int64 (pair step_t value_t)) v list

  let save ~add:_ _ = failwith "TODO: V1.save"

  let load ~find:_ _ = failwith "TODO: V1.load"
end
