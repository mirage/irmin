(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
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
include Inode_intf

let src =
  Logs.Src.create "irmin.pack.i" ~doc:"inodes for the irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

let rec drop n (l : 'a Seq.t) () =
  match l () with
  | l' when n = 0 -> l'
  | Nil -> Nil
  | Cons (_, l') -> drop (n - 1) l' ()

let take : type a. int -> a Seq.t -> a list =
  let rec aux acc n (l : a Seq.t) =
    if n = 0 then acc
    else
      match l () with Nil -> acc | Cons (x, l') -> aux (x :: acc) (n - 1) l'
  in
  fun n s -> List.rev (aux [] n s)

module Make_intermediate
    (Conf : Config.S)
    (H : Irmin.Hash.S)
    (Node : Irmin.Private.Node.S with type hash = H.t) =
struct
  module Node = struct
    include Node
    module H = Irmin.Hash.Typed (H) (Node)

    let hash = H.hash
  end

  module T = struct
    type hash = H.t [@@deriving irmin]
    type step = Node.step [@@deriving irmin]
    type metadata = Node.metadata [@@deriving irmin]

    let default = Node.default

    type value = Node.value

    let value_t = Node.value_t
    let pp_hash = Irmin.Type.(pp hash_t)
  end

  module StepMap = struct
    include Map.Make (struct
      type t = T.step

      let compare = Irmin.Type.(unstage (compare T.step_t))
    end)

    let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l

    let t a =
      let open Irmin.Type in
      map (list (pair T.step_t a)) of_list bindings
  end

  (* Binary representation, useful to compute hashes *)
  module Bin = struct
    open T

    type ptr = { index : int; hash : H.t }
    type tree = { depth : int; length : int; entries : ptr list }
    type v = Values of (step * value) list | Tree of tree

    let ptr_t : ptr Irmin.Type.t =
      let open Irmin.Type in
      record "Bin.ptr" (fun index hash -> { index; hash })
      |+ field "index" int (fun t -> t.index)
      |+ field "hash" H.t (fun (t : ptr) -> t.hash)
      |> sealr

    let tree_t : tree Irmin.Type.t =
      let open Irmin.Type in
      record "Bin.tree" (fun depth length entries -> { depth; length; entries })
      |+ field "depth" int (fun t -> t.depth)
      |+ field "length" int (fun t -> t.length)
      |+ field "entries" (list ptr_t) (fun t -> t.entries)
      |> sealr

    let v_t : v Irmin.Type.t =
      let open Irmin.Type in
      variant "Bin.v" (fun values tree -> function
        | Values l -> values l | Tree i -> tree i)
      |~ case1 "Values" (list (pair step_t value_t)) (fun t -> Values t)
      |~ case1 "Tree" tree_t (fun t -> Tree t)
      |> sealv

    module V =
      Irmin.Hash.Typed
        (H)
        (struct
          type t = v

          let t = v_t
        end)

    type t = { hash : H.t Lazy.t; stable : bool; v : v }

    let pre_hash_v = Irmin.Type.(unstage (pre_hash v_t))

    let t : t Irmin.Type.t =
      let open Irmin.Type in
      let pre_hash = stage (fun x -> pre_hash_v x.v) in
      record "Bin.t" (fun hash stable v -> { hash = lazy hash; stable; v })
      |+ field "hash" H.t (fun t -> Lazy.force t.hash)
      |+ field "stable" bool (fun t -> t.stable)
      |+ field "v" v_t (fun t -> t.v)
      |> sealr
      |> like ~pre_hash

    let v ~stable ~hash v = { stable; hash; v }
    let hash t = Lazy.force t.hash
  end

  (* Compressed binary representation *)
  module Compress = struct
    open T

    type name = Indirect of int | Direct of step
    type address = Indirect of int64 | Direct of H.t

    let address_t : address Irmin.Type.t =
      let open Irmin.Type in
      variant "Compress.address" (fun i d -> function
        | Indirect x -> i x | Direct x -> d x)
      |~ case1 "Indirect" int64 (fun x -> Indirect x)
      |~ case1 "Direct" H.t (fun x -> Direct x)
      |> sealv

    type ptr = { index : int; hash : address }

    let ptr_t : ptr Irmin.Type.t =
      let open Irmin.Type in
      record "Compress.ptr" (fun index hash -> { index; hash })
      |+ field "index" int (fun t -> t.index)
      |+ field "hash" address_t (fun t -> t.hash)
      |> sealr

    type tree = { depth : int; length : int; entries : ptr list }

    let tree_t : tree Irmin.Type.t =
      let open Irmin.Type in
      record "Compress.tree" (fun depth length entries ->
          { depth; length; entries })
      |+ field "depth" int (fun t -> t.depth)
      |+ field "length" int (fun t -> t.length)
      |+ field "entries" (list ptr_t) (fun t -> t.entries)
      |> sealr

    type value =
      | Contents of name * address * metadata
      | Node of name * address

    let is_default = Irmin.Type.(unstage (equal T.metadata_t)) T.default

    let value_t : value Irmin.Type.t =
      let open Irmin.Type in
      variant "Compress.value"
        (fun
          contents_ii
          contents_x_ii
          node_ii
          contents_id
          contents_x_id
          node_id
          contents_di
          contents_x_di
          node_di
          contents_dd
          contents_x_dd
          node_dd
        -> function
        | Contents (Indirect n, Indirect h, m) ->
            if is_default m then contents_ii (n, h) else contents_x_ii (n, h, m)
        | Node (Indirect n, Indirect h) -> node_ii (n, h)
        | Contents (Indirect n, Direct h, m) ->
            if is_default m then contents_id (n, h) else contents_x_id (n, h, m)
        | Node (Indirect n, Direct h) -> node_id (n, h)
        | Contents (Direct n, Indirect h, m) ->
            if is_default m then contents_di (n, h) else contents_x_di (n, h, m)
        | Node (Direct n, Indirect h) -> node_di (n, h)
        | Contents (Direct n, Direct h, m) ->
            if is_default m then contents_dd (n, h) else contents_x_dd (n, h, m)
        | Node (Direct n, Direct h) -> node_dd (n, h))
      |~ case1 "contents-ii" (pair int int64) (fun (n, i) ->
             Contents (Indirect n, Indirect i, T.default))
      |~ case1 "contents-x-ii" (triple int int64 metadata_t) (fun (n, i, m) ->
             Contents (Indirect n, Indirect i, m))
      |~ case1 "node-ii" (pair int int64) (fun (n, i) ->
             Node (Indirect n, Indirect i))
      |~ case1 "contents-id" (pair int H.t) (fun (n, h) ->
             Contents (Indirect n, Direct h, T.default))
      |~ case1 "contents-x-id" (triple int H.t metadata_t) (fun (n, h, m) ->
             Contents (Indirect n, Direct h, m))
      |~ case1 "node-id" (pair int H.t) (fun (n, h) ->
             Node (Indirect n, Direct h))
      |~ case1 "contents-di" (pair step_t int64) (fun (n, i) ->
             Contents (Direct n, Indirect i, T.default))
      |~ case1 "contents-x-di" (triple step_t int64 metadata_t)
           (fun (n, i, m) -> Contents (Direct n, Indirect i, m))
      |~ case1 "node-di" (pair step_t int64) (fun (n, i) ->
             Node (Direct n, Indirect i))
      |~ case1 "contents-dd" (pair step_t H.t) (fun (n, i) ->
             Contents (Direct n, Direct i, T.default))
      |~ case1 "contents-x-dd" (triple step_t H.t metadata_t) (fun (n, i, m) ->
             Contents (Direct n, Direct i, m))
      |~ case1 "node-dd" (pair step_t H.t) (fun (n, i) ->
             Node (Direct n, Direct i))
      |> sealv

    type v = Values of value list | Tree of tree

    let v_t : v Irmin.Type.t =
      let open Irmin.Type in
      variant "Compress.v" (fun values tree -> function
        | Values x -> values x | Tree x -> tree x)
      |~ case1 "Values" (list value_t) (fun x -> Values x)
      |~ case1 "Tree" tree_t (fun x -> Tree x)
      |> sealv

    type t = { hash : H.t; stable : bool; v : v }

    let v ~stable ~hash v = { hash; stable; v }
    let magic_node = 'N'
    let magic_inode = 'I'

    let stable_t : bool Irmin.Type.t =
      Irmin.Type.(map char)
        (fun n -> n = magic_node)
        (function true -> magic_node | false -> magic_inode)

    let t =
      let open Irmin.Type in
      record "Compress.t" (fun hash stable v -> { hash; stable; v })
      |+ field "hash" H.t (fun t -> t.hash)
      |+ field "stable" stable_t (fun t -> t.stable)
      |+ field "v" v_t (fun t -> t.v)
      |> sealr
  end

  module Val_impl = struct
    open T

    let equal_hash = Irmin.Type.(unstage (equal hash_t))
    let equal_value = Irmin.Type.(unstage (equal value_t))

    type ptr = { target_hash : hash Lazy.t; mutable target : t option }

    and tree = { depth : int; length : int; entries : ptr option array }

    and v = Values of value StepMap.t | Tree of tree

    and t = { hash : hash Lazy.t; stable : bool; v : v }

    let pred t =
      match t.v with
      | Tree i ->
          Array.fold_left
            (fun acc -> function
              | None -> acc
              | Some i -> `Inode (Lazy.force i.target_hash) :: acc)
            [] i.entries
      | Values l ->
          StepMap.fold
            (fun _ v acc ->
              let v =
                match v with
                | `Node _ as k -> k
                | `Contents (k, _) -> `Contents k
              in
              v :: acc)
            l []

    let hash_of_ptr (i : ptr) = Lazy.force i.target_hash

    let ptr_t t : ptr Irmin.Type.t =
      let same_hash =
        Irmin.Type.stage @@ fun x y ->
        equal_hash (hash_of_ptr x) (hash_of_ptr y)
      in
      let open Irmin.Type in
      record "Inode.ptr" (fun hash target ->
          { target_hash = lazy hash; target })
      |+ field "hash" hash_t (fun t -> Lazy.force t.target_hash)
      |+ field "target" (option t) (fun t -> t.target)
      |> sealr
      |> like ~equal:same_hash

    let tree_t entry : tree Irmin.Type.t =
      let open Irmin.Type in
      record "Inode.tree" (fun depth length entries ->
          { depth; length; entries })
      |+ field "depth" int (fun t -> t.depth)
      |+ field "length" int (fun t -> t.length)
      |+ field "entries" (array entry) (fun t -> t.entries)
      |> sealr

    let length t =
      match t.v with Values vs -> StepMap.cardinal vs | Tree vs -> vs.length

    let stable t = t.stable

    let get_target ~find t =
      match t.target with
      | Some t -> t
      | None -> (
          let h = hash_of_ptr t in
          match find h with
          | None -> Fmt.failwith "%a: unknown key" pp_hash h
          | Some x ->
              t.target <- Some x;
              x)

    type acc = {
      cursor : int;
      values : (step * value) list list;
      remaining : int;
    }

    let empty_acc n = { cursor = 0; values = []; remaining = n }

    let rec list_entry ~offset ~length ~find acc = function
      | None -> acc
      | Some i -> list_values ~offset ~length ~find acc (get_target ~find i)

    and list_tree ~offset ~length ~find acc t =
      if acc.remaining <= 0 || offset + length <= acc.cursor then acc
      else if acc.cursor + t.length < offset then
        { acc with cursor = t.length + acc.cursor }
      else Array.fold_left (list_entry ~offset ~length ~find) acc t.entries

    and list_values ~offset ~length ~find acc t =
      if acc.remaining <= 0 || offset + length <= acc.cursor then acc
      else
        match t.v with
        | Values vs ->
            let len = StepMap.cardinal vs in
            if acc.cursor + len < offset then
              { acc with cursor = len + acc.cursor }
            else
              let to_drop =
                if acc.cursor > offset then 0 else offset - acc.cursor
              in
              let vs =
                StepMap.to_seq vs |> drop to_drop |> take acc.remaining
              in
              let n = List.length vs in
              {
                values = vs :: acc.values;
                cursor = acc.cursor + len;
                remaining = acc.remaining - n;
              }
        | Tree t -> list_tree ~offset ~length ~find acc t

    let list ?(offset = 0) ?length ~find t =
      let length =
        match length with
        | Some n -> n
        | None -> (
            match t.v with
            | Values vs -> StepMap.cardinal vs - offset
            | Tree i -> i.length - offset)
      in
      let entries = list_values ~offset ~length ~find (empty_acc length) t in
      List.concat (List.rev entries.values)

    let to_bin_v = function
      | Values vs ->
          let vs = StepMap.bindings vs in
          Bin.Values vs
      | Tree t ->
          let _, entries =
            Array.fold_left
              (fun (i, acc) -> function
                | None -> (i + 1, acc)
                | Some ptr ->
                    let hash = hash_of_ptr ptr in
                    (i + 1, { Bin.index = i; hash } :: acc))
              (0, []) t.entries
          in
          let entries = List.rev entries in
          Bin.Tree { depth = t.depth; length = t.length; entries }

    let to_bin t =
      let v = to_bin_v t.v in
      Bin.v ~stable:t.stable ~hash:t.hash v

    let hash t = Lazy.force t.hash

    let stabilize ~find t =
      if t.stable then t
      else
        let n = length t in
        if n > Conf.stable_hash then t
        else
          let hash =
            lazy
              (let vs = list ~find t in
               Node.hash (Node.v vs))
          in
          { hash; stable = true; v = t.v }

    let hash_key = Irmin.Type.(unstage (short_hash step_t))
    let index ~depth k = abs (hash_key ~seed:depth k) mod Conf.entries
    let entry ?target target_hash = Some { target; target_hash }

    let of_bin t =
      let v =
        match t.Bin.v with
        | Bin.Values vs ->
            let vs = StepMap.of_list vs in
            Values vs
        | Tree t ->
            let entries = Array.make Conf.entries None in
            List.iter
              (fun { Bin.index; hash } -> entries.(index) <- entry (lazy hash))
              t.entries;
            Tree { depth = t.Bin.depth; length = t.length; entries }
      in
      { hash = t.Bin.hash; stable = t.Bin.stable; v }

    let pre_hash_bin = Irmin.Type.(unstage (pre_hash Bin.v_t))

    let v_t t : v Irmin.Type.t =
      let open Irmin.Type in
      let pre_hash = stage (fun x -> pre_hash_bin (to_bin_v x)) in
      let entry = option (ptr_t t) in
      variant "Inode.v" (fun values tree -> function
        | Values v -> values v | Tree i -> tree i)
      |~ case1 "Values" (StepMap.t value_t) (fun t -> Values t)
      |~ case1 "Tree" (tree_t entry) (fun t -> Tree t)
      |> sealv
      |> like ~pre_hash

    let t : t Irmin.Type.t =
      let open Irmin.Type in
      mu @@ fun t ->
      let v = v_t t in
      let t =
        record "Inode.t" (fun hash stable v -> { hash = lazy hash; stable; v })
        |+ field "hash" H.t (fun t -> Lazy.force t.hash)
        |+ field "stable" bool (fun t -> t.stable)
        |+ field "v" v (fun t -> t.v)
        |> sealr
      in
      let pre_hash = Irmin.Type.unstage (Irmin.Type.pre_hash v) in
      like ~pre_hash:(stage @@ fun x -> pre_hash x.v) t

    let empty =
      let hash = lazy (Node.hash Node.empty) in
      { stable = true; hash; v = Values StepMap.empty }

    let values vs =
      let length = StepMap.cardinal vs in
      if length = 0 then empty
      else
        let v = Values vs in
        let hash = lazy (Bin.V.hash (to_bin_v v)) in
        { hash; stable = false; v }

    let tree is =
      let v = Tree is in
      let hash = lazy (Bin.V.hash (to_bin_v v)) in
      { hash; stable = false; v }

    let of_values l = values (StepMap.of_list l)

    let is_empty t =
      match t.v with Values vs -> StepMap.is_empty vs | Tree _ -> false

    let find_value ~depth ~find t s =
      let rec aux ~depth = function
        | Values vs -> ( try Some (StepMap.find s vs) with Not_found -> None)
        | Tree t -> (
            let i = index ~depth s in
            let x = t.entries.(i) in
            match x with
            | None -> None
            | Some i -> aux ~depth:(depth + 1) (get_target ~find i).v)
      in
      aux ~depth t.v

    let find ~find t s = find_value ~depth:0 ~find t s

    let rec add ~depth ~find ~copy ~replace t s v k =
      match t.v with
      | Values vs ->
          let length =
            if replace then StepMap.cardinal vs else StepMap.cardinal vs + 1
          in
          let t =
            if length <= Conf.entries then values (StepMap.add s v vs)
            else
              let vs = StepMap.bindings (StepMap.add s v vs) in
              let empty =
                tree
                  { length = 0; depth; entries = Array.make Conf.entries None }
              in
              let aux t (s, v) =
                (add [@tailcall]) ~depth ~find ~copy:false ~replace t s v
                  (fun x -> x)
              in
              List.fold_left aux empty vs
          in
          k t
      | Tree t -> (
          let length = if replace then t.length else t.length + 1 in
          let entries = if copy then Array.copy t.entries else t.entries in
          let i = index ~depth s in
          match entries.(i) with
          | None ->
              let target = values (StepMap.singleton s v) in
              entries.(i) <- entry ~target target.hash;
              let t = tree { depth; length; entries } in
              k t
          | Some n ->
              let t = get_target ~find n in
              add ~depth:(depth + 1) ~find ~copy ~replace t s v @@ fun target ->
              entries.(i) <- entry ~target target.hash;
              let t = tree { depth; length; entries } in
              k t)

    let add ~find ~copy t s v =
      (* XXX: [find_value ~depth:42] should break the unit tests. It doesn't. *)
      match find_value ~depth:0 ~find t s with
      | Some v' when equal_value v v' -> stabilize ~find t
      | Some _ -> add ~depth:0 ~find ~copy ~replace:true t s v (stabilize ~find)
      | None -> add ~depth:0 ~find ~copy ~replace:false t s v (stabilize ~find)

    let rec remove ~depth ~find t s k =
      match t.v with
      | Values vs ->
          let t = values (StepMap.remove s vs) in
          k t
      | Tree t -> (
          let len = t.length - 1 in
          if len <= Conf.entries then
            let vs =
              list_tree ~offset:0 ~length:t.length ~find (empty_acc t.length) t
            in
            let vs = List.concat (List.rev vs.values) in
            let vs = StepMap.of_list vs in
            let vs = StepMap.remove s vs in
            let t = values vs in
            k t
          else
            let entries = Array.copy t.entries in
            let i = index ~depth s in
            match entries.(i) with
            | None -> assert false
            | Some t ->
                let t = get_target ~find t in
                if length t = 1 then (
                  entries.(i) <- None;
                  let t = tree { depth; length = len; entries } in
                  k t)
                else
                  remove ~depth:(depth + 1) ~find t s @@ fun target ->
                  entries.(i) <- entry ~target (lazy (hash target));
                  let t = tree { depth; length = len; entries } in
                  k t)

    let remove ~find t s =
      (* XXX: [find_value ~depth:42] should break the unit tests. It doesn't. *)
      match find_value ~depth:0 ~find t s with
      | None -> stabilize ~find t
      | Some _ -> remove ~find ~depth:0 t s (stabilize ~find)

    let v l : t =
      let len = List.length l in
      let find _ = assert false in
      let t =
        if len <= Conf.entries then of_values l
        else
          let aux acc (s, v) = add ~find ~copy:false acc s v in
          List.fold_left aux empty l
      in
      stabilize ~find t

    let add ~find t s v = add ~find ~copy:true t s v

    let save ~add ~mem t =
      let rec aux ~depth t =
        Log.debug (fun l -> l "save depth:%d" depth);
        match t.v with
        | Values _ -> add (Lazy.force t.hash) (to_bin t)
        | Tree n ->
            Array.iter
              (function
                | None | Some { target = None; _ } -> ()
                | Some ({ target = Some t; _ } as i) ->
                    let hash = hash_of_ptr i in
                    if mem hash then () else aux ~depth:(depth + 1) t)
              n.entries;
            add (Lazy.force t.hash) (to_bin t)
      in
      aux ~depth:0 t

    let check_stable ~find t =
      let rec check t any_stable_ancestor =
        let stable = t.stable || any_stable_ancestor in
        match t.v with
        | Values _ -> true
        | Tree tree ->
            Array.for_all
              (function
                | None -> true
                | Some t ->
                    let t = get_target ~find t in
                    (if stable then not t.stable else true) && check t stable)
              tree.entries
      in
      check t t.stable

    let contains_empty_map ~find t =
      let rec check_lower t =
        match t.v with
        | Values l when StepMap.is_empty l -> true
        | Values _ -> false
        | Tree inodes ->
            Array.exists
              (function
                | None -> false
                | Some t ->
                    let t = get_target ~find t in
                    check_lower t)
              inodes.entries
      in
      check_lower t

    let is_tree t = match t.v with Tree _ -> true | Values _ -> false
  end

  module Elt = struct
    type t = Bin.t

    let t = Bin.t

    let magic (t : t) =
      if t.stable then Compress.magic_node else Compress.magic_inode

    let hash t = Bin.hash t
    let step_to_bin = Irmin.Type.(unstage (to_bin_string T.step_t))
    let step_of_bin = Irmin.Type.(unstage (of_bin_string T.step_t))
    let encode_compress = Irmin.Type.(unstage (encode_bin Compress.t))
    let decode_compress = Irmin.Type.(unstage (decode_bin Compress.t))

    let encode_bin ~dict ~offset (t : t) k =
      let step s : Compress.name =
        let str = step_to_bin s in
        if String.length str <= 3 then Direct s
        else match dict str with Some i -> Indirect i | None -> Direct s
      in
      let hash h : Compress.address =
        match offset h with
        | None -> Compress.Direct h
        | Some off -> Compress.Indirect off
      in
      let ptr : Bin.ptr -> Compress.ptr =
       fun n ->
        let hash = hash n.hash in
        { index = n.index; hash }
      in
      let value : T.step * T.value -> Compress.value = function
        | s, `Contents (c, m) ->
            let s = step s in
            let v = hash c in
            Compress.Contents (s, v, m)
        | s, `Node n ->
            let s = step s in
            let v = hash n in
            Compress.Node (s, v)
      in
      (* List.map is fine here as the number of entries is small *)
      let v : Bin.v -> Compress.v = function
        | Values vs -> Values (List.map value vs)
        | Tree { depth; length; entries } ->
            let entries = List.map ptr entries in
            Tree { Compress.depth; length; entries }
      in
      let t = Compress.v ~stable:t.stable ~hash:k (v t.v) in
      encode_compress t

    exception Exit of [ `Msg of string ]

    let decode_bin_with_offset ~dict ~hash t off : int * t =
      let off, i = decode_compress t off in
      let step : Compress.name -> T.step = function
        | Direct n -> n
        | Indirect s -> (
            match dict s with
            | None -> raise_notrace (Exit (`Msg "dict"))
            | Some s -> (
                match step_of_bin s with
                | Error e -> raise_notrace (Exit e)
                | Ok v -> v))
      in
      let hash : Compress.address -> H.t = function
        | Indirect off -> hash off
        | Direct n -> n
      in
      let ptr : Compress.ptr -> Bin.ptr =
       fun n ->
        let hash = hash n.hash in
        { index = n.index; hash }
      in
      let value : Compress.value -> T.step * T.value = function
        | Contents (n, h, metadata) ->
            let name = step n in
            let hash = hash h in
            (name, `Contents (hash, metadata))
        | Node (n, h) ->
            let name = step n in
            let hash = hash h in
            (name, `Node hash)
      in
      let t : Compress.v -> Bin.v = function
        | Values vs -> Values (List.rev_map value (List.rev vs))
        | Tree { depth; length; entries } ->
            let entries = List.map ptr entries in
            Tree { depth; length; entries }
      in
      let t = Bin.v ~stable:i.stable ~hash:(lazy i.hash) (t i.v) in
      (off, t)

    let decode_bin ~dict ~hash t off =
      decode_bin_with_offset ~dict ~hash t off |> snd
  end

  type hash = T.hash

  let pp_hash = T.pp_hash

  let decode_bin ~dict ~hash t off =
    Elt.decode_bin_with_offset ~dict ~hash t off

  module Val = struct
    include T
    module I = Val_impl

    type t = { find : H.t -> I.t option; v : I.t }

    let pred t = I.pred t.v
    let niet _ = assert false

    let v l =
      let v = I.v l in
      { find = niet; v }

    let list ?offset ?length t = I.list ~find:t.find ?offset ?length t.v
    let empty = { find = niet; v = I.empty }
    let is_empty t = I.is_empty t.v
    let find t s = I.find ~find:t.find t.v s

    let add t s v =
      let v = I.add ~find:t.find t.v s v in
      if v == t.v then t else { t with v }

    let remove t s =
      let v = I.remove ~find:t.find t.v s in
      if v == t.v then t else { t with v }

    let pre_hash_i = Irmin.Type.(unstage (pre_hash I.t))
    let pre_hash_node = Irmin.Type.(unstage (pre_hash Node.t))

    let t : t Irmin.Type.t =
      let pre_hash =
        Irmin.Type.stage @@ fun x ->
        if not x.v.stable then pre_hash_i x.v
        else
          let vs = list x in
          pre_hash_node (Node.v vs)
      in
      Irmin.Type.map I.t ~pre_hash (fun v -> { find = niet; v }) (fun t -> t.v)

    let hash t = I.hash t.v
    let save ~add ~mem { v; _ } = I.save ~add ~mem v

    let of_bin find v =
      let find h =
        match find h with None -> None | Some v -> Some (I.of_bin v)
      in
      { v = I.of_bin v; find }

    let to_bin { v; _ } = I.to_bin v
    let stable t = I.stable t.v
    let length t = I.length t.v
    let index = I.index

    let integrity_check t =
      let check_stable t =
        let check t = I.check_stable ~find:t.find t.v in
        let n = length t in
        if n > Conf.stable_hash then (not (stable t)) && check t
        else stable t && check t
      in
      let contains_empty_map_non_root t =
        let check t = I.contains_empty_map ~find:t.find t.v in
        (* we are only looking for empty maps that are not at the root *)
        if I.is_tree t.v then check t else false
      in
      check_stable t && not (contains_empty_map_non_root t)
  end
end

module Make_ext
    (H : Irmin.Hash.S)
    (Node : Irmin.Private.Node.S with type hash = H.t)
    (Inter : INTER
               with type hash = H.t
                and type Val.metadata = Node.metadata
                and type Val.step = Node.step)
    (P : Pack.MAKER with type key = H.t and type index = Pack_index.Make(H).t) =
struct
  module Key = H
  module Pack = P.Make (Inter.Elt)

  type 'a t = 'a Pack.t
  type key = Key.t
  type value = Inter.Val.t
  type index = Pack.index

  let mem t k = Pack.mem t k

  let find t k =
    Pack.find t k >|= function
    | None -> None
    | Some v ->
        let find = Pack.unsafe_find ~check_integrity:true t in
        let v = Inter.Val.of_bin find v in
        Some v

  let save t v =
    let add k v =
      Pack.unsafe_append ~ensure_unique:true ~overcommit:false t k v
    in
    Inter.Val.save ~add ~mem:(Pack.unsafe_mem t) v

  let hash v = Inter.Val.hash v

  let add t v =
    save t v;
    Lwt.return (hash v)

  let equal_hash = Irmin.Type.(unstage (equal H.t))

  let check_hash expected got =
    if equal_hash expected got then ()
    else
      Fmt.invalid_arg "corrupted value: got %a, expecting %a" Inter.pp_hash
        expected Inter.pp_hash got

  let unsafe_add t k v =
    check_hash k (hash v);
    save t v;
    Lwt.return_unit

  let batch = Pack.batch
  let v = Pack.v
  let integrity_check = Pack.integrity_check
  let close = Pack.close
  let sync = Pack.sync
  let clear = Pack.clear
  let clear_caches = Pack.clear_caches

  let decode_bin ~dict ~hash buff off =
    Inter.decode_bin ~dict ~hash buff off |> fst

  module Val = Inter.Val

  let integrity_check_inodes t k =
    find t k >|= function
    | None ->
        (* we are traversing the node graph, should find all values *)
        assert false
    | Some v ->
        if Inter.Val.integrity_check v then Ok ()
        else
          let msg =
            Fmt.str "Problematic inode %a" (Irmin.Type.pp Inter.Val.t) v
          in
          Error msg
end

module Make
    (Conf : Config.S)
    (H : Irmin.Hash.S)
    (P : Pack.MAKER with type key = H.t and type index = Pack_index.Make(H).t)
    (Node : Irmin.Private.Node.S with type hash = H.t) =
struct
  module Inter = Make_intermediate (Conf) (H) (Node)
  include Make_ext (H) (Node) (Inter) (P)
end
