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

open Lwt.Infix
include Inode_intf

let src =
  Logs.Src.create "irmin.pack.i" ~doc:"inodes for the irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

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

  module Inode = struct
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

      type inode = { index : int; hash : H.t }

      type inodes = { seed : int; length : int; entries : inode list }

      type v = Values of (step * value) list | Inodes of inodes

      let inode : inode Irmin.Type.t =
        let open Irmin.Type in
        record "Bin.inode" (fun index hash -> { index; hash })
        |+ field "index" int (fun t -> t.index)
        |+ field "hash" H.t (fun (t : inode) -> t.hash)
        |> sealr

      let inodes : inodes Irmin.Type.t =
        let open Irmin.Type in
        record "Bin.inodes" (fun seed length entries ->
            { seed; length; entries })
        |+ field "seed" int (fun t -> t.seed)
        |+ field "length" int (fun t -> t.length)
        |+ field "entries" (list inode) (fun t -> t.entries)
        |> sealr

      let v_t : v Irmin.Type.t =
        let open Irmin.Type in
        variant "Bin.v" (fun values inodes -> function
          | Values l -> values l | Inodes i -> inodes i)
        |~ case1 "Values" (list (pair step_t value_t)) (fun t -> Values t)
        |~ case1 "Inodes" inodes (fun t -> Inodes t)
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

      let node ~hash v = { stable = true; hash; v }

      let inode ~hash v = { stable = false; hash; v }

      let hash t = Lazy.force t.hash
    end

    (* Compressed binary representation *)
    module Compress = struct
      open T

      type name = Indirect of int | Direct of step

      type address = Indirect of int64 | Direct of H.t

      let address : address Irmin.Type.t =
        let open Irmin.Type in
        variant "Compress.address" (fun i d -> function
          | Indirect x -> i x | Direct x -> d x)
        |~ case1 "Indirect" int64 (fun x -> Indirect x)
        |~ case1 "Direct" H.t (fun x -> Direct x)
        |> sealv

      type inode = { index : int; hash : address }

      let inode : inode Irmin.Type.t =
        let open Irmin.Type in
        record "Compress.inode" (fun index hash -> { index; hash })
        |+ field "index" int (fun t -> t.index)
        |+ field "hash" address (fun t -> t.hash)
        |> sealr

      type inodes = { seed : int; length : int; entries : inode list }

      let inodes : inodes Irmin.Type.t =
        let open Irmin.Type in
        record "Compress.inodes" (fun seed length entries ->
            { seed; length; entries })
        |+ field "seed" int (fun t -> t.seed)
        |+ field "length" int (fun t -> t.length)
        |+ field "entries" (list inode) (fun t -> t.entries)
        |> sealr

      type value =
        | Contents of name * address * metadata
        | Node of name * address

      let is_default = Irmin.Type.(unstage (equal T.metadata_t)) T.default

      let value : value Irmin.Type.t =
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
              if is_default m then contents_ii (n, h)
              else contents_x_ii (n, h, m)
          | Node (Indirect n, Indirect h) -> node_ii (n, h)
          | Contents (Indirect n, Direct h, m) ->
              if is_default m then contents_id (n, h)
              else contents_x_id (n, h, m)
          | Node (Indirect n, Direct h) -> node_id (n, h)
          | Contents (Direct n, Indirect h, m) ->
              if is_default m then contents_di (n, h)
              else contents_x_di (n, h, m)
          | Node (Direct n, Indirect h) -> node_di (n, h)
          | Contents (Direct n, Direct h, m) ->
              if is_default m then contents_dd (n, h)
              else contents_x_dd (n, h, m)
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
        |~ case1 "contents-x-dd" (triple step_t H.t metadata_t)
             (fun (n, i, m) -> Contents (Direct n, Direct i, m))
        |~ case1 "node-dd" (pair step_t H.t) (fun (n, i) ->
               Node (Direct n, Direct i))
        |> sealv

      type v = Values of value list | Inodes of inodes

      let v_t : v Irmin.Type.t =
        let open Irmin.Type in
        variant "Compress.v" (fun values inodes -> function
          | Values x -> values x | Inodes x -> inodes x)
        |~ case1 "Values" (list value) (fun x -> Values x)
        |~ case1 "Inodes" inodes (fun x -> Inodes x)
        |> sealv

      type t = { hash : H.t; stable : bool; v : v }

      let node ~hash v = { hash; stable = true; v }

      let inode ~hash v = { hash; stable = false; v }

      let magic_node = 'N'

      let magic_inode = 'I'

      let stable : bool Irmin.Type.t =
        Irmin.Type.(map char)
          (fun n -> n = magic_node)
          (function true -> magic_node | false -> magic_inode)

      let t =
        let open Irmin.Type in
        record "Compress.t" (fun hash stable v -> { hash; stable; v })
        |+ field "hash" H.t (fun t -> t.hash)
        |+ field "stable" stable (fun t -> t.stable)
        |+ field "v" v_t (fun t -> t.v)
        |> sealr
    end

    module Val = struct
      open T

      let equal_hash = Irmin.Type.(unstage (equal hash_t))

      let equal_value = Irmin.Type.(unstage (equal value_t))

      type inode = { i_hash : hash Lazy.t; mutable tree : t option }

      and entry = Empty | Inode of inode

      and inodes = { seed : int; length : int; entries : entry array }

      and v = Values of value StepMap.t | Inodes of inodes

      and t = { hash : hash Lazy.t; stable : bool; v : v }

      let pred t =
        match t.v with
        | Inodes i ->
            Array.fold_left
              (fun acc -> function
                | Empty -> acc
                | Inode i -> `Inode (Lazy.force i.i_hash) :: acc)
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

      let hash_of_inode (i : inode) = Lazy.force i.i_hash

      let inode_t t : inode Irmin.Type.t =
        let same_hash =
          Irmin.Type.stage @@ fun x y ->
          equal_hash (hash_of_inode x) (hash_of_inode y)
        in
        let open Irmin.Type in
        record "Node.inode" (fun hash tree -> { i_hash = lazy hash; tree })
        |+ field "hash" hash_t (fun t -> Lazy.force t.i_hash)
        |+ field "tree" (option t) (fun t -> t.tree)
        |> sealr
        |> like ~equal:same_hash

      let entry_t inode : entry Irmin.Type.t =
        let open Irmin.Type in
        variant "Node.entry" (fun empty inode -> function
          | Empty -> empty | Inode i -> inode i)
        |~ case0 "Empty" Empty
        |~ case1 "Inode" inode (fun i -> Inode i)
        |> sealv

      let inodes entry : inodes Irmin.Type.t =
        let open Irmin.Type in
        record "Node.entries" (fun seed length entries ->
            { seed; length; entries })
        |+ field "seed" int (fun t -> t.seed)
        |+ field "length" int (fun t -> t.length)
        |+ field "entries" (array entry) (fun t -> t.entries)
        |> sealr

      let length t =
        match t.v with
        | Values vs -> StepMap.cardinal vs
        | Inodes vs -> vs.length

      let get_tree ~find t =
        match t.tree with
        | Some t -> t
        | None -> (
            let h = hash_of_inode t in
            match find h with
            | None -> Fmt.failwith "%a: unknown key" pp_hash h
            | Some x ->
                t.tree <- Some x;
                x)

      let rec list_entry ~find acc = function
        | Empty -> acc
        | Inode i -> list_values ~find acc (get_tree ~find i)

      and list_inodes ~find acc t =
        Array.fold_left (list_entry ~find) acc t.entries

      and list_values ~find acc t =
        match t.v with
        | Values vs -> StepMap.bindings vs @ acc
        | Inodes t -> list_inodes ~find acc t

      let compare_step = Irmin.Type.(unstage (compare step_t))

      let compare_entry x y = compare_step (fst x) (fst y)

      let list ~find t =
        let entries = list_values ~find [] t in
        List.fast_sort compare_entry entries

      let to_bin_v = function
        | Values vs ->
            let vs = StepMap.bindings vs in
            Bin.Values vs
        | Inodes t ->
            let _, entries =
              Array.fold_left
                (fun (i, acc) -> function
                  | Empty -> (i + 1, acc)
                  | Inode inode ->
                      let hash = hash_of_inode inode in
                      (i + 1, { Bin.index = i; hash } :: acc))
                (0, []) t.entries
            in
            let entries = List.rev entries in
            Bin.Inodes { seed = t.seed; length = t.length; entries }

      let to_bin t =
        let v = to_bin_v t.v in
        if t.stable then Bin.node ~hash:t.hash v else Bin.inode ~hash:t.hash v

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

      let index ~seed k = abs (hash_key ~seed k) mod Conf.entries

      let inode ?tree i_hash = Inode { tree; i_hash }

      let of_bin t =
        let v =
          match t.Bin.v with
          | Bin.Values vs ->
              let vs = StepMap.of_list vs in
              Values vs
          | Inodes t ->
              let entries = Array.make Conf.entries Empty in
              List.iter
                (fun { Bin.index; hash } ->
                  entries.(index) <- inode (lazy hash))
                t.entries;
              Inodes { seed = t.Bin.seed; length = t.length; entries }
        in
        { hash = t.Bin.hash; stable = t.Bin.stable; v }

      let pre_hash_bin = Irmin.Type.(unstage (pre_hash Bin.v_t))

      let v_t t : v Irmin.Type.t =
        let open Irmin.Type in
        let pre_hash = stage (fun x -> pre_hash_bin (to_bin_v x)) in
        let entry = entry_t (inode_t t) in
        variant "Inode.t" (fun values inodes -> function
          | Values v -> values v | Inodes i -> inodes i)
        |~ case1 "Values" (StepMap.t value_t) (fun t -> Values t)
        |~ case1 "Inodes" (inodes entry) (fun t -> Inodes t)
        |> sealv
        |> like ~pre_hash

      let t : t Irmin.Type.t =
        let open Irmin.Type in
        mu @@ fun t ->
        let v = v_t t in
        let t =
          record "hash" (fun hash stable v -> { hash = lazy hash; stable; v })
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

      let inodes is =
        let v = Inodes is in
        let hash = lazy (Bin.V.hash (to_bin_v v)) in
        { hash; stable = false; v }

      let of_values l = values (StepMap.of_list l)

      let is_empty t =
        match t.v with Values vs -> StepMap.is_empty vs | Inodes _ -> false

      let find_value ~seed ~find t s =
        let rec aux ~seed = function
          | Values vs -> (
              try Some (StepMap.find s vs) with Not_found -> None)
          | Inodes t -> (
              let i = index ~seed s in
              let x = t.entries.(i) in
              match x with
              | Empty -> None
              | Inode i -> aux ~seed:(seed + 1) (get_tree ~find i).v)
        in
        aux ~seed t.v

      let find ~find t s = find_value ~seed:0 ~find t s

      let rec add ~seed ~find ~copy t s v k =
        match find_value ~seed ~find t s with
        | Some v' when equal_value v v' -> k t
        | v' -> (
            match t.v with
            | Values vs ->
                let length =
                  match v' with
                  | None -> StepMap.cardinal vs + 1
                  | Some _ -> StepMap.cardinal vs
                in
                let t =
                  if length <= Conf.entries then values (StepMap.add s v vs)
                  else
                    let vs = StepMap.bindings (StepMap.add s v vs) in
                    let empty =
                      inodes
                        {
                          length = 0;
                          seed;
                          entries = Array.make Conf.entries Empty;
                        }
                    in
                    let aux t (s, v) =
                      (add [@tailcall]) ~seed ~find ~copy:false t s v (fun x ->
                          x)
                    in
                    List.fold_left aux empty vs
                in
                k t
            | Inodes t -> (
                let length =
                  match v' with None -> t.length + 1 | Some _ -> t.length
                in
                let entries =
                  if copy then Array.copy t.entries else t.entries
                in
                let i = index ~seed s in
                match entries.(i) with
                | Empty ->
                    let tree = values (StepMap.singleton s v) in
                    entries.(i) <- inode ~tree tree.hash;
                    let t = inodes { seed; length; entries } in
                    k t
                | Inode n ->
                    let t = get_tree ~find n in
                    add ~seed:(seed + 1) ~find ~copy t s v @@ fun tree ->
                    let inode = inode ~tree tree.hash in
                    entries.(i) <- inode;
                    let t = inodes { seed; length; entries } in
                    k t))

      let add ~find ~copy t s v =
        add ~seed:0 ~find ~copy t s v (stabilize ~find)

      let rec remove ~seed ~find t s k =
        match find_value ~seed ~find t s with
        | None -> k t
        | Some _ -> (
            match t.v with
            | Values vs ->
                let t = values (StepMap.remove s vs) in
                k t
            | Inodes t -> (
                let length = t.length - 1 in
                if length <= Conf.entries then
                  let vs = list_inodes ~find [] t in
                  let vs = StepMap.of_list vs in
                  let vs = StepMap.remove s vs in
                  let t = values vs in
                  k t
                else
                  let entries = Array.copy t.entries in
                  let i = index ~seed s in
                  match entries.(i) with
                  | Empty -> assert false
                  | Inode t ->
                      let t = get_tree ~find t in
                      remove ~seed:(seed + 1) ~find t s @@ fun tree ->
                      entries.(i) <- inode ~tree (lazy (hash tree));
                      let t = inodes { seed; length; entries } in
                      k t))

      let remove ~find t s = remove ~find ~seed:0 t s (stabilize ~find)

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
        let rec aux ~seed t =
          Log.debug (fun l -> l "save seed:%d" seed);
          match t.v with
          | Values _ -> add (Lazy.force t.hash) (to_bin t)
          | Inodes n ->
              Array.iter
                (function
                  | Empty | Inode { tree = None; _ } -> ()
                  | Inode ({ tree = Some t; _ } as i) ->
                      let hash = hash_of_inode i in
                      if mem hash then () else aux ~seed:(seed + 1) t)
                n.entries;
              add (Lazy.force t.hash) (to_bin t)
        in
        aux ~seed:0 t
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
        let inode : Bin.inode -> Compress.inode =
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
          | Inodes { seed; length; entries } ->
              let entries = List.map inode entries in
              Inodes { Compress.seed; length; entries }
        in
        let t =
          if t.stable then Compress.node ~hash:k (v t.v)
          else Compress.inode ~hash:k (v t.v)
        in
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
        let inode : Compress.inode -> Bin.inode =
         fun n ->
          let hash = hash n.hash in
          { index = n.index; hash }
        in
        let value : Compress.value -> T.step * T.value = function
          | Contents (n, h, metadata) ->
              let name = step n in
              let node = hash h in
              (name, `Contents (node, metadata))
          | Node (n, h) ->
              let name = step n in
              let node = hash h in
              (name, `Node node)
        in
        let t : Compress.v -> Bin.v = function
          | Values vs -> Values (List.rev_map value (List.rev vs))
          | Inodes { seed; length; entries } ->
              let entries = List.map inode entries in
              Inodes { seed; length; entries }
        in
        let t =
          if i.stable then Bin.node ~hash:(lazy i.hash) (t i.v)
          else Bin.inode ~hash:(lazy i.hash) (t i.v)
        in
        (off, t)

      let decode_bin ~dict ~hash t off =
        decode_bin_with_offset ~dict ~hash t off |> snd
    end

    type hash = T.hash

    let pp_hash = T.pp_hash

    let decode_bin ~dict ~hash t off =
      Elt.decode_bin_with_offset ~dict ~hash t off
  end

  module Val = struct
    include T
    module I = Inode.Val

    type inode_val = I.t

    type t = { find : H.t -> I.t option; v : I.t }

    let pred t = I.pred t.v

    let niet _ = assert false

    let v l =
      let v = I.v l in
      { find = niet; v }

    let list t = I.list ~find:t.find t.v

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
  end
end

module Make_ext
    (H : Irmin.Hash.S)
    (Node : Irmin.Private.Node.S with type hash = H.t)
    (Inode : INODE_EXT with type hash = H.t)
    (Val : VAL_INTER
             with type hash = H.t
              and type inode_val = Inode.Val.t
              and type metadata = Node.metadata
              and type step = Node.step) =
struct
  module Key = H

  type 'a t = 'a Inode.t

  type key = Key.t

  type value = Val.t

  let mem t k = Inode.mem t k

  let unsafe_find ~check_integrity t k =
    match Inode.unsafe_find ~check_integrity t k with
    | None -> None
    | Some v ->
        let v = Inode.Val.of_bin v in
        Some v

  let find t k =
    Inode.find t k >|= function
    | None -> None
    | Some v ->
        let v = Inode.Val.of_bin v in
        let find = unsafe_find ~check_integrity:true t in
        Some { Val.find; v }

  let save t v =
    let add k v =
      Inode.unsafe_append ~ensure_unique:true ~overcommit:false t k v
    in
    Inode.Val.save ~add ~mem:(Inode.unsafe_mem t) v

  let hash v = Inode.Val.hash v.Val.v

  let add t v =
    save t v.Val.v;
    Lwt.return (hash v)

  let equal_hash = Irmin.Type.(unstage (equal H.t))

  let check_hash expected got =
    if equal_hash expected got then ()
    else
      Fmt.invalid_arg "corrupted value: got %a, expecting %a" Inode.pp_hash
        expected Inode.pp_hash got

  let unsafe_add t k v =
    check_hash k (hash v);
    save t v.Val.v;
    Lwt.return_unit

  let batch = Inode.batch

  let v = Inode.v

  let integrity_check = Inode.integrity_check

  let close = Inode.close

  let sync = Inode.sync

  let clear = Inode.clear

  let clear_caches = Inode.clear_caches

  let decode_bin ~dict ~hash buff off =
    Inode.decode_bin ~dict ~hash buff off |> fst
end

module Make
    (Conf : Config.S)
    (H : Irmin.Hash.S)
    (Pack : Pack.MAKER with type key = H.t)
    (Node : Irmin.Private.Node.S with type hash = H.t) =
struct
  type index = Pack.index

  include Make_intermediate (Conf) (H) (Node)

  module Inode = struct
    include Inode
    include Pack.Make (Elt)
  end

  include Make_ext (H) (Node) (Inode) (Val)
end
