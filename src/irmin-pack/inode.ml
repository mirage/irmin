(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

module Make_internal
    (Conf : Conf.S)
    (H : Irmin.Hash.S)
    (Node : Irmin.Node.S with type hash = H.t) =
struct
  let () =
    if Conf.entries > Conf.stable_hash then
      invalid_arg "entries should be lower or equal to stable_hash"

  module Node = struct
    include Node
    module H = Irmin.Hash.Typed (H) (Node)

    let hash = H.hash
  end

  module T = struct
    type hash = H.t [@@deriving irmin ~pp ~equal]
    type node_key = H.t [@@deriving irmin]
    type contents_key = H.t [@@deriving irmin]

    type step = Node.step
    [@@deriving irmin ~compare ~to_bin_string ~of_bin_string]

    type metadata = Node.metadata [@@deriving irmin ~equal]

    let default = Node.default

    type value = Node.value [@@deriving irmin ~equal]
  end

  module StepMap = struct
    include Map.Make (struct
      type t = T.step

      let compare = T.compare_step
    end)

    let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
  end

  (* Binary representation, useful to compute hashes *)
  module Bin = struct
    open T

    type ptr = { index : int; hash : H.t } [@@deriving irmin]

    type tree = { depth : int; length : int; entries : ptr list }
    [@@deriving irmin]

    type v = Values of (step * value) list | Tree of tree
    [@@deriving irmin ~pre_hash]

    module V =
      Irmin.Hash.Typed
        (H)
        (struct
          type t = v

          let t = v_t
        end)

    type t = { hash : H.t Lazy.t; stable : bool; v : v }

    let t : t Irmin.Type.t =
      let open Irmin.Type in
      let pre_hash x = pre_hash_v x.v in
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
    type address = Indirect of int63 | Direct of H.t

    let address_t : address Irmin.Type.t =
      let open Irmin.Type in
      variant "Compress.address" (fun i d -> function
        | Indirect x -> i x | Direct x -> d x)
      |~ case1 "Indirect" int63_t (fun x -> Indirect x)
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

    let is_default = T.(equal_metadata default)

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
      |~ case1 "contents-ii" (pair int Int63.t) (fun (n, i) ->
             Contents (Indirect n, Indirect i, T.default))
      |~ case1 "contents-x-ii" (triple int int63_t metadata_t) (fun (n, i, m) ->
             Contents (Indirect n, Indirect i, m))
      |~ case1 "node-ii" (pair int Int63.t) (fun (n, i) ->
             Node (Indirect n, Indirect i))
      |~ case1 "contents-id" (pair int H.t) (fun (n, h) ->
             Contents (Indirect n, Direct h, T.default))
      |~ case1 "contents-x-id" (triple int H.t metadata_t) (fun (n, h, m) ->
             Contents (Indirect n, Direct h, m))
      |~ case1 "node-id" (pair int H.t) (fun (n, h) ->
             Node (Indirect n, Direct h))
      |~ case1 "contents-di" (pair step_t Int63.t) (fun (n, i) ->
             Contents (Direct n, Indirect i, T.default))
      |~ case1 "contents-x-di" (triple step_t int63_t metadata_t)
           (fun (n, i, m) -> Contents (Direct n, Indirect i, m))
      |~ case1 "node-di" (pair step_t Int63.t) (fun (n, i) ->
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
    let kind_node = Pack_value.Kind.Node
    let kind_inode = Pack_value.Kind.Inode
    let magic_node = Pack_value.Kind.to_magic kind_node
    let magic_inode = Pack_value.Kind.to_magic kind_inode

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

  (** [Val_impl] defines the recursive structure of inodes.

      {3 Inode Layout}

      {4 Layout Types}

      The layout ['a layout] associated to an inode ['a t] defines certain
      properties of the inode:

      - When [Total], the inode is self contained and immutable.
      - When [Partial], chunks of the inode might be missing but they can be
        fetched from the backend when needed using the available [find] function
        stored in the layout. Mutable pointers act as cache.
      - When [Truncated], chunks of the inode might be missing. Those chunks are
        unreachable because the pointer to the backend is missing. The inode is
        immutable.

      {4 Layout Instantiation}

      The layout of an inode is determined from the module [Val], it depends on
      the way the inode was constructed:

      - When [Total], it originates from [Val.v] or [Val.empty].
      - When [Partial], it originates from [Val.of_bin], which is only used by
        [Inode.find].
      - When [Truncated], it originates from an [Irmin.Type] deserialisation
        made possible by [Val.t].

      Almost all other functions in [Val_impl] are polymorphic regarding the
      layout of the manipulated inode.

      {4 Details on the [Truncated] Layout}

      The [Truncated] layout is identical to [Partial] except for the missing
      [find] function.

      On the one hand, when creating the root of a [Truncated] inode, the
      pointers to children inodes - if any - are set to the [Broken] tag,
      meaning that we know the hash to such children but we will have to way to
      load them in the future. On the other hand, when adding children to a
      [Truncated] inode, there is no such problem, the pointer is then set to
      the [Intact] tag.

      As of Irmin 2.4 (February 2021), inode deserialisation using Repr happens
      in [irmin/slice.ml] and [irmin/sync_ext.ml], and maybe some other places.

      At some point we might want to forbid such deserialisations and instead
      use something in the flavour of [Val.of_bin] to create [Partial] inodes.

      {3 Topmost Inode Ancestor}

      [Val_impl.t] is a recursive type, it is labelled with a [depth] integer
      that indicates the recursion depth. An inode with [depth = 0] corresponds
      to the root of a directory, its hash is the hash of the directory.

      A [Val.t] points to the topmost [Val_impl.t] of an inode tree. In most
      scenarios, that topmost inode has [depth = 0], but it is also legal for
      the topmost inode to be an intermediate inode, i.e. with [depth > 0].

      The only way for an inode tree to have an intermediate inode as root is to
      fetch it from the backend by calling [Make_ext.find], using the hash of
      that inode.

      Write-only operations are not permitted when the root is an intermediate
      inode. *)
  module Val_impl = struct
    open T

    type _ layout =
      | Total : total_ptr layout
      | Partial : (hash -> partial_ptr t option) -> partial_ptr layout
      | Truncated : truncated_ptr layout

    and partial_ptr_target =
      | Dirty of partial_ptr t
      | Lazy of hash
      | Lazy_loaded of partial_ptr t
          (** A partial pointer differentiates the [Dirty] and [Lazy_loaded]
              cases in order to remember that only the latter should be
              collected when [clear] is called.

              The child in [Lazy_loaded] can only emanate from the disk. It can
              be savely collected on [clear].

              The child in [Dirty] can only emanate from a user modification,
              e.g. through the [add] or [to_concrete] functions. It shouldn't be
              collected on [clear] because it will be needed for [save]. *)

    and partial_ptr = { mutable target : partial_ptr_target }

    and total_ptr = Total_ptr of total_ptr t [@@unboxed]

    and truncated_ptr = Broken of hash | Intact of truncated_ptr t

    and 'ptr tree = { depth : int; length : int; entries : 'ptr option array }

    and 'ptr v = Values of value StepMap.t | Tree of 'ptr tree

    and 'ptr t = { hash : hash Lazy.t; stable : bool; v : 'ptr v }

    module Ptr = struct
      let hash : type ptr. ptr layout -> ptr -> _ = function
        | Total -> fun (Total_ptr ptr) -> Lazy.force ptr.hash
        | Partial _ -> (
            fun { target } ->
              match target with
              | Lazy hash -> hash
              | Dirty { hash; _ } | Lazy_loaded { hash; _ } -> Lazy.force hash)
        | Truncated -> (
            function Broken h -> h | Intact ptr -> Lazy.force ptr.hash)

      let target : type ptr. cache:bool -> ptr layout -> ptr -> ptr t =
       fun ~cache layout ->
        match layout with
        | Total -> fun (Total_ptr t) -> t
        | Partial find -> (
            function
            | { target = Dirty entry } | { target = Lazy_loaded entry } ->
                (* [target] is already cached. [cache] is only concerned with
                   new cache entries, not the older ones for which the irmin
                   users can discard using [clear]. *)
                entry
            | { target = Lazy _ } as t -> (
                let h = hash layout t in
                match find h with
                | None -> Fmt.failwith "%a: unknown key" pp_hash h
                | Some x ->
                    if cache then t.target <- Lazy_loaded x;
                    x))
        | Truncated -> (
            function
            | Intact entry -> entry
            | _ ->
                failwith
                  "Impossible to load the subtree on an inode deserialized \
                   using Repr")

      let of_target : type ptr. ptr layout -> ptr t -> ptr = function
        | Total -> fun target -> Total_ptr target
        | Partial _ -> fun target -> { target = Dirty target }
        | Truncated -> fun target -> Intact target

      let of_hash : type ptr. ptr layout -> hash -> ptr = function
        | Total -> assert false
        | Partial _ -> fun hash -> { target = Lazy hash }
        | Truncated -> fun hash -> Broken hash

      let save :
          type ptr.
          broken:(hash -> unit) ->
          save_dirty:(ptr t -> unit) ->
          clear:bool ->
          ptr layout ->
          ptr ->
          unit =
       fun ~broken ~save_dirty ~clear -> function
        | Total -> fun (Total_ptr entry) -> (save_dirty [@tailcall]) entry
        | Partial _ -> (
            function
            | { target = Dirty entry } as box ->
                if clear then box.target <- Lazy (Lazy.force entry.hash)
                else
                  (* Promote from dirty to lazy as it will be saved during
                     [save_dirty]. *)
                  box.target <- Lazy_loaded entry;
                (save_dirty [@tailcall]) entry
            | { target = Lazy_loaded entry } as box ->
                if clear then box.target <- Lazy (Lazy.force entry.hash);
                (save_dirty [@tailcall]) entry
            | { target = Lazy _ } -> ())
        | Truncated -> (
            function
            | Broken h -> (broken [@tailcall]) h
            | Intact entry -> (save_dirty [@tailcall]) entry)

      let clear :
          type ptr.
          iter_dirty:(ptr layout -> ptr t -> unit) -> ptr layout -> ptr -> unit
          =
       fun ~iter_dirty layout ptr ->
        match layout with
        | Partial _ -> (
            match ptr with
            | { target = Lazy _ } -> ()
            | { target = Dirty ptr } -> iter_dirty layout ptr
            | { target = Lazy_loaded ptr } as box ->
                let hash = Lazy.force ptr.hash in
                (* Since a [Lazy_loaded] used to be a [Lazy], the hash is always
                   available. *)
                box.target <- Lazy hash)
        | Total | Truncated -> ()
    end

    let pred layout t =
      match t.v with
      | Tree i ->
          let hash_of_ptr = Ptr.hash layout in
          Array.fold_left
            (fun acc -> function
              | None -> acc
              | Some ptr -> (None, `Inode (hash_of_ptr ptr)) :: acc)
            [] i.entries
      | Values l ->
          StepMap.fold
            (fun s v acc ->
              let v =
                match v with
                | `Node _ as k -> (Some s, k)
                | `Contents (k, _) -> (Some s, `Contents k)
              in
              v :: acc)
            l []

    let length_of_v = function
      | Values vs -> StepMap.cardinal vs
      | Tree vs -> vs.length

    let length t = length_of_v t.v

    let rec clear layout t =
      match t.v with
      | Tree i ->
          Array.iter
            (Option.iter (Ptr.clear ~iter_dirty:clear layout))
            i.entries
      | Values _ -> ()

    let nb_children t =
      match t.v with
      | Tree i ->
          Array.fold_left
            (fun i -> function None -> i | Some _ -> i + 1)
            0 i.entries
      | Values vs -> StepMap.cardinal vs

    let stable t = t.stable

    type cont = off:int -> len:int -> (step * value) Seq.node

    let rec seq_tree layout bucket_seq ~cache : cont -> cont =
     fun k ~off ~len ->
      assert (off >= 0);
      assert (len > 0);
      match bucket_seq () with
      | Seq.Nil -> k ~off ~len
      | Seq.Cons (None, rest) -> seq_tree layout rest ~cache k ~off ~len
      | Seq.Cons (Some i, rest) ->
          let trg = Ptr.target ~cache layout i in
          let trg_len = length trg in
          if off - trg_len >= 0 then
            (* Skip a branch of the inode tree in case the user asked for a
               specific starting offset.

               Without this branch the algorithm would keep the same semantic
               because [seq_value] would handles the pagination value by value
               instead. *)
            let off = off - trg_len in
            seq_tree layout rest ~cache k ~off ~len
          else
            seq_v layout trg.v ~cache (seq_tree layout rest ~cache k) ~off ~len

    and seq_values layout value_seq : cont -> cont =
     fun k ~off ~len ->
      assert (off >= 0);
      assert (len > 0);
      match value_seq () with
      | Seq.Nil -> k ~off ~len
      | Cons (x, rest) ->
          if off = 0 then
            let len = len - 1 in
            if len = 0 then
              (* Yield the current value and skip the rest of the inode tree in
                 case the user asked for a specific length. *)
              Seq.Cons (x, Seq.empty)
            else Seq.Cons (x, fun () -> seq_values layout rest k ~off ~len)
          else
            (* Skip one value in case the user asked for a specific starting
               offset. *)
            let off = off - 1 in
            seq_values layout rest k ~off ~len

    and seq_v layout v ~cache : cont -> cont =
     fun k ~off ~len ->
      assert (off >= 0);
      assert (len > 0);
      match v with
      | Tree t -> seq_tree layout (Array.to_seq t.entries) ~cache k ~off ~len
      | Values vs -> seq_values layout (StepMap.to_seq vs) k ~off ~len

    let empty_continuation : cont = fun ~off:_ ~len:_ -> Seq.Nil

    let seq layout ?offset:(off = 0) ?length:(len = Int.max_int) ?(cache = true)
        t : (step * value) Seq.t =
      if off < 0 then invalid_arg "Invalid pagination offset";
      if len < 0 then invalid_arg "Invalid pagination length";
      if len = 0 then Seq.empty
      else fun () -> seq_v layout t.v ~cache empty_continuation ~off ~len

    let seq_tree layout ?(cache = true) i : (step * value) Seq.t =
      let off = 0 in
      let len = Int.max_int in
      fun () -> seq_v layout (Tree i) ~cache empty_continuation ~off ~len

    let seq_v layout ?(cache = true) v : (step * value) Seq.t =
      let off = 0 in
      let len = Int.max_int in
      fun () -> seq_v layout v ~cache empty_continuation ~off ~len

    let to_bin_v layout v =
      Stats.incr_inode_to_binv ();
      match v with
      | Values vs ->
          let vs = StepMap.bindings vs in
          Bin.Values vs
      | Tree t ->
          let hash_of_ptr = Ptr.hash layout in
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

    let to_bin layout t =
      let v = to_bin_v layout t.v in
      Bin.v ~stable:t.stable ~hash:t.hash v

    module Concrete = struct
      type kind = Contents | Contents_x of metadata | Node [@@deriving irmin]
      type entry = { name : step; kind : kind; hash : hash } [@@deriving irmin]

      type 'a pointer = { index : int; pointer : hash; tree : 'a }
      [@@deriving irmin]

      type 'a tree = { depth : int; length : int; pointers : 'a pointer list }
      [@@deriving irmin]

      type t = Tree of t tree | Value of entry list [@@deriving irmin]

      let to_entry (name, v) =
        match v with
        | `Contents (hash, m) ->
            if T.equal_metadata m Node.default then
              { name; kind = Contents; hash }
            else { name; kind = Contents_x m; hash }
        | `Node hash -> { name; kind = Node; hash }

      let of_entry e =
        ( e.name,
          match e.kind with
          | Contents -> `Contents (e.hash, Node.default)
          | Contents_x m -> `Contents (e.hash, m)
          | Node -> `Node e.hash )

      type error =
        [ `Invalid_hash of hash * hash * t
        | `Invalid_depth of int * int * t
        | `Invalid_length of int * int * t
        | `Duplicated_entries of t
        | `Duplicated_pointers of t
        | `Unsorted_entries of t
        | `Unsorted_pointers of t
        | `Empty ]
      [@@deriving irmin]

      let rec length = function
        | Value l -> List.length l
        | Tree t ->
            List.fold_left (fun acc p -> acc + length p.tree) 0 t.pointers

      let pp = Irmin.Type.pp_json t

      let pp_error ppf = function
        | `Invalid_hash (got, expected, t) ->
            Fmt.pf ppf "invalid hash for %a@,got: %a@,expecting: %a" pp t
              pp_hash got pp_hash expected
        | `Invalid_depth (got, expected, t) ->
            Fmt.pf ppf "invalid depth for %a@,got: %d@,expecting: %d" pp t got
              expected
        | `Invalid_length (got, expected, t) ->
            Fmt.pf ppf "invalid length for %a@,got: %d@,expecting: %d" pp t got
              expected
        | `Duplicated_entries t -> Fmt.pf ppf "duplicated entries: %a" pp t
        | `Duplicated_pointers t -> Fmt.pf ppf "duplicated pointers: %a" pp t
        | `Unsorted_entries t -> Fmt.pf ppf "entries should be sorted: %a" pp t
        | `Unsorted_pointers t ->
            Fmt.pf ppf "pointers should be sorted: %a" pp t
        | `Empty -> Fmt.pf ppf "concrete subtrees cannot be empty"
    end

    let to_concrete (la : 'ptr layout) (t : 'ptr t) =
      let rec aux t =
        match t.v with
        | Tree tr ->
            ( Lazy.force t.hash,
              Concrete.Tree
                {
                  depth = tr.depth;
                  length = tr.length;
                  pointers =
                    Array.fold_left
                      (fun (i, acc) e ->
                        match e with
                        | None -> (i + 1, acc)
                        | Some t ->
                            let pointer, tree =
                              aux (Ptr.target ~cache:true la t)
                            in
                            (i + 1, { Concrete.index = i; tree; pointer } :: acc))
                      (0, []) tr.entries
                    |> snd
                    |> List.rev;
                } )
        | Values l ->
            ( Lazy.force t.hash,
              Concrete.Value (List.map Concrete.to_entry (StepMap.bindings l))
            )
      in
      snd (aux t)

    exception Invalid_hash of hash * hash * Concrete.t
    exception Invalid_depth of int * int * Concrete.t
    exception Invalid_length of int * int * Concrete.t
    exception Empty
    exception Duplicated_entries of Concrete.t
    exception Duplicated_pointers of Concrete.t
    exception Unsorted_entries of Concrete.t
    exception Unsorted_pointers of Concrete.t

    let of_concrete_exn t =
      let sort_entries =
        List.sort_uniq (fun x y -> compare x.Concrete.name y.Concrete.name)
      in
      let sort_pointers =
        List.sort_uniq (fun x y -> compare x.Concrete.index y.Concrete.index)
      in
      let check_entries t es =
        if es = [] then raise Empty;
        let s = sort_entries es in
        if List.length s <> List.length es then raise (Duplicated_entries t);
        if s <> es then raise (Unsorted_entries t)
      in
      let check_pointers t ps =
        if ps = [] then raise Empty;
        let s = sort_pointers ps in
        if List.length s <> List.length ps then raise (Duplicated_pointers t);
        if s <> ps then raise (Unsorted_pointers t)
      in
      let hash v = Bin.V.hash (to_bin_v Total v) in
      let rec aux depth t =
        match t with
        | Concrete.Value l ->
            check_entries t l;
            Values (StepMap.of_list (List.map Concrete.of_entry l))
        | Concrete.Tree tr ->
            let entries = Array.make Conf.entries None in
            check_pointers t tr.pointers;
            List.iter
              (fun { Concrete.index; pointer; tree } ->
                let v = aux (depth + 1) tree in
                let hash = hash v in
                if not (equal_hash hash pointer) then
                  raise (Invalid_hash (hash, pointer, t));
                let t = { hash = lazy pointer; stable = false; v } in
                entries.(index) <- Some (Ptr.of_target Total t))
              tr.pointers;
            let length = Concrete.length t in
            if depth <> tr.depth then raise (Invalid_depth (depth, tr.depth, t));
            if length <> tr.length then
              raise (Invalid_length (length, tr.length, t));
            Tree { depth = tr.depth; length = tr.length; entries }
      in
      let v = aux 0 t in
      let length = length_of_v v in
      let stable, hash =
        if length > Conf.stable_hash then (false, hash v)
        else
          let node = Node.of_seq (seq_v Total v) in
          (true, Node.hash node)
      in
      { hash = lazy hash; stable; v }

    let of_concrete t =
      try Ok (of_concrete_exn t) with
      | Invalid_hash (x, y, z) -> Error (`Invalid_hash (x, y, z))
      | Invalid_depth (x, y, z) -> Error (`Invalid_depth (x, y, z))
      | Invalid_length (x, y, z) -> Error (`Invalid_length (x, y, z))
      | Empty -> Error `Empty
      | Duplicated_entries t -> Error (`Duplicated_entries t)
      | Duplicated_pointers t -> Error (`Duplicated_pointers t)
      | Unsorted_entries t -> Error (`Unsorted_entries t)
      | Unsorted_pointers t -> Error (`Unsorted_pointers t)

    let hash t = Lazy.force t.hash

    let is_root t =
      match t.v with
      | Tree { depth; _ } -> depth = 0
      | Values _ ->
          (* When [t] is of tag [Values], then [t] is root iff [t] is stable. It
             is implied by the following.

             When [t] is stable, then [t] is a root, because:
              - Only 2 functions produce stable inodes: [stabilize] and [empty].
              - Only the roots are output of [stabilize].
              - An empty map can only be located at the root.

             When [t] is a root of tag [Value], then [t] is stable, because:
             - All the roots are output of [stabilize].
             - When an unstable inode enters [stabilize], it becomes stable if
               it has at most [Conf.stable_hash] leaves.
             - A [Value] has at most [Conf.stable_hash] leaves because
               [Conf.entries <= Conf.stable_hash] is enforced.
          *)
          t.stable

    let check_write_op_supported t =
      if not @@ is_root t then
        failwith "Cannot perform operation on non-root inode value."

    let stabilize layout t =
      if t.stable then t
      else
        let n = length t in
        if n > Conf.stable_hash then t
        else
          let hash =
            lazy
              (let vs = seq layout t in
               Node.hash (Node.of_seq vs))
          in
          { hash; stable = true; v = t.v }

    let hash_key = Irmin.Type.(unstage (short_hash step_t))
    let index ~depth k = abs (hash_key ~seed:depth k) mod Conf.entries

    (** This function shouldn't be called with the [Total] layout. In the
        future, we could add a polymorphic variant to the GADT parameter to
        enfoce that. *)
    let of_bin layout t =
      let v =
        match t.Bin.v with
        | Bin.Values vs ->
            let vs = StepMap.of_list vs in
            Values vs
        | Tree t ->
            let entries = Array.make Conf.entries None in
            let ptr_of_hash = Ptr.of_hash layout in
            List.iter
              (fun { Bin.index; hash } ->
                entries.(index) <- Some (ptr_of_hash hash))
              t.entries;
            Tree { depth = t.Bin.depth; length = t.length; entries }
      in
      { hash = t.Bin.hash; stable = t.Bin.stable; v }

    let empty : 'a. 'a layout -> 'a t =
     fun _ ->
      let hash = lazy (Node.hash Node.empty) in
      { stable = true; hash; v = Values StepMap.empty }

    let values layout vs =
      let length = StepMap.cardinal vs in
      if length = 0 then empty layout
      else
        let v = Values vs in
        let hash = lazy (Bin.V.hash (to_bin_v layout v)) in
        { hash; stable = false; v }

    let tree layout is =
      let v = Tree is in
      let hash = lazy (Bin.V.hash (to_bin_v layout v)) in
      { hash; stable = false; v }

    let is_empty t =
      match t.v with Values vs -> StepMap.is_empty vs | Tree _ -> false

    let find_value ~cache layout ~depth t s =
      let target_of_ptr = Ptr.target ~cache layout in
      let rec aux ~depth = function
        | Values vs -> ( try Some (StepMap.find s vs) with Not_found -> None)
        | Tree t -> (
            let i = index ~depth s in
            let x = t.entries.(i) in
            match x with
            | None -> None
            | Some i -> aux ~depth:(depth + 1) (target_of_ptr i).v)
      in
      aux ~depth t.v

    let find ?(cache = true) layout t s = find_value ~cache ~depth:0 layout t s

    let rec add layout ~depth ~copy ~replace t s v k =
      Stats.incr_inode_rec_add ();
      match t.v with
      | Values vs ->
          let length =
            if replace then StepMap.cardinal vs else StepMap.cardinal vs + 1
          in
          let t =
            if length <= Conf.entries then values layout (StepMap.add s v vs)
            else
              let vs = StepMap.bindings (StepMap.add s v vs) in
              let empty =
                tree layout
                  { length = 0; depth; entries = Array.make Conf.entries None }
              in
              let aux t (s, v) =
                (add [@tailcall]) layout ~depth ~copy:false ~replace t s v
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
              let target = values layout (StepMap.singleton s v) in
              entries.(i) <- Some (Ptr.of_target layout target);
              let t = tree layout { depth; length; entries } in
              k t
          | Some n ->
              let t =
                (* [cache] is unimportant here as we've already called
                   [find_value] for that path.*)
                Ptr.target ~cache:true layout n
              in
              (add [@tailcall]) layout ~depth:(depth + 1) ~copy ~replace t s v
                (fun target ->
                  entries.(i) <- Some (Ptr.of_target layout target);
                  let t = tree layout { depth; length; entries } in
                  k t))

    let add layout ~copy t s v =
      (* XXX: [find_value ~depth:42] should break the unit tests. It doesn't. *)
      match find_value ~cache:true ~depth:0 layout t s with
      | Some v' when equal_value v v' -> stabilize layout t
      | Some _ ->
          add ~depth:0 layout ~copy ~replace:true t s v Fun.id
          |> stabilize layout
      | None ->
          add ~depth:0 layout ~copy ~replace:false t s v Fun.id
          |> stabilize layout

    let rec remove layout ~depth t s k =
      Stats.incr_inode_rec_remove ();
      match t.v with
      | Values vs ->
          let t = values layout (StepMap.remove s vs) in
          k t
      | Tree t -> (
          let len = t.length - 1 in
          if len <= Conf.entries then
            let vs = seq_tree layout t in
            let vs = StepMap.of_seq vs in
            let vs = StepMap.remove s vs in
            let t = values layout vs in
            k t
          else
            let entries = Array.copy t.entries in
            let i = index ~depth s in
            match entries.(i) with
            | None -> assert false
            | Some t ->
                let t =
                  (* [cache] is unimportant here as we've already called
                     [find_value] for that path.*)
                  Ptr.target ~cache:true layout t
                in
                if length t = 1 then (
                  entries.(i) <- None;
                  let t = tree layout { depth; length = len; entries } in
                  k t)
                else
                  remove ~depth:(depth + 1) layout t s @@ fun target ->
                  entries.(i) <- Some (Ptr.of_target layout target);
                  let t = tree layout { depth; length = len; entries } in
                  k t)

    let remove layout t s =
      (* XXX: [find_value ~depth:42] should break the unit tests. It doesn't. *)
      match find_value ~cache:true layout ~depth:0 t s with
      | None -> stabilize layout t
      | Some _ -> remove layout ~depth:0 t s Fun.id |> stabilize layout

    let of_seq l =
      let t =
        let rec aux_big seq inode =
          match seq () with
          | Seq.Nil -> inode
          | Seq.Cons ((s, v), rest) ->
              aux_big rest (add Total ~copy:false inode s v)
        in
        let len =
          (* [StepMap.cardinal] is (a bit) expensive to compute, let's track the
             size of the map in a [ref] while doing [StepMap.update]. *)
          ref 0
        in
        let rec aux_small seq map =
          match seq () with
          | Seq.Nil ->
              assert (!len <= Conf.entries);
              values Total map
          | Seq.Cons ((s, v), rest) ->
              let map =
                StepMap.update s
                  (function
                    | None ->
                        incr len;
                        Some v
                    | Some _ -> Some v)
                  map
              in
              if !len = Conf.entries then aux_big rest (values Total map)
              else aux_small rest map
        in
        aux_small l StepMap.empty
      in
      stabilize Total t

    let save layout ~add ~mem t =
      let clear =
        (* When set to [true], collect the loaded inodes as soon as they're
           saved.

           This parameter is not exposed yet. Ideally it would be exposed and
           be forwarded from [Tree.export ?clear] through [P.Node.add].

           It is currently set to true in order to preserve behaviour *)
        false
      in
      let iter_entries =
        let broken h =
          (* This function is called when we encounter a Broken pointer with
             Truncated layouts. *)
          if not @@ mem h then
            Fmt.failwith
              "You are trying to save to the backend an inode deserialized \
               using [Irmin.Type] that used to contain pointer(s) to inodes \
               which are unknown to the backend. Hash: %a"
              pp_hash h
          else
            (* The backend already knows this target inode, there is no need to
               traverse further down. This happens during the unit tests. *)
            ()
        in
        fun save_dirty arr ->
          let iter_ptr = Ptr.save ~broken ~save_dirty ~clear layout in
          Array.iter (Option.iter iter_ptr) arr
      in
      let rec aux ~depth t =
        [%log.debug "save depth:%d" depth];
        match t.v with
        | Values _ -> add (Lazy.force t.hash) (to_bin layout t)
        | Tree n ->
            iter_entries
              (fun t ->
                let hash = Lazy.force t.hash in
                if mem hash then () else aux ~depth:(depth + 1) t)
              n.entries;
            add (Lazy.force t.hash) (to_bin layout t)
      in
      aux ~depth:0 t

    let check_stable layout t =
      let target_of_ptr = Ptr.target ~cache:true layout in
      let rec check t any_stable_ancestor =
        let stable = t.stable || any_stable_ancestor in
        match t.v with
        | Values _ -> true
        | Tree tree ->
            Array.for_all
              (function
                | None -> true
                | Some t ->
                    let t = target_of_ptr t in
                    (if stable then not t.stable else true) && check t stable)
              tree.entries
      in
      check t t.stable

    let contains_empty_map layout t =
      let target_of_ptr = Ptr.target ~cache:true layout in
      let rec check_lower t =
        match t.v with
        | Values l when StepMap.is_empty l -> true
        | Values _ -> false
        | Tree inodes ->
            Array.exists
              (function
                | None -> false | Some t -> target_of_ptr t |> check_lower)
              inodes.entries
      in
      check_lower t

    let is_tree t = match t.v with Tree _ -> true | Values _ -> false
  end

  module Raw = struct
    type hash = H.t
    type t = Bin.t

    let t = Bin.t

    let kind (t : t) =
      if t.stable then Compress.kind_node else Compress.kind_inode

    let hash t = Bin.hash t
    let step_to_bin = T.step_to_bin_string
    let step_of_bin = T.step_of_bin_string
    let encode_compress = Irmin.Type.(unstage (encode_bin Compress.t))
    let decode_compress = Irmin.Type.(unstage (decode_bin Compress.t))

    let decode_compress_length =
      match Irmin.Type.Size.of_encoding Compress.t with
      | Unknown | Static _ -> assert false
      | Dynamic f -> f

    let encode_bin ~dict ~offset (t : t) k =
      Stats.incr_inode_encode_bin ();
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

    let decode_bin ~dict ~hash t off : int * t =
      Stats.incr_inode_decode_bin ();
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

    let decode_bin_length = decode_compress_length
  end

  type hash = T.hash

  let pp_hash = T.pp_hash

  module Val = struct
    include T
    module I = Val_impl

    type t =
      | Total of I.total_ptr I.t
      | Partial of I.partial_ptr I.layout * I.partial_ptr I.t
      | Truncated of I.truncated_ptr I.t

    type 'b apply_fn = { f : 'a. 'a I.layout -> 'a I.t -> 'b } [@@unboxed]

    let apply : t -> 'b apply_fn -> 'b =
     fun t f ->
      match t with
      | Total v -> f.f I.Total v
      | Partial (layout, v) -> f.f layout v
      | Truncated v -> f.f I.Truncated v

    type map_fn = { f : 'a. 'a I.layout -> 'a I.t -> 'a I.t } [@@unboxed]

    let map : t -> map_fn -> t =
     fun t f ->
      match t with
      | Total v ->
          let v' = f.f I.Total v in
          if v == v' then t else Total v'
      | Partial (layout, v) ->
          let v' = f.f layout v in
          if v == v' then t else Partial (layout, v')
      | Truncated v ->
          let v' = f.f I.Truncated v in
          if v == v' then t else Truncated v'

    let pred t = apply t { f = (fun layout v -> I.pred layout v) }

    let of_seq l =
      Stats.incr_inode_of_seq ();
      Total (I.of_seq l)

    let of_list l = of_seq (List.to_seq l)

    let seq ?offset ?length ?cache t =
      apply t { f = (fun layout v -> I.seq layout ?offset ?length ?cache v) }

    let list ?offset ?length ?cache t =
      List.of_seq (seq ?offset ?length ?cache t)

    let empty = of_list []
    let is_empty t = apply t { f = (fun _ v -> I.is_empty v) }

    let find ?cache t s =
      apply t { f = (fun layout v -> I.find ?cache layout v s) }

    let add t s value =
      Stats.incr_inode_add ();
      let f layout v =
        I.check_write_op_supported v;
        I.add ~copy:true layout v s value
      in
      map t { f }

    let remove t s =
      Stats.incr_inode_remove ();
      let f layout v =
        I.check_write_op_supported v;
        I.remove layout v s
      in
      map t { f }

    let pre_hash_binv = Irmin.Type.(unstage (pre_hash Bin.v_t))
    let pre_hash_node = Irmin.Type.(unstage (pre_hash Node.t))

    let t : t Irmin.Type.t =
      let pre_hash x =
        let stable = apply x { f = (fun _ v -> I.stable v) } in
        if not stable then
          let bin = apply x { f = (fun layout v -> I.to_bin layout v) } in
          pre_hash_binv bin.v
        else
          let vs = seq x in
          pre_hash_node (Node.of_seq vs)
      in
      Irmin.Type.map ~pre_hash Bin.t
        (fun bin -> Truncated (I.of_bin I.Truncated bin))
        (fun x -> apply x { f = (fun layout v -> I.to_bin layout v) })

    let hash t = apply t { f = (fun _ v -> I.hash v) }

    let save ~add ~mem t =
      let f layout v =
        I.check_write_op_supported v;
        I.save layout ~add ~mem v
      in
      apply t { f }

    let of_raw find' v =
      Stats.incr_inode_of_raw ();
      let rec find h =
        match find' h with None -> None | Some v -> Some (I.of_bin layout v)
      and layout = I.Partial find in
      Partial (layout, I.of_bin layout v)

    let to_raw t = apply t { f = (fun layout v -> I.to_bin layout v) }
    let stable t = apply t { f = (fun _ v -> I.stable v) }
    let length t = apply t { f = (fun _ v -> I.length v) }
    let clear t = apply t { f = (fun layout v -> I.clear layout v) }
    let nb_children t = apply t { f = (fun _ v -> I.nb_children v) }
    let index = I.index

    let integrity_check t =
      let f layout v =
        let check_stable () =
          let check () = I.check_stable layout v in
          let n = length t in
          if n > Conf.stable_hash then (not (stable t)) && check ()
          else stable t && check ()
        in
        let contains_empty_map_non_root () =
          let check () = I.contains_empty_map layout v in
          (* we are only looking for empty maps that are not at the root *)
          if I.is_tree v then check () else false
        in
        check_stable () && not (contains_empty_map_non_root ())
      in
      apply t { f }

    module Concrete = I.Concrete

    module Portable = struct
      type nonrec t = t [@@deriving irmin]
      type nonrec hash = hash
      type nonrec value = value

      let of_node t = t
      let of_seq = of_seq
      let add = add
      let list = list
      let length = length
      let find = find
      let remove = remove
    end

    let to_concrete t = apply t { f = (fun la v -> I.to_concrete la v) }

    let of_concrete t =
      match I.of_concrete t with Ok t -> Ok (Total t) | Error _ as e -> e

    let merge ~contents ~node : t Irmin.Merge.t =
      let merge = Node.merge ~contents ~node in
      let to_node t = of_seq (Node.seq t) in
      let of_node n = Node.of_seq (seq n) in
      Irmin.Merge.like t merge of_node to_node
  end
end

module Make
    (H : Irmin.Hash.S)
    (Node : Irmin.Node.S with type hash = H.t)
    (Inter : Internal
               with type hash = H.t
                and type Val.metadata = Node.metadata
                and type Val.step = Node.step)
    (Pack : Content_addressable.S
              with type key = H.t
               and type value = Inter.Raw.t) =
struct
  module Hash = H
  module Key = Irmin.Key.Of_hash (H)
  module Val = Inter.Val

  type 'a t = 'a Pack.t
  type key = Key.t
  type hash = Hash.t
  type value = Inter.Val.t

  let mem t k = Pack.mem t k
  let index _ k = Lwt.return_some k

  let find t k =
    Pack.find t k >|= function
    | None -> None
    | Some v ->
        let find = Pack.unsafe_find ~check_integrity:true t in
        let v = Val.of_raw find v in
        Some v

  let save t v =
    let add k v =
      Pack.unsafe_append ~ensure_unique:true ~overcommit:false t k v
    in
    Val.save ~add ~mem:(Pack.unsafe_mem t) v

  let hash v = Val.hash v

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
    Lwt.return k

  let batch = Pack.batch
  let close = Pack.close
  let clear = Pack.clear
  let decode_bin_length = Inter.Raw.decode_bin_length

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

module Make_persistent
    (H : Irmin.Hash.S)
    (Node : Irmin.Node.S with type hash = H.t)
    (Inter : Internal
               with type hash = H.t
                and type Val.metadata = Node.metadata
                and type Val.step = Node.step)
    (CA : Pack_store.Maker
            with type key = H.t
             and type index = Pack_index.Make(H).t) =
struct
  module Persistent_pack = CA.Make (Inter.Raw)
  module Pack = Persistent_pack
  include Make (H) (Node) (Inter) (Pack)

  type index = Pack.index

  let v = Pack.v
  let sync = Pack.sync
  let integrity_check = Pack.integrity_check
  let clear_caches = Pack.clear_caches
end
