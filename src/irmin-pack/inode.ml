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
    (H : Irmin.Hash.S) (Key : sig
      include Irmin.Key.S with type hash = H.t

      val unfindable_of_hash : hash -> t
    end)
    (Node : Irmin.Node.Generic_key.S
              with type hash = H.t
               and type contents_key = Key.t
               and type node_key = Key.t) =
struct
  (** If [should_be_stable ~length ~root] is true for an inode [i], then [i]
      hashes the same way as a [Node.t] containing the same entries. *)
  let should_be_stable ~length ~root =
    if length = 0 then true
    else if not root then false
    else if length <= Conf.stable_hash then true
    else false

  module Node = struct
    include Node
    module H = Irmin.Hash.Typed (H) (Node)

    let hash = H.hash
  end

  module T = struct
    type hash = H.t [@@deriving irmin ~pp ~equal]
    type key = Key.t [@@deriving irmin ~pp ~equal]
    type node_key = Node.node_key [@@deriving irmin]
    type contents_key = Node.contents_key [@@deriving irmin]

    type step = Node.step
    [@@deriving irmin ~compare ~to_bin_string ~of_bin_string]

    let short_hash_step =
      let pre_hash =
        Irmin.Type.(unstage (pre_hash_unboxed_primitives step_t))
      in
      fun ?seed x ->
        let seed = match seed with None -> 0 | Some t -> t in
        let h = ref seed in
        pre_hash x (fun s -> h := Hashtbl.seeded_hash !h s);
        !h

    type metadata = Node.metadata [@@deriving irmin ~equal]
    type value = Node.value [@@deriving irmin ~equal]

    module Metadata = Node.Metadata
  end

  module StepMap = struct
    include Map.Make (struct
      type t = T.step

      let compare = T.compare_step
    end)

    let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
  end

  module Val_ref : sig
    open T

    type t [@@deriving irmin]
    type v = private Key of Key.t | Hash of hash Lazy.t

    val inspect : t -> v
    val of_key : key -> t
    val of_hash : hash Lazy.t -> t
    val promote_exn : t -> key -> unit
    val to_hash : t -> hash
    val to_key : t -> key option
    val to_key_exn : t -> key
  end = struct
    open T

    (** Nodes that have been persisted to an underlying store are referenced via
        keys. Otherwise, when building in-memory inodes (e.g. via [Portable] or
        [of_concrete_exn]) lazily-computed hashes are used instead. If such
        values are persisted, the hash reference can be promoted to a key
        reference (but [Key] values are never demoted to hashes). *)
    type v = Key of Key.t | Hash of hash Lazy.t [@@deriving irmin ~pp_dump]

    type t = v ref

    let inspect t = !t
    let of_key k = ref (Key k)
    let of_hash h = ref (Hash h)

    let promote_exn t k =
      let existing_hash =
        match !t with
        | Key k' ->
            (* NOTE: it's valid for [k'] to not be strictly equal to [k], because
               of duplicate objects in the store. In this case, we preferentially
               take the newer key. *)
            Key.to_hash k'
        | Hash h -> Lazy.force h
      in
      if not (equal_hash existing_hash (Key.to_hash k)) then
        Fmt.failwith
          "Attempted to promote existing reference %a to an inconsistent key %a"
          pp_dump_v !t pp_key k;
      t := Key k

    let to_hash t =
      match !t with Hash h -> Lazy.force h | Key k -> Key.to_hash k

    let to_key t = match !t with Key k -> Some k | Hash _ -> None

    let to_key_exn t =
      match !t with
      | Key k -> k
      | Hash h ->
          Fmt.failwith "Encountered unkeyed hash but expected key: %a" pp_hash
            (Lazy.force h)

    let t =
      let pre_hash_hash = Irmin.Type.(unstage (pre_hash hash_t)) in
      let pre_hash x f =
        match !x with
        | Key k -> pre_hash_hash (Key.to_hash k) f
        | Hash h -> pre_hash_hash (Lazy.force h) f
      in
      Irmin.Type.map ~pre_hash v_t (fun x -> ref x) (fun x -> !x)
  end

  (* Binary representation. Used in two modes:

      - with [key]s as pointers to child values, when encoding values to add
        to the underlying store (or decoding values read from the store) –
        interoperable with the [Compress]-ed binary representation.

      - with either [key]s or [hash]es as pointers to child values, when
        pre-computing the hash of a node with children that haven't yet been
        written to the store. *)
  module Bin = struct
    open T

    (** Distinguishes between the two possible modes of binary value. *)
    type 'vref mode = Ptr_key : key mode | Ptr_any : Val_ref.t mode

    type 'vref with_index = { index : int; vref : 'vref } [@@deriving irmin]

    type 'vref tree = {
      depth : int;
      length : int;
      entries : 'vref with_index list;
    }
    [@@deriving irmin]

    type 'vref v = Values of (step * value) list | Tree of 'vref tree
    [@@deriving irmin ~pre_hash]

    module V =
      Irmin.Hash.Typed
        (H)
        (struct
          type t = Val_ref.t v [@@deriving irmin]
        end)

    type 'vref t = { hash : H.t Lazy.t; root : bool; v : 'vref v }

    let t : type vref. vref Irmin.Type.t -> vref t Irmin.Type.t =
     fun vref_t ->
      let open Irmin.Type in
      let v_t = v_t vref_t in
      let pre_hash_v = pre_hash_v vref_t in
      let pre_hash x = pre_hash_v x.v in
      record "Bin.t" (fun hash root v -> { hash = lazy hash; root; v })
      |+ field "hash" H.t (fun t -> Lazy.force t.hash)
      |+ field "root" bool (fun t -> t.root)
      |+ field "v" v_t (fun t -> t.v)
      |> sealr
      |> like ~pre_hash

    let v ~hash ~root v = { hash; root; v }
    let hash t = Lazy.force t.hash
  end

  (* Compressed binary representation *)
  module Compress = struct
    open T

    type name = Indirect of int | Direct of step

    type address =
      | Indirect of int63
      (* TODO: remove [Indirect] case: we're already doing this now. *)
      | Direct of key

    let address_t : address Irmin.Type.t =
      let open Irmin.Type in
      variant "Compress.address" (fun i d -> function
        | Indirect x -> i x | Direct x -> d x)
      |~ case1 "Indirect" int63_t (fun x -> Indirect x)
      |~ case1 "Direct" Key.t (fun x -> Direct x)
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

    let is_default = T.(equal_metadata Metadata.default)

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
             Contents (Indirect n, Indirect i, Metadata.default))
      |~ case1 "contents-x-ii" (triple int int63_t metadata_t) (fun (n, i, m) ->
             Contents (Indirect n, Indirect i, m))
      |~ case1 "node-ii" (pair int Int63.t) (fun (n, i) ->
             Node (Indirect n, Indirect i))
      |~ case1 "contents-id" (pair int Key.t) (fun (n, h) ->
             Contents (Indirect n, Direct h, Metadata.default))
      |~ case1 "contents-x-id" (triple int Key.t metadata_t) (fun (n, h, m) ->
             Contents (Indirect n, Direct h, m))
      |~ case1 "node-id" (pair int Key.t) (fun (n, h) ->
             Node (Indirect n, Direct h))
      |~ case1 "contents-di" (pair step_t Int63.t) (fun (n, i) ->
             Contents (Direct n, Indirect i, Metadata.default))
      |~ case1 "contents-x-di" (triple step_t int63_t metadata_t)
           (fun (n, i, m) -> Contents (Direct n, Indirect i, m))
      |~ case1 "node-di" (pair step_t Int63.t) (fun (n, i) ->
             Node (Direct n, Indirect i))
      |~ case1 "contents-dd" (pair step_t Key.t) (fun (n, i) ->
             Contents (Direct n, Direct i, Metadata.default))
      |~ case1 "contents-x-dd" (triple step_t Key.t metadata_t)
           (fun (n, i, m) -> Contents (Direct n, Direct i, m))
      |~ case1 "node-dd" (pair step_t Key.t) (fun (n, i) ->
             Node (Direct n, Direct i))
      |> sealv

    type v = Values of value list | Tree of tree
    [@@deriving irmin ~encode_bin ~decode_bin ~size_of]

    let dynamic_size_of_v_encoding =
      match Irmin.Type.Size.of_encoding v_t with
      | Irmin.Type.Size.Dynamic f -> f
      | _ -> assert false

    let dynamic_size_of_v =
      match Irmin.Type.Size.of_value v_t with
      | Irmin.Type.Size.Dynamic f -> f
      | _ -> assert false

    type kind = Pack_value.Kind.t
    [@@deriving irmin ~encode_bin ~decode_bin ~size_of]

    type nonrec int32 = int32
    [@@deriving irmin ~encode_bin ~decode_bin ~size_of]

    let no_length = Int32.zero
    let is_real_length length = not (Int32.equal length no_length)

    type v1 = { mutable length : int32; v : v } [@@deriving irmin]
    (** [length] is the length of the binary encoding of [v]. It is not known
        right away. [length] is [no_length] when it isn't known. Calling
        [encode_bin] or [size_of] will make [length] known. *)

    (** [tagged_v] sits between [v] and [t]. It is a variant with the header
        binary encoded as the magic. *)
    type tagged_v =
      | V0_stable of v
      | V0_unstable of v
      | V1_root of v1
      | V1_nonroot of v1
    [@@deriving irmin]

    let encode_bin_tv_staggered ({ v; _ } as tv) kind f =
      (* We need to write [length] before [v], but we will know [length]
         after [v] is encoded. The solution is to first encode [v], then write
         [length] and then write [v]. *)
      let l = ref [] in
      encode_bin_v v (fun s -> l := s :: !l);
      let length =
        List.fold_left (fun acc s -> acc + String.length s) 0 !l |> Int32.of_int
      in
      tv.length <- length;
      encode_bin_kind kind f;
      encode_bin_int32 length f;
      List.iter f (List.rev !l)

    let encode_bin_tv tv f =
      match tv with
      | V0_stable _ -> assert false
      | V0_unstable _ -> assert false
      | V1_root { length; v } when is_real_length length ->
          encode_bin_kind Pack_value.Kind.Inode_v1_root f;
          encode_bin_int32 length f;
          encode_bin_v v f
      | V1_nonroot { length; v } when is_real_length length ->
          encode_bin_kind Pack_value.Kind.Inode_v1_nonroot f;
          encode_bin_int32 length f;
          encode_bin_v v f
      | V1_root tv -> encode_bin_tv_staggered tv Pack_value.Kind.Inode_v1_root f
      | V1_nonroot tv ->
          encode_bin_tv_staggered tv Pack_value.Kind.Inode_v1_nonroot f

    let decode_bin_tv s off =
      let kind = decode_bin_kind s off in
      match kind with
      | Pack_value.Kind.Inode_v0_unstable ->
          let v = decode_bin_v s off in
          V0_unstable v
      | Inode_v0_stable ->
          let v = decode_bin_v s off in
          V0_stable v
      | Inode_v1_root ->
          let length = decode_bin_int32 s off in
          assert (is_real_length length);
          let v = decode_bin_v s off in
          V1_root { length; v }
      | Inode_v1_nonroot ->
          let length = decode_bin_int32 s off in
          assert (is_real_length length);
          let v = decode_bin_v s off in
          V1_nonroot { length; v }
      | Commit_v0 | Commit_v1 -> assert false
      | Contents -> assert false

    let size_of_tv =
      let of_value tv =
        match tv with
        | V0_stable v | V0_unstable v -> 1 + dynamic_size_of_v v
        | (V1_root { length; _ } | V1_nonroot { length; _ })
          when is_real_length length ->
            Int32.to_int length - H.hash_size
        | V1_root ({ v; _ } as tv) ->
            let length = H.hash_size + 1 + 4 + dynamic_size_of_v v in
            tv.length <- Int32.of_int length;
            length - H.hash_size
        | V1_nonroot ({ v; _ } as tv) ->
            let length = H.hash_size + 1 + 4 + dynamic_size_of_v v in
            tv.length <- Int32.of_int length;
            length - H.hash_size
      in
      let of_encoding s off =
        let offref = ref off in
        let kind = decode_bin_kind s offref in
        match kind with
        | Pack_value.Kind.Inode_v0_unstable | Inode_v0_stable ->
            1 + dynamic_size_of_v_encoding s !offref
        | Inode_v1_root | Inode_v1_nonroot ->
            let len = decode_bin_int32 s offref in
            Int32.to_int len - H.hash_size
        | Commit_v0 | Commit_v1 | Contents -> assert false
      in
      Irmin.Type.Size.custom_dynamic ~of_value ~of_encoding ()

    let tagged_v_t =
      Irmin.Type.like ~bin:(encode_bin_tv, decode_bin_tv, size_of_tv) tagged_v_t

    type t = { hash : H.t; tv : tagged_v }

    let v ~root ~hash v =
      let length = no_length in
      let tv =
        if root then V1_root { v; length } else V1_nonroot { v; length }
      in
      { hash; tv }

    (** The rule to determine the [is_root] property of a v0 [Value] is a bit
        convoluted, it relies on the fact that back then the following property
        was enforced: [Conf.stable_hash > Conf.entries].

        When [t] is of tag [Values], then [t] is root iff [t] is stable.

        When [t] is stable, then [t] is a root, because:

        - Only 2 functions produce stable inodes: [stabilize] and [empty].
        - Only the roots are output of [stabilize].
        - An empty map can only be located at the root.

        When [t] is a root of tag [Value], then [t] is stable, because:

        - All the roots are output of [stabilize].
        - When an unstable inode enters [stabilize], it becomes stable if it has
          at most [Conf.stable_hash] leaves.
        - A [Value] has at most [Conf.stable_hash] leaves because
          [Conf.entries <= Conf.stable_hash] is enforced. *)
    let is_root = function
      | { tv = V0_stable (Values _); _ } -> true
      | { tv = V0_unstable (Values _); _ } -> false
      | { tv = V0_stable (Tree { depth; _ }); _ }
      | { tv = V0_unstable (Tree { depth; _ }); _ } ->
          depth = 0
      | { tv = V1_root _; _ } -> true
      | { tv = V1_nonroot _; _ } -> false

    let t =
      let open Irmin.Type in
      record "Compress.t" (fun hash tv -> { hash; tv })
      |+ field "hash" H.t (fun t -> t.hash)
      |+ field "tagged_v" tagged_v_t (fun t -> t.tv)
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
      | Partial : (key -> partial_ptr t option) -> partial_ptr layout
      | Truncated : truncated_ptr layout

    and partial_ptr_target =
      | Dirty of partial_ptr t
      | Lazy of key
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

    and truncated_ptr =
      | Broken of Val_ref.t
          (** Initially [Hash.t], then set to [Key.t] when we try to save the
              parent and successfully index the hash. *)
      | Intact of truncated_ptr t

    and 'ptr tree = { depth : int; length : int; entries : 'ptr option array }

    and 'ptr v = Values of value StepMap.t | Tree of 'ptr tree

    and 'ptr t = {
      root : bool;
      v : 'ptr v;
      v_ref : Val_ref.t;
          (** Represents what is known about [v]'s presence in a corresponding
              store. Will be a [hash] if [v] is purely in-memory, and a [key] if
              [v] has been written to / loaded from a store.

              TODO: consider reflecting the internal state of [Val_ref.t] as a
              type parameter of [t]. *)
    }

    module Ptr = struct
      let val_ref : type ptr. ptr layout -> ptr -> Val_ref.t = function
        | Total -> fun (Total_ptr ptr) -> ptr.v_ref
        | Partial _ -> (
            fun { target } ->
              match target with
              | Lazy key -> Val_ref.of_key key
              | Lazy_loaded { v_ref; _ } | Dirty { v_ref; _ } -> v_ref)
        | Truncated -> ( function Broken v -> v | Intact ptr -> ptr.v_ref)

      let key_exn : type ptr. ptr layout -> ptr -> key = function
        | Total -> fun (Total_ptr ptr) -> Val_ref.to_key_exn ptr.v_ref
        | Partial _ -> (
            fun { target } ->
              match target with
              | Lazy key -> key
              | Lazy_loaded { v_ref; _ } | Dirty { v_ref; _ } ->
                  Val_ref.to_key_exn v_ref)
        | Truncated -> (
            function
            | Broken h -> Val_ref.to_key_exn h
            | Intact ptr -> Val_ref.to_key_exn ptr.v_ref)

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
            | { target = Lazy key } as t -> (
                match find key with
                | None -> Fmt.failwith "%a: unknown key" pp_key key
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

      let of_key : type ptr. ptr layout -> key -> ptr = function
        | Total -> assert false
        | Partial _ -> fun key -> { target = Lazy key }
        | Truncated -> fun key -> Broken (Val_ref.of_key key)

      type ('input, 'output) cps = { f : 'r. 'input -> ('output -> 'r) -> 'r }
      [@@ocaml.unboxed]

      let save :
          type ptr.
          broken:(hash -> key) ->
          save_dirty:(ptr t, key) cps ->
          clear:bool ->
          ptr layout ->
          ptr ->
          unit =
       fun ~broken ~save_dirty ~clear -> function
        (* Invariant: TODO (after return, we can get the key) *)
        | Total ->
            fun (Total_ptr entry) ->
              save_dirty.f entry (fun key ->
                  Val_ref.promote_exn entry.v_ref key)
        | Partial _ -> (
            function
            | { target = Dirty entry } as box ->
                save_dirty.f entry (fun key ->
                    if clear then box.target <- Lazy key
                    else (
                      box.target <- Lazy_loaded entry;
                      Val_ref.promote_exn entry.v_ref key))
            | { target = Lazy_loaded entry } as box ->
                (* TODO: Why do we save here anyway? *)
                save_dirty.f entry (fun key ->
                    if clear then box.target <- Lazy key)
            | { target = Lazy _ } -> ())
        | Truncated -> (
            function
            | Intact entry ->
                save_dirty.f entry (fun key ->
                    Val_ref.promote_exn entry.v_ref key)
            | Broken vref -> (
                (* TODO: add this to val_ref *)
                match Val_ref.to_key vref with
                | Some _ -> ()
                | None ->
                    let key =
                      broken (* TODO: make tailcall? *) (Val_ref.to_hash vref)
                    in
                    Val_ref.promote_exn vref key))

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
                (* Since a [Lazy_loaded] used to be a [Lazy], the key is always
                   available. *)
                let key = Val_ref.to_key_exn ptr.v_ref in
                box.target <- Lazy key)
        | Total | Truncated -> ()
    end

    let pred layout t =
      match t.v with
      | Tree i ->
          let key_of_ptr = Ptr.key_exn layout in
          Array.fold_left
            (fun acc -> function
              | None -> acc
              | Some ptr -> (None, `Inode (key_of_ptr ptr)) :: acc)
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

    let to_bin_v :
        type ptr vref. ptr layout -> vref Bin.mode -> ptr v -> vref Bin.v =
     fun layout mode node ->
      Stats.incr_inode_to_binv ();
      match node with
      | Values vs ->
          let vs = StepMap.bindings vs in
          Bin.Values vs
      | Tree t ->
          let vref_of_ptr : ptr -> vref =
            match mode with
            | Bin.Ptr_any -> Ptr.val_ref layout
            | Bin.Ptr_key -> Ptr.key_exn layout
          in
          let _, entries =
            Array.fold_left
              (fun (i, acc) -> function
                | None -> (i + 1, acc)
                | Some ptr ->
                    let vref = vref_of_ptr ptr in
                    (i + 1, { Bin.index = i; vref } :: acc))
              (0, []) t.entries
          in
          let entries = List.rev entries in
          Bin.Tree { depth = t.depth; length = t.length; entries }

    let is_root t = t.root
    let is_stable t = should_be_stable ~length:(length t) ~root:(is_root t)

    let to_bin layout mode t =
      let v = to_bin_v layout mode t.v in
      Bin.v ~root:(is_root t) ~hash:(lazy (Val_ref.to_hash t.v_ref)) v

    module Concrete = struct
      type kinded_key =
        | Contents of contents_key
        | Contents_x of metadata * contents_key
        | Node of node_key
      [@@deriving irmin]

      type entry = { name : step; key : kinded_key } [@@deriving irmin]

      type 'a pointer = { index : int; pointer : hash; tree : 'a }
      [@@deriving irmin]

      type 'a tree = { depth : int; length : int; pointers : 'a pointer list }
      [@@deriving irmin]

      type t = Tree of t tree | Value of entry list [@@deriving irmin]

      let to_entry (name, v) =
        match v with
        | `Contents (contents_key, m) ->
            if T.equal_metadata m Metadata.default then
              { name; key = Contents contents_key }
            else { name; key = Contents_x (m, contents_key) }
        | `Node node_key -> { name; key = Node node_key }

      let of_entry e =
        ( e.name,
          match e.key with
          | Contents key -> `Contents (key, Metadata.default)
          | Contents_x (m, key) -> `Contents (key, m)
          | Node key -> `Node key )

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
            ( Val_ref.to_hash t.v_ref,
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
            ( Val_ref.to_hash t.v_ref,
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
      let hash v = Bin.V.hash (to_bin_v Total Bin.Ptr_any v) in
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
                let t =
                  { v_ref = Val_ref.of_hash (lazy pointer); root = false; v }
                in
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
      let hash =
        if should_be_stable ~length ~root:true then
          let node = Node.of_seq (seq_v Total v) in
          Node.hash node
        else hash v
      in
      { v_ref = Val_ref.of_hash (lazy hash); root = true; v }

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

    let hash t = Val_ref.to_hash t.v_ref

    let check_write_op_supported t =
      if not @@ is_root t then
        failwith "Cannot perform operation on non-root inode value."

    let stabilize_root layout t =
      let n = length t in
      (* If [t] is the empty inode (i.e. [n = 0]) then is is already stable *)
      if n > Conf.stable_hash then { t with root = true }
      else
        let v_ref =
          Val_ref.of_hash
            (lazy
              (let vs = seq layout t in
               Node.hash (Node.of_seq vs)))
        in
        { v_ref; v = t.v; root = true }

    let hash_key = short_hash_step
    let index ~depth k = abs (hash_key ~seed:depth k) mod Conf.entries

    (** This function shouldn't be called with the [Total] layout. In the
        future, we could add a polymorphic variant to the GADT parameter to
        enfoce that. *)
    let of_bin layout (t : key Bin.t) =
      let v =
        match t.Bin.v with
        | Bin.Values vs ->
            let vs = StepMap.of_list vs in
            Values vs
        | Tree t ->
            let entries = Array.make Conf.entries None in
            let ptr_of_key = Ptr.of_key layout in
            List.iter
              (fun { Bin.index; vref } ->
                entries.(index) <- Some (ptr_of_key vref))
              t.entries;
            Tree { depth = t.Bin.depth; length = t.length; entries }
      in
      {
        v_ref =
          Val_ref.of_hash t.Bin.hash
          (* TODO: doesn't this mean [Bin] should be keeping [key]s instead? *);
        root = t.Bin.root;
        v;
      }

    let empty : 'a. 'a layout -> 'a t =
     fun _ ->
      let v_ref = Val_ref.of_hash (lazy (Node.hash (Node.empty ()))) in
      { root = false; v_ref; v = Values StepMap.empty }

    let values layout vs =
      let length = StepMap.cardinal vs in
      if length = 0 then empty layout
      else
        let v = Values vs in
        let v_ref =
          Val_ref.of_hash (lazy (Bin.V.hash (to_bin_v layout Bin.Ptr_any v)))
        in
        { v_ref; root = false; v }

    let tree layout is =
      let v = Tree is in
      let v_ref =
        Val_ref.of_hash (lazy (Bin.V.hash (to_bin_v layout Bin.Ptr_any v)))
      in
      { v_ref; root = false; v }

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
      | Some v' when equal_value v v' -> t
      | Some _ ->
          add ~depth:0 layout ~copy ~replace:true t s v Fun.id
          |> stabilize_root layout
      | None ->
          add ~depth:0 layout ~copy ~replace:false t s v Fun.id
          |> stabilize_root layout

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
      | None -> t
      | Some _ -> remove layout ~depth:0 t s Fun.id |> stabilize_root layout

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
      stabilize_root Total t

    let save layout ~add ~index ~mem t =
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
          match index h with
          | None ->
              Fmt.failwith
                "You are trying to save to the backend an inode deserialized \
                 using [Irmin.Type] that used to contain pointer(s) to inodes \
                 which are unknown to the backend. Hash: %a"
                pp_hash h
          | Some key ->
              (* The backend already knows this target inode, there is no need to
                 traverse further down. This happens during the unit tests. *)
              key
        in
        fun ~save_dirty arr ->
          let iter_ptr = Ptr.save ~broken ~save_dirty ~clear layout in
          Array.iter (Option.iter iter_ptr) arr
      in
      let rec aux ~depth t =
        [%log.debug "save depth:%d" depth];
        match t.v with
        | Values _ -> (
            let unguarded_add hash =
              let value =
                to_bin layout Bin.Ptr_key t
                (* TODO: justify why this is safe *)
              in
              let key = add hash value in
              Val_ref.promote_exn t.v_ref key;
              key
            in
            match Val_ref.inspect t.v_ref with
            | Key key ->
                if mem key then key else unguarded_add (Key.to_hash key)
            | Hash hash -> unguarded_add (Lazy.force hash))
        | Tree n ->
            let save_dirty t k =
              let key =
                match Val_ref.inspect t.v_ref with
                | Key key -> if mem key then key else aux ~depth:(depth + 1) t
                | Hash hash -> (
                    match index (Lazy.force hash) with
                    | Some key ->
                        if mem key then key else aux ~depth:(depth + 1) t
                    | None -> aux ~depth:(depth + 1) t)
              in
              Val_ref.promote_exn t.v_ref key;
              k key
            in
            iter_entries ~save_dirty:{ f = save_dirty } n.entries;
            let key =
              add (Val_ref.to_hash t.v_ref)
                (to_bin layout Bin.Ptr_key
                   t (* TODO: justify why this is safe *))
            in
            Val_ref.promote_exn t.v_ref key;
            key
      in
      aux ~depth:0 t

    let check_stable layout t =
      let target_of_ptr = Ptr.target ~cache:true layout in
      let rec check t any_stable_ancestor =
        let stable = is_stable t || any_stable_ancestor in
        match t.v with
        | Values _ -> true
        | Tree tree ->
            Array.for_all
              (function
                | None -> true
                | Some t ->
                    let t = target_of_ptr t in
                    (if stable then not (is_stable t) else true)
                    && check t stable)
              tree.entries
      in
      check t (is_stable t)

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
    type key = Key.t
    type t = T.key Bin.t [@@deriving irmin]

    let kind (t : t) =
      (* This is the kind of newly appended values, let's use v1 then *)
      if t.root then Pack_value.Kind.Inode_v1_root
      else Pack_value.Kind.Inode_v1_nonroot

    let hash t = Bin.hash t
    let step_to_bin = T.step_to_bin_string
    let step_of_bin = T.step_of_bin_string
    let encode_compress = Irmin.Type.(unstage (encode_bin Compress.t))
    let decode_compress = Irmin.Type.(unstage (decode_bin Compress.t))

    let length_header =
      let some_int32_be = Some `Int32_be in
      let contents =
        (* NOTE: the Node instantiation of the pack store must have access to
           the header format used by contents values in order to eagerly
           construct contents keys with length information during
           [key_of_offset]. *)
        match Conf.contents_length_header with
        | `None -> None
        | `Varint -> Some `Varint
      in
      `Sometimes
        (function
        | Pack_value.Kind.Inode_v0_unstable -> None
        | Inode_v0_stable -> None
        | Inode_v1_root -> some_int32_be
        | Inode_v1_nonroot -> some_int32_be
        | Contents -> contents
        | Commit_v0 | Commit_v1 ->
            (* The node store never inspects a commit object: *)
            assert false)

    let decode_compress_length =
      match Irmin.Type.Size.of_encoding Compress.t with
      | Unknown | Static _ -> assert false
      | Dynamic f -> f

    let encode_bin :
        dict:(string -> int option) ->
        offset_of_key:(Key.t -> int63 option) ->
        t ->
        hash ->
        (string -> unit) ->
        unit =
     fun ~dict ~offset_of_key (t : t) k ->
      Stats.incr_inode_encode_bin ();
      let step s : Compress.name =
        let str = step_to_bin s in
        if String.length str <= 3 then Direct s
        else match dict str with Some i -> Indirect i | None -> Direct s
      in
      let hash h : Compress.address =
        match offset_of_key h with
        | None -> Compress.Direct h
        | Some _off -> Compress.Direct h
        (* TODO: remove [Indirect] case. *)
        (* Compress.Indirect off *)
      in
      let ptr : T.key Bin.with_index -> Compress.ptr =
       fun n ->
        let hash = hash n.vref in
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
      let v : T.key Bin.v -> Compress.v = function
        | Values vs -> Values (List.map value vs)
        | Tree { depth; length; entries } ->
            let entries = List.map ptr entries in
            Tree { Compress.depth; length; entries }
      in
      let t = Compress.v ~root:t.root ~hash:k (v t.v) in
      encode_compress t

    exception Exit of [ `Msg of string ]

    let decode_bin :
        dict:(int -> string option) ->
        key_of_offset:(int63 -> key) ->
        t Irmin.Type.decode_bin =
     fun ~dict ~key_of_offset t pos_ref ->
      Stats.incr_inode_decode_bin ();
      let i = decode_compress t pos_ref in
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
      let key : Compress.address -> T.key = function
        | Indirect off ->
            (* NOTE: this happens for _existing_ nodes in the store, but we
               don't create any new [Compress] values with non-direct pointers
               on disk.

               XXX(craigfe): clean up this comment, and check that it's actually
               true. *)
            key_of_offset off
        | Direct n -> n
      in
      let ptr : Compress.ptr -> T.key Bin.with_index =
       fun n ->
        let vref = key n.hash in
        { index = n.index; vref }
      in
      let value : Compress.value -> T.step * T.value = function
        | Contents (n, h, metadata) ->
            let name = step n in
            let hash = key h in
            (name, `Contents (hash, metadata))
        | Node (n, h) ->
            let name = step n in
            let hash = key h in
            (name, `Node hash)
      in
      let t : Compress.tagged_v -> T.key Bin.v =
       fun tv ->
        let v =
          match tv with
          | V0_stable v -> v
          | V0_unstable v -> v
          | V1_root { v; _ } -> v
          | V1_nonroot { v; _ } -> v
        in
        match v with
        | Values vs -> Values (List.rev_map value (List.rev vs))
        | Tree { depth; length; entries } ->
            let entries = List.map ptr entries in
            Tree { depth; length; entries }
      in
      let root = Compress.is_root i in
      let v = t i.tv in
      Bin.v ~root ~hash:(lazy i.hash) v

    let decode_bin_length = decode_compress_length
  end

  type hash = T.hash
  type key = Key.t

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

    let empty () = of_list []
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

    let t : t Irmin.Type.t =
      let pre_hash_binv = Irmin.Type.(unstage (pre_hash (Bin.v_t Val_ref.t))) in
      let pre_hash_node = Irmin.Type.(unstage (pre_hash Node.t)) in
      let pre_hash x =
        let stable = apply x { f = (fun _ v -> I.is_stable v) } in
        if not stable then
          let bin =
            apply x { f = (fun layout v -> I.to_bin layout Bin.Ptr_any v) }
          in
          pre_hash_binv bin.v
        else
          let vs = seq x in
          pre_hash_node (Node.of_seq vs)
      in
      let module Ptr_any = struct
        let t =
          Irmin.Type.map (Bin.t Val_ref.t)
            (fun _ -> assert false)
            (fun x ->
              apply x { f = (fun layout v -> I.to_bin layout Bin.Ptr_any v) })

        type nonrec t = t [@@deriving irmin ~equal ~compare ~pp]

        (* TODO(repr): add these to [ppx_repr] meta-deriving *)
        (* TODO(repr): why is there no easy way to get a decoder value to pass to [map ~json]? *)
        let encode_json = Irmin.Type.encode_json t
        let decode_json _ = failwith "TODO"
      end in
      Irmin.Type.map ~pre_hash ~pp:Ptr_any.pp
        ~json:(Ptr_any.encode_json, Ptr_any.decode_json)
        ~equal:Ptr_any.equal ~compare:Ptr_any.compare (Bin.t T.key_t)
        (fun bin -> Truncated (I.of_bin I.Truncated bin))
        (fun x ->
          apply x { f = (fun layout v -> I.to_bin layout Bin.Ptr_key v) })

    let hash t = apply t { f = (fun _ v -> I.hash v) }

    let save ~add ~index ~mem t =
      let f layout v =
        I.check_write_op_supported v;
        I.save layout ~add ~index ~mem v
      in
      apply t { f }

    let of_raw (find' : key -> key Bin.t option) v =
      Stats.incr_inode_of_raw ();
      let rec find h =
        match find' h with None -> None | Some v -> Some (I.of_bin layout v)
      and layout = I.Partial find in
      Partial (layout, I.of_bin layout v)

    let to_raw t =
      apply t { f = (fun layout v -> I.to_bin layout Bin.Ptr_key v) }

    let stable t = apply t { f = (fun _ v -> I.is_stable v) }
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
      type value = [ `Contents of hash * metadata | `Node of hash ]

      let of_node t = t

      let keyvalue_of_hashvalue = function
        | `Contents (h, m) -> `Contents (Key.unfindable_of_hash h, m)
        | `Node h -> `Node (Key.unfindable_of_hash h)

      let hashvalue_of_keyvalue = function
        | `Contents (k, m) -> `Contents (Key.to_hash k, m)
        | `Node k -> `Node (Key.to_hash k)

      let _of_list bindings =
        bindings
        |> List.map (fun (k, v) -> (k, keyvalue_of_hashvalue v))
        |> of_list

      let of_seq bindings =
        bindings
        |> Seq.map (fun (k, v) -> (k, keyvalue_of_hashvalue v))
        |> of_seq

      let add : t -> step -> value -> t =
       fun t s v -> add t s (keyvalue_of_hashvalue v)

      let list ?offset ?length ?cache t =
        list ?offset ?length ?cache t
        |> List.map (fun (s, v) -> (s, hashvalue_of_keyvalue v))

      let find ?cache t s = find ?cache t s |> Option.map hashvalue_of_keyvalue
      let length = length
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
    (Key : Irmin.Key.S with type hash = H.t)
    (Node : Irmin.Node.Generic_key.S
              with type hash = H.t
               and type contents_key = Key.t
               and type node_key = Key.t)
    (Inter : Internal
               with type hash = H.t
                and type key = Key.t
                and type Val.metadata = Node.metadata
                and type Val.step = Node.step)
    (Pack : Indexable.S
              with type key = Key.t
               and type hash = H.t
               and type value = Inter.Raw.t) =
struct
  module Hash = H
  module Val = Inter.Val
  module Key = Key

  type 'a t = 'a Pack.t
  type key = Key.t [@@deriving irmin ~equal]
  type hash = Hash.t
  type value = Inter.Val.t

  let mem t k = Pack.mem t k
  let index t k = Pack.index t k

  let find t k =
    Pack.find t k >|= function
    | None -> None
    | Some v ->
        let find = Pack.unsafe_find ~check_integrity:true t in
        let v = Val.of_raw find v in
        Some v

  let save t v =
    let add k v =
      Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false t k v
    in
    Val.save ~add ~index:(Pack.index_direct t) ~mem:(Pack.unsafe_mem t) v

  let hash v = Val.hash v
  let add t v = Lwt.return (save t v)
  let equal_hash = Irmin.Type.(unstage (equal H.t))

  let check_hash expected got =
    if equal_hash expected got then ()
    else
      Fmt.invalid_arg "corrupted value: got %a, expecting %a" Inter.pp_hash
        expected Inter.pp_hash got

  let unsafe_add t k v =
    check_hash k (hash v);
    Lwt.return (save t v)

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
    (Node : Irmin.Node.Generic_key.S
              with type hash = H.t
               and type contents_key = H.t Pack_key.t
               and type node_key = H.t Pack_key.t)
    (Inter : Internal
               with type hash = H.t
                and type key = H.t Pack_key.t
                and type Val.metadata = Node.metadata
                and type Val.step = Node.step)
    (CA : Pack_store.Maker
            with type hash = H.t
             and type index := Pack_index.Make(H).t) =
struct
  module Persistent_pack = CA.Make (Inter.Raw)
  module Pack = Persistent_pack
  module XKey = Pack_key.Make (H)
  include Make (H) (XKey) (Node) (Inter) (Pack)

  let v = Pack.v
  let sync = Pack.sync
  let integrity_check = Pack.integrity_check
  let clear_caches = Pack.clear_caches
end
