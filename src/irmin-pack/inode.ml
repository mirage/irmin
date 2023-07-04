(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

exception Max_depth of int

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

  (* Keep at most 50 bits of information. *)
  let max_depth = int_of_float (log (2. ** 50.) /. log (float Conf.entries))

  module T = struct
    type hash = H.t [@@deriving irmin ~pp ~to_bin_string ~equal]
    type key = Key.t [@@deriving irmin ~pp ~equal]
    type node_key = Node.node_key [@@deriving irmin]
    type contents_key = Node.contents_key [@@deriving irmin]

    type step = Node.step
    [@@deriving irmin ~compare ~to_bin_string ~of_bin_string ~short_hash]

    type metadata = Node.metadata [@@deriving irmin ~equal]
    type value = Node.value [@@deriving irmin ~equal]

    module Metadata = Node.Metadata

    exception Dangling_hash = Node.Dangling_hash

    let raise_dangling_hash c hash =
      let context = "Irmin_pack.Inode." ^ c in
      raise (Dangling_hash { context; hash })

    let unsafe_keyvalue_of_hashvalue = function
      | `Contents (h, m) -> `Contents (Key.unfindable_of_hash h, m)
      | `Node h -> `Node (Key.unfindable_of_hash h)

    let hashvalue_of_keyvalue = function
      | `Contents (k, m) -> `Contents (Key.to_hash k, m)
      | `Node k -> `Node (Key.to_hash k)
  end

  module Step =
    Irmin.Hash.Typed
      (H)
      (struct
        type t = T.step

        let t = T.step_t
      end)

  module Child_ordering : Child_ordering with type step := T.step = struct
    open T

    type key = bytes

    let log_entry = int_of_float (log (float Conf.entries) /. log 2.)

    let () =
      assert (log_entry >= 1);
      (* NOTE: the [`Hash_bits] mode is restricted to inodes with at most 1024
         entries in order to simplify the implementation (see below). *)
      assert ((not (Conf.inode_child_order = `Hash_bits)) || log_entry <= 10);
      assert (Conf.entries = int_of_float (2. ** float log_entry))

    let key =
      match Conf.inode_child_order with
      | `Hash_bits ->
          (* Bytes.unsafe_of_string usage: possibly safe TODO justify safety, or switch to
             use the safe Bytes.of_string *)
          fun s -> Bytes.unsafe_of_string (hash_to_bin_string (Step.hash s))
      | `Seeded_hash | `Custom _ ->
          (* Bytes.unsafe_of_string usage: possibly safe TODO justify safety, or switch to
             use the safe Bytes.of_string *)
          fun s -> Bytes.unsafe_of_string (step_to_bin_string s)

    (* Assume [k = cryto_hash(step)] (see {!key}) and [Conf.entry] can
       can represented with [n] bits. Then, [hash_bits ~depth k] is
       the [n]-bits integer [i] with the following binary representation:

         [k(n*depth) ... k(n*depth+n-1)]

       When [n] is not a power of 2, [hash_bits] needs to handle
       unaligned reads properly. *)
    let hash_bits ~depth k =
      assert (Bytes.length k = Step.hash_size);
      (* We require above that the child indices have at most 10 bits to ensure
         that they span no more than 2 bytes of the step hash. The 3 byte case
         (with [1 + 8 + 1]) does not happen for 10-bit indices because 10 is
         even, but [2 + 8 + 1] would occur with 11-byte indices (e.g. when
         [depth=2]). *)
      let byte = 8 in
      let initial_bit_pos = log_entry * depth in
      let n = initial_bit_pos / byte in
      let r = initial_bit_pos mod byte in
      if n >= Step.hash_size then raise (Max_depth depth);
      if r + log_entry <= byte then
        (* The index is contained in a single character of the hash *)
        let i = Bytes.get_uint8 k n in
        let e0 = i lsr (byte - log_entry - r) in
        let r0 = e0 land (Conf.entries - 1) in
        r0
      else
        (* The index spans two characters of the hash *)
        let i0 = Bytes.get_uint8 k n in
        let to_read = byte - r in
        let rest = log_entry - to_read in
        let mask = (1 lsl to_read) - 1 in
        let r0 = (i0 land mask) lsl rest in
        if n + 1 >= Step.hash_size then raise (Max_depth depth);
        let i1 = Bytes.get_uint8 k (n + 1) in
        let r1 = i1 lsr (byte - rest) in
        r0 + r1

    let short_hash = Irmin.Type.(unstage (short_hash bytes))
    let seeded_hash ~depth k = abs (short_hash ~seed:depth k) mod Conf.entries

    let index =
      match Conf.inode_child_order with
      | `Seeded_hash -> seeded_hash
      | `Hash_bits -> hash_bits
      | `Custom f -> f
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
    val to_lazy_hash : t -> hash Lazy.t
    val to_key_exn : t -> key
    val is_key : t -> bool
  end = struct
    open T

    (** Nodes that have been persisted to an underlying store are referenced via
        keys. Otherwise, when building in-memory inodes (e.g. via [Portable] or
        [of_concrete_exn]) lazily-computed hashes are used instead. If such
        values are persisted, the hash reference can be promoted to a key
        reference (but [Key] values are never demoted to hashes).

        NOTE: in future, we could reflect the case of this type in a type
        parameter and refactor the [layout] types below to get static guarantees
        that [Portable] nodes (with hashes for internal pointers) are not saved
        without first saving their children. *)
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

    let to_lazy_hash t =
      match !t with Hash h -> h | Key k -> lazy (Key.to_hash k)

    let is_key t = match !t with Key _ -> true | _ -> false

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
    type _ mode = Ptr_key : key mode | Ptr_any : Val_ref.t mode

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
      record "Bin.t" (fun hash root v -> { hash = Lazy.from_val hash; root; v })
      |+ field "hash" H.t (fun t -> Lazy.force t.hash)
      |+ field "root" bool (fun t -> t.root)
      |+ field "v" v_t (fun t -> t.v)
      |> sealr
      |> like ~pre_hash

    let v ~hash ~root v = { hash; root; v }
    let hash t = Lazy.force t.hash

    let depth t =
      match t.v with
      | Values _ -> if t.root then Some 0 else None
      | Tree t -> Some t.depth
  end

  (* Compressed binary representation *)
  module Compress = struct
    open T

    type dict_key = int [@@deriving irmin]
    type pack_offset = int63 [@@deriving irmin]
    type name = Indirect of dict_key | Direct of step
    type address = Offset of pack_offset | Hash of H.t [@@deriving irmin]
    type ptr = { index : int; hash : address } [@@deriving irmin]

    type tree = { depth : int; length : int; entries : ptr list }
    [@@deriving irmin]

    type value =
      | Contents of name * address * metadata
      | Node of name * address

    let is_default = T.(equal_metadata Metadata.default)

    (* We distribute products over sums in the type representation of [value]
       in order to pack many possible cases into a single tag character in the
       encoded representation.

       - whether the referenced value is a [Node] or a [Contents] value;

       - in the [Contents] case, whether the associated metadata is [default]
         (in which case the serialised representation elides it), or if it is
         included;

       - whether the [name] of the entry is provided inline [Direct], or is
         stored in the dict and refernced via a dict key [Indirect];

       - whether the [address] of the entry is a pack offset or a hash to be
         indexed *)
    let[@ocamlformat "disable"] value_t : value Irmin.Type.t =
      let module Payload = struct
          (* Different payload types that can appear after packed tags: *)
          let io  = [%typ: dict_key * pack_offset]
          let ih  = [%typ: dict_key * H.t]
          let do_ = [%typ: step * pack_offset]
          let dh  = [%typ: step * H.t]
          (* As above but for contents values with non-default metadata: *)
          let x_io = [%typ: dict_key * pack_offset * metadata]
          let x_ih = [%typ: dict_key * H.t * metadata]
          let x_do = [%typ: step * pack_offset * metadata]
          let x_dh = [%typ: step * H.t * metadata]
      end in
      let open Irmin.Type in
      variant "Compress.value"
        (fun
          (* The ordering of these arguments determines which tags are assigned
             to the cases, so should not be changed: *)
          contents_io contents_x_io node_io contents_ih contents_x_ih node_ih
          contents_do contents_x_do node_do contents_dh contents_x_dh node_dh
        -> function
        | Node (Indirect n, Offset o) -> node_io (n, o)
        | Node (Indirect n, Hash h)   -> node_ih (n, h)
        | Node (Direct n,   Offset o) -> node_do (n, o)
        | Node (Direct n,   Hash h)   -> node_dh (n, h)
        | Contents (Indirect n, Offset o, m) -> if is_default m then contents_io (n, o) else contents_x_io (n, o, m)
        | Contents (Indirect n, Hash h,   m) -> if is_default m then contents_ih (n, h) else contents_x_ih (n, h, m)
        | Contents (Direct n,   Offset o, m) -> if is_default m then contents_do (n, o) else contents_x_do (n, o, m)
        | Contents (Direct n,   Hash h,   m) -> if is_default m then contents_dh (n, h) else contents_x_dh (n, h, m))
      |~ case1 "contents-io"   Payload.io   (fun (n, o)    -> Contents (Indirect n, Offset o, Metadata.default))
      |~ case1 "contents-x-io" Payload.x_io (fun (n, i, m) -> Contents (Indirect n, Offset i, m))
      |~ case1 "node-io"       Payload.io   (fun (n, i)    -> Node (Indirect n, Offset i))
      |~ case1 "contents-ih"   Payload.ih   (fun (n, h)    -> Contents (Indirect n, Hash h, Metadata.default))
      |~ case1 "contents-x-ih" Payload.x_ih (fun (n, h, m) -> Contents (Indirect n, Hash h, m))
      |~ case1 "node-ih"       Payload.ih   (fun (n, h)    -> Node (Indirect n, Hash h))
      |~ case1 "contents-do"   Payload.do_  (fun (n, i)    -> Contents (Direct n, Offset i, Metadata.default))
      |~ case1 "contents-x-do" Payload.x_do (fun (n, i, m) -> Contents (Direct n, Offset i, m))
      |~ case1 "node-do"       Payload.do_  (fun (n, i)    -> Node (Direct n, Offset i))
      |~ case1 "contents-dh"   Payload.dh   (fun (n, i)    -> Contents (Direct n, Hash i, Metadata.default))
      |~ case1 "contents-x-dh" Payload.x_dh (fun (n, i, m) -> Contents (Direct n, Hash i, m))
      |~ case1 "node-dd"       Payload.dh   (fun (n, i)    -> Node (Direct n, Hash i))
      |> sealv

    type v = Values of value list | Tree of tree
    [@@deriving irmin ~encode_bin ~decode_bin ~size_of]

    let dynamic_size_of_v_encoding =
      match Irmin.Type.Size.of_encoding v_t with
      | Irmin.Type.Size.Dynamic f -> f
      | _ -> assert false

    type kind = Pack_value.Kind.t
    [@@deriving irmin ~encode_bin ~decode_bin ~size_of]

    type nonrec int = int [@@deriving irmin ~encode_bin ~decode_bin]

    let no_length = 0
    let is_real_length length = not (length = 0)

    type v1 = { mutable length : int; v : v } [@@deriving irmin]
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
      match size_of_v v with
      | Some length ->
          tv.length <- length;
          encode_bin_kind kind f;
          encode_bin_int length f;
          encode_bin_v v f
      | None ->
          let buf = Buffer.create 1024 in
          encode_bin_v v (Buffer.add_string buf);
          let length = Buffer.length buf in
          tv.length <- length;
          encode_bin_kind kind f;
          encode_bin_int length f;
          f (Buffer.contents buf)

    let encode_bin_tv tv f =
      match tv with
      | V0_stable _ -> assert false
      | V0_unstable _ -> assert false
      | V1_root { length; v } when is_real_length length ->
          encode_bin_kind Pack_value.Kind.Inode_v2_root f;
          encode_bin_int length f;
          encode_bin_v v f
      | V1_nonroot { length; v } when is_real_length length ->
          encode_bin_kind Pack_value.Kind.Inode_v2_nonroot f;
          encode_bin_int length f;
          encode_bin_v v f
      | V1_root tv -> encode_bin_tv_staggered tv Pack_value.Kind.Inode_v2_root f
      | V1_nonroot tv ->
          encode_bin_tv_staggered tv Pack_value.Kind.Inode_v2_nonroot f

    let decode_bin_tv s off =
      let kind = decode_bin_kind s off in
      match kind with
      | Pack_value.Kind.Inode_v1_unstable ->
          let v = decode_bin_v s off in
          V0_unstable v
      | Inode_v1_stable ->
          let v = decode_bin_v s off in
          V0_stable v
      | Inode_v2_root ->
          let length = decode_bin_int s off in
          assert (is_real_length length);
          let v = decode_bin_v s off in
          V1_root { length; v }
      | Inode_v2_nonroot ->
          let length = decode_bin_int s off in
          assert (is_real_length length);
          let v = decode_bin_v s off in
          V1_nonroot { length; v }
      | Commit_v1 | Commit_v2 -> assert false
      | Contents -> assert false
      | Dangling_parent_commit -> assert false

    let size_of_tv =
      let of_encoding s off =
        let offref = ref off in
        let kind = decode_bin_kind s offref in
        let magic_len = 1 in
        match kind with
        | Pack_value.Kind.Inode_v1_unstable | Inode_v1_stable ->
            let vlen = dynamic_size_of_v_encoding s !offref in
            magic_len + vlen
        | Inode_v2_root | Inode_v2_nonroot ->
            let before = !offref in
            let vlen = decode_bin_int s offref in
            let after = !offref in
            let lenlen = after - before in
            magic_len + lenlen + vlen
        | Commit_v1 | Commit_v2 | Contents -> assert false
        | Dangling_parent_commit -> assert false
      in
      Irmin.Type.Size.custom_dynamic ~of_encoding ()

    let tagged_v_t =
      Irmin.Type.like ~bin:(encode_bin_tv, decode_bin_tv, size_of_tv) tagged_v_t

    type t = { hash : H.t; tv : tagged_v } [@@deriving irmin]

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
      - When [Truncated], it either originates from an [Irmin.Type]
        deserialisation or from a proof.

      Almost all other functions in [Val_impl] are polymorphic regarding the
      layout of the manipulated inode.

      {4 Details on the [Truncated] Layout}

      The [Truncated] layout is identical to [Partial] except for the missing
      [find] function.

      On the one hand, when creating the root of a [Truncated] inode, the
      pointers to children inodes - if any - are set to the [Broken] tag,
      meaning that we know the hash to such children but we will have no way to
      load them in the future. On the other hand, when adding child to a
      [Truncated] inode, there is no such problem, the pointer is then set to
      the [Intact] tag.

      A tree of inode only made of [Intact] tags is similar to a [Total] layout.

      As of Irmin 2.4 (February 2022), inode deserialisation using Repr happens
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
      | Partial : find -> partial_ptr layout
      | Truncated : truncated_ptr layout

    and find = expected_depth:int -> key -> partial_ptr t option

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
              [v] has been written to / loaded from a store. *)
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

      (** [force = false] will cause [target] to raise an exception when
          encountering a tag [Lazy] inside a [Partial] inode. This feature is
          used by [to_concrete] to make shallow the non-loaded inode branches. *)
      let target :
          type ptr.
          expected_depth:int ->
          cache:bool ->
          force:bool ->
          string ->
          ptr layout ->
          ptr ->
          ptr t =
       fun ~expected_depth ~cache ~force context layout ->
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
                if not force then raise_dangling_hash context (Key.to_hash key);
                match find ~expected_depth key with
                | None ->
                    Fmt.failwith "%a: unknown inode key (%s)" pp_key key context
                | Some x ->
                    if cache then t.target <- Lazy_loaded x;
                    x))
        | Truncated -> (
            function
            | Intact entry -> entry
            | Broken vref ->
                let h = Val_ref.to_hash vref in
                raise_dangling_hash context h)

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
          broken:(hash, key) cps ->
          save_dirty:(ptr t, key) cps ->
          clear:bool ->
          ptr layout ->
          ptr ->
          unit =
       fun ~broken ~save_dirty ~clear -> function
        (* Invariant: after returning, we can recover the key from the saved
           pointer (i.e. [key_exn] does not raise an exception). This is necessary
           in order to be able to serialise a parent inode (for export) after
           having saved its children. *)
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
                (* In this case, [entry.v_ref] is a [Hash h] such that [mem t
                   (index t h) = true]. We "save" the entry in order to trigger
                   the [index] lookup and recover the key, in order to meet the
                   return invariant above.

                   TODO: refactor this case to be more precise. *)
                save_dirty.f entry (fun key ->
                    if clear then box.target <- Lazy key)
            | { target = Lazy _ } -> ())
        | Truncated -> (
            function
            (* TODO: this branch is currently untested: we never attempt to
               save a truncated node as part of the unit tests. *)
            | Intact entry ->
                save_dirty.f entry (fun key ->
                    Val_ref.promote_exn entry.v_ref key)
            | Broken vref ->
                if not (Val_ref.is_key vref) then
                  broken.f (Val_ref.to_hash vref) (fun key ->
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

    let rec seq_tree layout bucket_seq ~depth ~cache : cont -> cont =
     fun k ~off ~len ->
      assert (off >= 0);
      assert (len > 0);
      match bucket_seq () with
      | Seq.Nil -> k ~off ~len
      | Seq.Cons (None, rest) -> seq_tree layout rest ~depth ~cache k ~off ~len
      | Seq.Cons (Some i, rest) ->
          let trg =
            let expected_depth = depth + 1 in
            Ptr.target ~expected_depth ~cache ~force:true "seq_tree" layout i
          in
          let trg_len = length trg in
          if off - trg_len >= 0 then
            (* Skip a branch of the inode tree in case the user asked for a
               specific starting offset.

               Without this branch the algorithm would keep the same semantic
               because [seq_value] would handles the pagination value by value
               instead. *)
            let off = off - trg_len in
            seq_tree layout rest ~depth ~cache k ~off ~len
          else
            seq_v layout trg.v ~cache
              (seq_tree layout rest ~depth ~cache k)
              ~off ~len

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
      | Tree t ->
          let depth = t.depth in
          seq_tree layout (Array.to_seq t.entries) ~depth ~cache k ~off ~len
      | Values vs -> seq_values layout (StepMap.to_seq vs) k ~off ~len

    let list_v layout v ~cache k ~off ~len =
      match v with
      | Tree _ ->
          let s () = seq_v layout v ~cache k ~off ~len in
          List.of_seq s
      | Values vs ->
          if off = 0 && len = Int.max_int then StepMap.bindings vs
          else
            let seq () = seq_values layout (StepMap.to_seq vs) k ~off ~len in
            List.of_seq seq

    let empty_continuation : cont = fun ~off:_ ~len:_ -> Seq.Nil

    let seq layout ?offset:(off = 0) ?length:(len = Int.max_int) ?(cache = true)
        t : (step * value) Seq.t =
      if off < 0 then invalid_arg "Invalid pagination offset";
      if len < 0 then invalid_arg "Invalid pagination length";
      if len = 0 then Seq.empty
      else fun () -> seq_v layout t.v ~cache empty_continuation ~off ~len

    let list layout ?offset:(off = 0) ?length:(len = Int.max_int)
        ?(cache = true) t : (step * value) list =
      if off < 0 then invalid_arg "Invalid pagination offset";
      if len < 0 then invalid_arg "Invalid pagination length";
      if len = 0 then []
      else list_v layout t.v ~cache empty_continuation ~off ~len

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
      Bin.v ~root:(is_root t) ~hash:(Val_ref.to_lazy_hash t.v_ref) v

    type len = [ `Eq of int | `Ge of int ] [@@deriving irmin]

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

      type t = Tree of t tree | Values of entry list | Blinded
      [@@deriving irmin]

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
        | `Invalid_length of len * int * t
        | `Duplicated_entries of t
        | `Duplicated_pointers of t
        | `Unsorted_entries of t
        | `Unsorted_pointers of t
        | `Blinded_root
        | `Too_large_values of t
        | `Empty ]
      [@@deriving irmin]

      let rec length = function
        | Values l -> `Eq (List.length l)
        | Tree t ->
            List.fold_left
              (fun acc p ->
                match (acc, length p.tree) with
                | `Eq x, `Eq y -> `Eq (x + y)
                | (`Eq x | `Ge x), (`Eq y | `Ge y) -> `Ge (x + y))
              (`Eq 0) t.pointers
        | Blinded -> `Ge 0

      let pp = Irmin.Type.pp_json t

      let pp_len ppf = function
        | `Eq e -> Fmt.pf ppf "%d" e
        | `Ge e -> Fmt.pf ppf "'at least %d'" e

      let pp_error ppf = function
        | `Invalid_hash (got, expected, t) ->
            Fmt.pf ppf "invalid hash for %a@,got: %a@,expecting: %a" pp t
              pp_hash got pp_hash expected
        | `Invalid_depth (got, expected, t) ->
            Fmt.pf ppf "invalid depth for %a@,got: %d@,expecting: %d" pp t got
              expected
        | `Invalid_length (got, expected, t) ->
            Fmt.pf ppf "invalid length for %a@,got: %a@,expecting: %d" pp t
              pp_len got expected
        | `Duplicated_entries t -> Fmt.pf ppf "duplicated entries: %a" pp t
        | `Duplicated_pointers t -> Fmt.pf ppf "duplicated pointers: %a" pp t
        | `Unsorted_entries t -> Fmt.pf ppf "entries should be sorted: %a" pp t
        | `Unsorted_pointers t ->
            Fmt.pf ppf "pointers should be sorted: %a" pp t
        | `Blinded_root -> Fmt.pf ppf "blinded root"
        | `Too_large_values t ->
            Fmt.pf ppf "A Values should have at most Conf.entries elements: %a"
              pp t
        | `Empty -> Fmt.pf ppf "concrete subtrees cannot be empty"
    end

    let to_concrete ~force (la : 'ptr layout) (t : 'ptr t) =
      let rec aux t =
        let h = Val_ref.to_hash t.v_ref in
        match t.v with
        | Tree tr ->
            ( h,
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
                            let expected_depth = tr.depth + 1 in
                            let pointer, tree =
                              try
                                aux
                                  (Ptr.target ~expected_depth ~cache:true ~force
                                     "to_concrete" la t)
                              with Dangling_hash { hash; _ } ->
                                (hash, Concrete.Blinded)
                            in
                            (i + 1, { Concrete.index = i; tree; pointer } :: acc))
                      (0, []) tr.entries
                    |> snd
                    |> List.rev;
                } )
        | Values l ->
            ( h,
              Concrete.Values (List.map Concrete.to_entry (StepMap.bindings l))
            )
      in
      snd (aux t)

    exception Invalid_hash of hash * hash * Concrete.t
    exception Invalid_depth of int * int * Concrete.t
    exception Invalid_length of len * int * Concrete.t
    exception Empty
    exception Duplicated_entries of Concrete.t
    exception Duplicated_pointers of Concrete.t
    exception Unsorted_entries of Concrete.t
    exception Unsorted_pointers of Concrete.t
    exception Blinded_root
    exception Too_large_values of Concrete.t

    let hash_equal = Irmin.Type.(unstage (equal hash_t))

    let of_concrete_exn : type a. depth:int -> a layout -> _ -> a t =
     fun ~depth la t ->
      let sort_entries =
        List.sort_uniq (fun x y -> compare x.Concrete.name y.Concrete.name)
      in
      let sort_pointers =
        List.sort_uniq (fun x y -> compare x.Concrete.index y.Concrete.index)
      in
      let check_entries t es =
        if es = [] then raise Empty;
        let s = sort_entries es in
        if List.compare_length_with es Conf.entries > 0 then
          raise (Too_large_values t);
        if List.compare_lengths s es <> 0 then raise (Duplicated_entries t);
        if s <> es then raise (Unsorted_entries t)
      in
      let check_pointers t ps =
        if ps = [] then raise Empty;
        let s = sort_pointers ps in
        if List.length s <> List.length ps then raise (Duplicated_pointers t);
        if s <> ps then raise (Unsorted_pointers t)
      in
      let hash v = Bin.V.hash (to_bin_v la Bin.Ptr_any v) in
      let rec aux depth t =
        match t with
        | Concrete.Blinded -> None
        | Concrete.Values l ->
            check_entries t l;
            Some (Values (StepMap.of_list (List.map Concrete.of_entry l)))
        | Concrete.Tree tr ->
            let entries = Array.make Conf.entries None in
            check_pointers t tr.pointers;
            List.iter
              (fun { Concrete.index; pointer; tree } ->
                match aux (depth + 1) tree with
                | None ->
                    (* Child is blinded *)
                    let ptr =
                      match la with
                      | Total -> assert false
                      | Partial _ ->
                          (* [of_concrete_exn (Partial _)] is only used in the
                             context of portable inodes, [unfindable_of_hash] is
                             fine. *)
                          let k = Key.unfindable_of_hash pointer in
                          Ptr.of_key la k
                      | Truncated ->
                          let v_ref = Val_ref.of_hash (Lazy.from_val pointer) in
                          (Broken v_ref : a)
                    in
                    entries.(index) <- Some ptr
                | Some v ->
                    let hash = hash v in
                    if not (hash_equal hash pointer) then
                      raise (Invalid_hash (hash, pointer, t));
                    let v_ref = Val_ref.of_hash (Lazy.from_val pointer) in
                    let t = { v_ref; root = false; v } in
                    entries.(index) <- Some (Ptr.of_target la t))
              tr.pointers;
            if depth <> tr.depth then raise (Invalid_depth (depth, tr.depth, t));
            let () =
              match Concrete.length t with
              | `Eq length ->
                  if length <> tr.length then
                    raise (Invalid_length (`Eq length, tr.length, t))
              | `Ge length ->
                  if length > tr.length then
                    raise (Invalid_length (`Ge length, tr.length, t))
            in

            Some (Tree { depth = tr.depth; length = tr.length; entries })
      in
      let v =
        match aux depth t with None -> raise Blinded_root | Some v -> v
      in
      let length = length_of_v v in
      let hash =
        (* Compute the hash right away (not lazily) so that
           [hash_exn ~force:false] is possible on the result of
           [of_proof]. *)
        if should_be_stable ~length ~root:(depth = 0) then
          (* [seq_v] may call [find], even if some branches are blinded *)
          let node = Node.of_seq (seq_v la v) in
          Node.hash node
        else hash v
      in
      { v_ref = Val_ref.of_hash (Lazy.from_val hash); root = depth = 0; v }

    let of_concrete ~depth la t =
      try Ok (of_concrete_exn ~depth la t) with
      | Invalid_hash (x, y, z) -> Error (`Invalid_hash (x, y, z))
      | Invalid_depth (x, y, z) -> Error (`Invalid_depth (x, y, z))
      | Invalid_length (x, y, z) -> Error (`Invalid_length (x, y, z))
      | Empty -> Error `Empty
      | Duplicated_entries t -> Error (`Duplicated_entries t)
      | Duplicated_pointers t -> Error (`Duplicated_pointers t)
      | Unsorted_entries t -> Error (`Unsorted_entries t)
      | Unsorted_pointers t -> Error (`Unsorted_pointers t)
      | Too_large_values t -> Error (`Too_large_values t)
      | Blinded_root -> Error `Blinded_root

    let hash t = Val_ref.to_hash t.v_ref

    let hash_exn ?(force = true) t =
      match Val_ref.inspect t.v_ref with
      | Key k -> Key.to_hash k
      | Hash h ->
          if Lazy.is_val h || force then Lazy.force h else raise Not_found

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
              (let vs = seq layout ~cache:false t in
               Node.hash (Node.of_seq vs)))
        in
        { v_ref; v = t.v; root = true }

    let index ~depth k =
      if depth >= max_depth then raise (Max_depth depth);
      Child_ordering.index ~depth k

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
      { v_ref = Val_ref.of_hash t.Bin.hash; root = t.Bin.root; v }

    let recompute_hash layout t =
      if is_stable t then
        let vs = seq layout ~cache:false t in
        Node.hash (Node.of_seq vs)
      else
        let v = to_bin_v layout Bin.Ptr_any t.v in
        let hash = Bin.V.hash v in
        hash

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

    let find_value ~cache layout t s =
      let key = Child_ordering.key s in
      let rec aux = function
        | Values vs -> ( try Some (StepMap.find s vs) with Not_found -> None)
        | Tree t -> (
            let i = index ~depth:t.depth key in
            let x = t.entries.(i) in
            match x with
            | None -> None
            | Some i ->
                let expected_depth = t.depth + 1 in
                aux
                  (Ptr.target ~expected_depth ~cache ~force:true "find_value"
                     layout i)
                    .v)
      in
      aux t.v

    let find ?(cache = true) layout t s = find_value ~cache layout t s

    let rec add layout ~depth ~copy ~replace parent s key v k =
      Stats.incr_inode_rec_add ();
      match parent.v with
      | Values vs ->
          let length =
            if replace then StepMap.cardinal vs else StepMap.cardinal vs + 1
          in
          let parent =
            if length <= Conf.entries then values layout (StepMap.add s v vs)
            else
              let vs = StepMap.bindings (StepMap.add s v vs) in
              let empty =
                tree layout
                  { length = 0; depth; entries = Array.make Conf.entries None }
              in
              let aux t (s', v) =
                let key' = Child_ordering.key s' in
                (add [@tailcall]) layout ~depth ~copy:false ~replace t s' key' v
                  (fun x -> x)
              in
              List.fold_left aux empty vs
          in
          k parent
      | Tree tr -> (
          assert (depth = tr.depth);
          let length = if replace then tr.length else tr.length + 1 in
          let entries = if copy then Array.copy tr.entries else tr.entries in
          let i = index ~depth key in
          match entries.(i) with
          | None ->
              let child = values layout (StepMap.singleton s v) in
              entries.(i) <- Some (Ptr.of_target layout child);
              let parent = tree layout { tr with length; entries } in
              k parent
          | Some ptr ->
              let child =
                let expected_depth = depth + 1 in
                (* [cache] is unimportant here as we've already called
                   [find_value] for that path.*)
                Ptr.target ~expected_depth ~cache:true ~force:true "add" layout
                  ptr
              in
              (add [@tailcall]) layout ~depth:(depth + 1) ~copy ~replace child s
                key v (fun child ->
                  entries.(i) <- Some (Ptr.of_target layout child);
                  let parent = tree layout { tr with length; entries } in
                  k parent))

    let add layout ~copy t s v =
      let k = Child_ordering.key s in
      match find_value ~cache:true layout t s with
      | Some v' when equal_value v v' -> t
      | Some _ ->
          add ~depth:0 layout ~copy ~replace:true t s k v Fun.id
          |> stabilize_root layout
      | None ->
          add ~depth:0 layout ~copy ~replace:false t s k v Fun.id
          |> stabilize_root layout

    let rec remove layout parent s key k =
      Stats.incr_inode_rec_remove ();
      match parent.v with
      | Values vs ->
          let parent = values layout (StepMap.remove s vs) in
          k parent
      | Tree tr -> (
          let depth = tr.depth in
          let len = tr.length - 1 in
          if len <= Conf.entries then
            let vs = seq_tree layout tr in
            let vs = StepMap.of_seq vs in
            let vs = StepMap.remove s vs in
            let parent = values layout vs in
            k parent
          else
            let entries = Array.copy tr.entries in
            let i = index ~depth key in
            match entries.(i) with
            | None -> assert false
            | Some ptr ->
                let child =
                  let expected_depth = depth + 1 in
                  (* [cache] is unimportant here as we've already called
                     [find_value] for that path.*)
                  Ptr.target ~expected_depth ~cache:true ~force:true "remove"
                    layout ptr
                in
                if length child = 1 then (
                  entries.(i) <- None;
                  let parent = tree layout { depth; length = len; entries } in
                  k parent)
                else
                  (remove [@tailcall]) layout child s key (fun child ->
                      entries.(i) <- Some (Ptr.of_target layout child);
                      let parent =
                        tree layout { tr with length = len; entries }
                      in
                      k parent))

    let remove layout t s =
      let k = Child_ordering.key s in
      match find_value ~cache:true layout t s with
      | None -> t
      | Some _ -> remove layout t s k Fun.id |> stabilize_root layout

    let of_seq la l =
      let t =
        let rec aux_big seq inode =
          match seq () with
          | Seq.Nil -> inode
          | Seq.Cons ((s, v), rest) ->
              aux_big rest (add la ~copy:false inode s v)
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
              values la map
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
              if !len = Conf.entries then aux_big rest (values la map)
              else aux_small rest map
        in
        aux_small l StepMap.empty
      in
      stabilize_root la t

    let save layout ~add ~index ~mem t =
      let clear =
        (* When set to [true], collect the loaded inodes as soon as they're
           saved.

           This parameter is not exposed yet. Ideally it would be exposed and
           be forwarded from [Tree.export ?clear] through [P.Node.add].

           It is currently set to false in order to preserve behaviour *)
        false
      in
      let iter_entries =
        let broken h k =
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
              k key
        in
        fun ~save_dirty arr ->
          let iter_ptr =
            Ptr.save ~broken:{ f = broken } ~save_dirty ~clear layout
          in
          Array.iter (Option.iter iter_ptr) arr
      in
      let rec aux ~depth t =
        match t.v with
        | Values _ -> (
            [%log.debug "Inode.save values depth:%d" depth];
            let unguarded_add hash =
              let value =
                (* NOTE: the choice of [Bin.mode] is irrelevant (and this
                   conversion is always safe), since nodes of kind [Values _]
                   contain no internal pointers. *)
                to_bin layout Bin.Ptr_key t
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
            [%log.debug "Inode.save tree depth:%d" depth];
            let save_dirty t k =
              let key =
                match Val_ref.inspect t.v_ref with
                | Key key -> if mem key then key else aux ~depth:(depth + 1) t
                | Hash hash -> (
                    match index (Lazy.force hash) with
                    | Some key ->
                        if mem key then key
                        else
                          (* In this case, [index] has returned a key that is
                             not present in the underlying store. This is
                             permitted by the contract on index functions (and
                             required by [irmin-pack.mem]), but never happens
                             with the persistent {!Pack_store} backend (provided
                             the store is not corrupted). *)
                          aux ~depth:(depth + 1) t
                    | None -> aux ~depth:(depth + 1) t)
              in
              Val_ref.promote_exn t.v_ref key;
              k key
            in
            iter_entries ~save_dirty:{ f = save_dirty } n.entries;
            let bin =
              (* Serialising with [Bin.Ptr_key] is safe here because just called
                 [Ptr.save] on any dirty children (and we never try to save
                 [Portable] nodes). *)
              to_bin layout Bin.Ptr_key t
            in
            let key = add (Val_ref.to_hash t.v_ref) bin in
            Val_ref.promote_exn t.v_ref key;
            key
      in
      aux ~depth:0 t

    let check_stable layout t =
      let rec check t any_stable_ancestor =
        let stable = is_stable t || any_stable_ancestor in
        match t.v with
        | Values _ -> true
        | Tree tree ->
            Array.for_all
              (function
                | None -> true
                | Some t ->
                    let t =
                      let expected_depth = tree.depth + 1 in
                      Ptr.target ~expected_depth ~cache:true ~force:true
                        "check_stable" layout t
                    in
                    (if stable then not (is_stable t) else true)
                    && check t stable)
              tree.entries
      in
      check t (is_stable t)

    let contains_empty_map layout t =
      let rec check_lower t =
        match t.v with
        | Values l when StepMap.is_empty l -> true
        | Values _ -> false
        | Tree inodes ->
            Array.exists
              (function
                | None -> false
                | Some t ->
                    let expected_depth = inodes.depth + 1 in
                    Ptr.target ~expected_depth ~cache:true ~force:true
                      "contains_empty_map" layout t
                    |> check_lower)
              inodes.entries
      in
      check_lower t

    let is_tree t = match t.v with Tree _ -> true | Values _ -> false

    module Proof = struct
      type value = [ `Contents of hash * metadata | `Node of hash ]
      [@@deriving irmin]

      type t =
        [ `Blinded of hash
        | `Values of (step * value) list
        | `Inode of int * (int * t) list ]
      [@@deriving irmin]

      let weaken_step_value (step, v) = (step, hashvalue_of_keyvalue v)

      let strengthen_step_value (step, v) =
        (* Since proofs are used only in the context of portable, using this
           unsafe function is safe. *)
        (step, unsafe_keyvalue_of_hashvalue v)

      let rec proof_of_concrete :
          type a. hash Lazy.t -> Concrete.t -> (t -> a) -> a =
       fun h concrete k ->
        match concrete with
        | Blinded -> k (`Blinded (Lazy.force h))
        | Values vs ->
            let l =
              List.map Concrete.of_entry vs |> List.map weaken_step_value
            in
            k (`Values l)
        | Tree tr ->
            let proofs =
              List.fold_left
                (fun acc (e : _ Concrete.pointer) ->
                  let hash = Lazy.from_val e.pointer in
                  proof_of_concrete hash e.tree (fun proof ->
                      (e.index, proof) :: acc))
                [] (List.rev tr.pointers)
            in
            k (`Inode (tr.length, proofs))

      let hash_values ~depth l =
        let inode = values Truncated (StepMap.of_list l) in
        let t =
          match depth with 0 -> { inode with root = true } | _ -> inode
        in
        hash t

      let hash_inode ~depth ~length es =
        let entries = Array.make Conf.entries None in
        List.iter (fun (index, ptr) -> entries.(index) <- Some ptr) es;
        let v : truncated_ptr v = Tree { depth; length; entries } in
        Bin.V.hash (to_bin_v Truncated Bin.Ptr_any v)

      let rec concrete_of_proof :
          type a. depth:int -> t -> (hash -> Concrete.t -> a) -> a =
       fun ~depth proof k ->
        match proof with
        | `Blinded h -> k h Concrete.Blinded
        | `Values vs ->
            let vs = List.map strengthen_step_value vs in
            assert (List.compare_length_with vs Conf.entries <= 0);
            let hash = hash_values ~depth vs in
            let c = Concrete.Values (List.map Concrete.to_entry vs) in
            k hash c
        | `Inode (length, proofs) -> concrete_of_inode ~length ~depth proofs k

      and concrete_of_inode :
          type a.
          length:int ->
          depth:int ->
          (int * t) list ->
          (hash -> Concrete.t -> a) ->
          a =
       fun ~length ~depth proofs k ->
        let rec aux ps es = function
          | [] ->
              let c = Concrete.Tree { depth; length; pointers = ps } in
              let hash = hash_inode ~depth ~length es in
              k hash c
          | (index, proof) :: proofs ->
              concrete_of_proof ~depth:(depth + 1) proof (fun pointer tree ->
                  let ps = { Concrete.tree; pointer; index } :: ps in
                  let h = Val_ref.of_hash (Lazy.from_val pointer) in
                  let es = (index, Broken h) :: es in
                  aux ps es proofs)
        in
        aux [] [] (List.rev proofs)

      let proof_of_concrete h p = proof_of_concrete h p Fun.id
      let concrete_of_proof ~depth p = concrete_of_proof ~depth p (fun _ t -> t)

      let to_proof la t : t =
        let p =
          if is_stable t then
            (* To preserve the stable hash, the proof needs to contain
               all the underlying values. *)
            let bindings =
              seq la t
              |> Seq.map Concrete.to_entry
              |> List.of_seq
              |> List.fast_sort (fun x y ->
                     compare_step x.Concrete.name y.Concrete.name)
            in
            Concrete.Values bindings
          else to_concrete ~force:false la t
        in
        proof_of_concrete (Val_ref.to_lazy_hash t.v_ref) p

      let of_proof (Partial _ as la) ~depth (proof : t) =
        match proof with
        | `Values vs when List.compare_length_with vs Conf.entries > 0 -> (
            if depth <> 0 then None
            else
              (* [proof] is a big stable inode that was unshallowed and encoded
                 in a [Values], it needs to be converted back to a [Tree]
                 shallowed. *)
              let t =
                of_seq Total (List.map strengthen_step_value vs |> List.to_seq)
              in
              let hash =
                (* Compute the hash right away (not lazily) so that
                   [hash_exn ~force:false] is possible on the result of
                   [of_proof]. *)
                hash t
              in
              let v_ref = Val_ref.of_hash (Lazy.from_val hash) in
              match t.v with
              | Values _ -> assert false
              | Tree { depth; length; entries } ->
                  let ptr_of_key = Ptr.of_key la in
                  let entries =
                    Array.map
                      (function
                        | None -> None
                        | Some ptr ->
                            let hash =
                              Ptr.val_ref Total ptr |> Val_ref.to_hash
                            in
                            (* Since [of_proof] is only called in the context of
                               Portable inodes, [unfindable_of_hash] is safe. *)
                            let key = Key.unfindable_of_hash hash in
                            Some (ptr_of_key key))
                      entries
                  in
                  let v = Tree { depth; length; entries } in
                  let t = { v_ref; v; root = true } in
                  Some t)
        | _ -> (
            let c = concrete_of_proof ~depth proof in
            match of_concrete la ~depth c with
            | Ok v -> Some v
            | Error _ -> None)

      let of_concrete t = proof_of_concrete (lazy (failwith "blinded root")) t
      let to_concrete = concrete_of_proof ~depth:0
    end

    module Snapshot = struct
      include T

      type kinded_hash = Contents of hash * metadata | Node of hash
      [@@deriving irmin]

      type entry = { step : string; hash : kinded_hash } [@@deriving irmin]

      type inode_tree = {
        depth : int;
        length : int;
        pointers : (int * hash) list;
      }
      [@@deriving irmin]

      type v = Inode_tree of inode_tree | Inode_value of entry list
      [@@deriving irmin]

      type inode = { v : v; root : bool } [@@deriving irmin]
    end

    let of_entry ~index e : step * Node.value =
      let step =
        match T.step_of_bin_string e.Snapshot.step with
        | Ok s -> s
        | Error (`Msg m) -> Fmt.failwith "step of bin error: %s" m
      in
      ( step,
        match e.hash with
        | Snapshot.Contents (hash, m) ->
            let key = index hash in
            `Contents (key, m)
        | Node hash ->
            let key = index hash in
            `Node key )

    let of_inode_tree ~index layout tr =
      let entries = Array.make Conf.entries None in
      let ptr_of_key hash =
        let key = index hash in
        Ptr.of_key layout key
      in
      List.iter
        (fun (index, pointer) -> entries.(index) <- Some (ptr_of_key pointer))
        tr.Snapshot.pointers;
      { depth = tr.depth; length = tr.length; entries }

    let of_snapshot ~index layout (v : Snapshot.inode) =
      let t =
        match v.v with
        | Inode_value vs ->
            values layout (StepMap.of_list (List.map (of_entry ~index) vs))
        | Inode_tree tr -> tree layout (of_inode_tree ~index layout tr)
      in
      if v.root then stabilize_root layout t else t
  end

  module Raw = struct
    type hash = H.t [@@deriving irmin]
    type key = Key.t
    type t = T.key Bin.t [@@deriving irmin]
    type metadata = T.metadata [@@deriving irmin]
    type Pack_value.kinded += Node of t

    let to_kinded t = Node t
    let of_kinded = function Node n -> n | _ -> assert false
    let depth = Bin.depth

    exception Invalid_depth of { expected : int; got : int; v : t }

    let kind (t : t) =
      (* This is the kind of newly appended values, let's use v2 then *)
      if t.root then Pack_value.Kind.Inode_v2_root
      else Pack_value.Kind.Inode_v2_nonroot

    let repr_size = Mem.repr_size t

    (** [repr_size] undercounts the size of an inode by around this factor.

        A value of 4.5 was empirically observed by averaging the ratio between
        [Mem.reachable_bytes] and [repr_size] during a few runs of a trace
        replay. This value is rounded to 5 to prevent float-int conversion
        during weight calculation, at the expense of letting fewer objects into
        the LRU. *)
    let repr_size_adjustment = 5

    let weight t =
      Pack_value.Deferred (fun () -> repr_size_adjustment * repr_size t)

    let hash t = Bin.hash t
    let step_to_bin = T.step_to_bin_string
    let step_of_bin = T.step_of_bin_string
    let encode_compress = Irmin.Type.(unstage (encode_bin Compress.t))
    let decode_compress = Irmin.Type.(unstage (decode_bin Compress.t))

    let length_header = function
      | Pack_value.Kind.Contents ->
          (* NOTE: the Node instantiation of the pack store must have access to
             the header format used by contents values in order to eagerly
             construct contents keys with length information during
             [key_of_offset]. *)
          Conf.contents_length_header
      | k -> Pack_value.Kind.length_header_exn k

    let decode_compress_length =
      match Irmin.Type.Size.of_encoding Compress.t with
      | Unknown | Static _ -> assert false
      | Dynamic f -> f

    let encode_bin :
        dict:(string -> int option) ->
        offset_of_key:(Key.t -> int63 option) ->
        hash ->
        t Irmin.Type.encode_bin =
     fun ~dict ~offset_of_key hash t ->
      Stats.incr_inode_encode_bin ();
      let step s : Compress.name =
        let str = step_to_bin s in
        if String.length str <= 3 then Direct s
        else match dict str with Some i -> Indirect i | None -> Direct s
      in
      let address_of_key key : Compress.address =
        match offset_of_key key with
        | Some off -> Compress.Offset off
        | None ->
            (* The key references an inode/contents that is not in the pack
                file. This is highly unusual but not forbidden. *)
            Compress.Hash (Key.to_hash key)
      in
      let ptr : T.key Bin.with_index -> Compress.ptr =
       fun n ->
        let hash = address_of_key n.vref in
        { index = n.index; hash }
      in
      let value : T.step * T.value -> Compress.value = function
        | s, `Contents (c, m) ->
            let s = step s in
            let v = address_of_key c in
            Compress.Contents (s, v, m)
        | s, `Node n ->
            let s = step s in
            let v = address_of_key n in
            Compress.Node (s, v)
      in
      (* List.map is fine here as the number of entries is small *)
      let v : T.key Bin.v -> Compress.v = function
        | Values vs -> Values (List.map value vs)
        | Tree { depth; length; entries } ->
            let entries = List.map ptr entries in
            Tree { Compress.depth; length; entries }
      in
      let t = Compress.v ~root:t.root ~hash (v t.v) in
      encode_compress t

    exception Exit of [ `Msg of string ]

    let decode_bin :
        dict:(int -> string option) ->
        key_of_offset:(int63 -> key) ->
        key_of_hash:(hash -> key) ->
        t Irmin.Type.decode_bin =
     fun ~dict ~key_of_offset ~key_of_hash t pos_ref ->
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
        | Offset off -> key_of_offset off
        | Hash n -> key_of_hash n
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
      Bin.v ~root ~hash:(Lazy.from_val i.hash) v

    let decode_bin_length = decode_compress_length

    let decode_children_offsets ~entry_of_offset ~entry_of_hash t pos_ref =
      let i = decode_compress t pos_ref in
      let { Compress.tv; _ } = i in
      let v =
        match tv with
        | V0_stable v | V0_unstable v -> v
        | V1_root { v; _ } | V1_nonroot { v; _ } -> v
      in
      let entry_of_address = function
        | Compress.Offset offset -> entry_of_offset offset
        | Hash h -> entry_of_hash h
      in
      match v with
      | Values ls ->
          List.map
            (function
              | Compress.Contents (_, address, _) | Node (_, address) ->
                  entry_of_address address)
            ls
      | Tree { entries; _ } ->
          List.map
            (function ({ hash; _ } : Compress.ptr) -> entry_of_address hash)
            entries

    module Snapshot = Val_impl.Snapshot

    let to_entry : T.step * Node.value -> Snapshot.entry =
     fun (name, v) ->
      let step = step_to_bin name in
      match v with
      | `Contents (contents_key, m) ->
          let h = Key.to_hash contents_key in
          { Snapshot.step; hash = Contents (h, m) }
      | `Node node_key ->
          let h = Key.to_hash node_key in
          { step; hash = Node h }

    (* The implementation of [of_snapshot] is in the module [Val]. This is
       because we cannot compute the hash of a root from [Bin]. *)
    let to_snapshot : t -> Snapshot.inode =
     fun t ->
      match t.v with
      | Bin.Tree tree ->
          let inode_tree =
            {
              Snapshot.depth = tree.depth;
              length = tree.length;
              pointers =
                List.map
                  (fun { Bin.index; vref } ->
                    let hash = Key.to_hash vref in
                    (index, hash))
                  tree.entries;
            }
          in
          { v = Inode_tree inode_tree; root = t.root }
      | Values vs ->
          let vs = List.map to_entry vs in
          let v = Snapshot.Inode_value vs in
          { v; root = t.root }
  end

  module Snapshot = Val_impl.Snapshot

  let to_snapshot = Raw.to_snapshot

  type hash = T.hash
  type key = Key.t

  let pp_hash = T.pp_hash

  module Val_portable = struct
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
      Total (I.of_seq Total l)

    let of_list l = of_seq (List.to_seq l)

    let seq ?offset ?length ?cache t =
      apply t { f = (fun layout v -> I.seq layout ?offset ?length ?cache v) }

    let list ?offset ?length ?cache t =
      apply t { f = (fun layout v -> I.list layout ?offset ?length ?cache v) }

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
          let vs =
            (* If [x] is shallow, this [seq] call will perform IOs. *)
            seq x
          in
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

    let hash_exn ?force t = apply t { f = (fun _ v -> I.hash_exn ?force v) }

    let save ?(allow_non_root = false) ~add ~index ~mem t =
      if Conf.forbid_empty_dir_persistence && is_empty t then
        failwith
          "Persisting an empty node is forbidden by the configuration of the \
           irmin-pack store";
      let f layout v =
        if not allow_non_root then I.check_write_op_supported v;
        I.save layout ~add ~index ~mem v
      in
      apply t { f }

    let of_raw (find' : expected_depth:int -> key -> key Bin.t option) v =
      Stats.incr_inode_of_raw ();
      let rec find ~expected_depth h =
        Option.map (I.of_bin layout) (find' ~expected_depth h)
      and layout = I.Partial find in
      Partial (layout, I.of_bin layout v)

    let recompute_hash t =
      apply t { f = (fun layout v -> I.recompute_hash layout v) }

    let to_raw t =
      apply t { f = (fun layout v -> I.to_bin layout Bin.Ptr_key v) }

    let stable t = apply t { f = (fun _ v -> I.is_stable v) }
    let length t = apply t { f = (fun _ v -> I.length v) }
    let clear t = apply t { f = (fun layout v -> I.clear layout v) }
    let nb_children t = apply t { f = (fun _ v -> I.nb_children v) }
    let index ~depth s = I.index ~depth (Child_ordering.key s)

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

    let merge ~contents ~node : t Irmin.Merge.t =
      let merge = Node.merge ~contents ~node in
      let to_node t = of_seq (Node.seq t) in
      let of_node n = Node.of_seq (seq n) in
      Irmin.Merge.like t merge of_node to_node

    let with_handler f_env t =
      match t with
      | Total _ -> t
      | Truncated _ -> t
      | Partial ((I.Partial find as la), v) ->
          (* [f_env] works on [Val.t] while [find] in [Partial find] works on
             [Val_impl.t], hence the following wrapping (before applying
             [f_env]) and unwrapping (after [f_env]). *)
          let find_v ~expected_depth h =
            match find ~expected_depth h with
            | None -> None
            | Some v -> Some (Partial (la, v))
          in
          let find = f_env find_v in
          let find_ptr ~expected_depth h =
            match find ~expected_depth h with
            | Some (Partial (_, v)) -> Some v
            | _ -> None
          in
          let la = I.Partial find_ptr in
          Partial (la, v)

    let head t =
      let f la (v : _ I.t) =
        if Val_impl.is_stable v then
          (* To preserve the stable hash, the proof needs to contain
             all the underlying values. *)
          let elts =
            I.seq la v
            |> List.of_seq
            |> List.fast_sort (fun (x, _) (y, _) -> compare_step x y)
          in
          `Node elts
        else
          match v.v with
          | I.Values n -> `Node (List.of_seq (StepMap.to_seq n))
          | I.Tree v ->
              let entries = ref [] in
              for i = Array.length v.entries - 1 downto 0 do
                match v.entries.(i) with
                | None -> ()
                | Some ptr ->
                    let h = I.Ptr.val_ref la ptr |> Val_ref.to_hash in
                    entries := (i, h) :: !entries
              done;
              `Inode (v.length, !entries)
      in
      apply t { f }
  end

  module Val = struct
    include Val_portable

    module Portable = struct
      include Val_portable

      type node_key = hash [@@deriving irmin]
      type contents_key = hash [@@deriving irmin]

      type value = [ `Contents of hash * metadata | `Node of hash ]
      [@@deriving irmin]

      let of_node t = t

      let of_list bindings =
        bindings
        |> List.map (fun (k, v) -> (k, unsafe_keyvalue_of_hashvalue v))
        |> of_list

      let of_seq bindings =
        bindings
        |> Seq.map (fun (k, v) -> (k, unsafe_keyvalue_of_hashvalue v))
        |> of_seq

      let seq ?offset ?length ?cache t =
        seq ?offset ?length ?cache t
        |> Seq.map (fun (k, v) -> (k, hashvalue_of_keyvalue v))

      let add : t -> step -> value -> t =
       fun t s v -> add t s (unsafe_keyvalue_of_hashvalue v)

      let list ?offset ?length ?cache t =
        list ?offset ?length ?cache t
        |> List.map (fun (s, v) -> (s, hashvalue_of_keyvalue v))

      let find ?cache t s = find ?cache t s |> Option.map hashvalue_of_keyvalue

      let merge =
        let promote_merge :
            hash option Irmin.Merge.t -> key option Irmin.Merge.t =
         fun t ->
          Irmin.Merge.like [%typ: key option] t (Option.map Key.to_hash)
            (Option.map Key.unfindable_of_hash)
        in
        fun ~contents ~node ->
          merge ~contents:(promote_merge contents) ~node:(promote_merge node)

      module Proof = I.Proof

      type proof = I.Proof.t [@@deriving irmin]

      let to_proof (t : t) : proof =
        apply t { f = (fun la v -> I.Proof.to_proof la v) }

      let of_proof ~depth (p : proof) =
        let find ~expected_depth:_ k =
          raise_dangling_hash "of_proof@find" (Key.to_hash k)
        in
        (* A [Partial] should be built instead of a [Truncated] because we need a
           [find] function that will be hooked by the proof env and that will
           raise the above exception in case of miss in the env. *)
        let la = I.Partial find in
        Option.map (fun v -> Partial (la, v)) (I.Proof.of_proof la ~depth p)

      type 'a find = expected_depth:int -> 'a -> t option

      let with_handler : (hash find -> hash find) -> t -> t =
        let to_hash : key find -> hash find =
         fun find ~expected_depth h ->
          find ~expected_depth (Key.unfindable_of_hash h)
        in
        let to_key : hash find -> key find =
         fun find ~expected_depth k -> find ~expected_depth (Key.to_hash k)
        in
        fun f_env t ->
          with_handler (fun find -> find |> to_hash |> f_env |> to_key) t

      let head t =
        match head t with
        | `Inode _ as x -> x
        | `Node l -> `Node (List.map Proof.weaken_step_value l)
    end

    let to_concrete t =
      apply t { f = (fun la v -> I.to_concrete ~force:true la v) }

    let of_concrete t =
      match I.of_concrete Truncated ~depth:0 t with
      | Ok t -> Ok (Truncated t)
      | Error _ as e -> e

    module Snapshot = I.Snapshot
    module Concrete = I.Concrete

    let of_snapshot t ~index find' =
      let rec find ~expected_depth h =
        match find' ~expected_depth h with
        | None -> None
        | Some v -> Some (I.of_bin layout v)
      and layout = I.Partial find in
      Partial (layout, I.of_snapshot layout t ~index)
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
                and type Snapshot.metadata = Node.metadata
                and type Val.step = Node.step)
    (Pack : Indexable.S
              with type hash = H.t
               and type key = Key.t
               and type value = Inter.Raw.t) =
struct
  module Hash = H
  module Key = Key
  module Val = Inter.Val

  type 'a t = 'a Pack.t
  type key = Key.t [@@deriving irmin ~equal]
  type hash = Hash.t
  type value = Inter.Val.t

  let mem t k = Pack.mem t k
  let index t k = Pack.index t k

  exception Invalid_depth = Inter.Raw.Invalid_depth

  let pp_value = Irmin.Type.pp Inter.Raw.t

  let pp_invalid_depth ppf (expected, got, v) =
    Fmt.pf ppf "Invalid depth: got %d, expecting %d (%a)" got expected pp_value
      v

  let check_depth_opt ~expected_depth:expected = function
    | None -> ()
    | Some v -> (
        match Inter.Raw.depth v with
        | None -> ()
        | Some got ->
            if got <> expected then raise (Invalid_depth { expected; got; v }))

  let unsafe_find ~check_integrity t k =
    match Pack.unsafe_find ~check_integrity t k with
    | None -> None
    | Some v ->
        let find ~expected_depth k =
          let v = Pack.unsafe_find ~check_integrity t k in
          check_depth_opt ~expected_depth v;
          v
        in
        let v = Val.of_raw find v in
        Some v

  let find t k = unsafe_find ~check_integrity:true t k |> Lwt.return

  let save ?allow_non_root t v =
    let add k v =
      Pack.unsafe_append ~ensure_unique:true ~overcommit:false t k v
    in
    Val.save ?allow_non_root ~add ~index:(Pack.index_direct t)
      ~mem:(Pack.unsafe_mem t) v

  let hash_exn = Val.hash_exn
  let add t v = Lwt.return (save t v)
  let equal_hash = Irmin.Type.(unstage (equal H.t))

  let check_hash expected got =
    if equal_hash expected got then ()
    else
      Fmt.invalid_arg "corrupted value: got %a, expecting %a" Inter.pp_hash
        expected Inter.pp_hash got

  let unsafe_add t k v =
    check_hash k (hash_exn v);
    Lwt.return (save t v)

  let batch = Pack.batch
  let close = Pack.close
  let decode_bin_length = Inter.Raw.decode_bin_length

  let protect_from_invalid_depth_exn f =
    Lwt.catch f (function
      | Invalid_depth { expected; got; v } ->
          let msg = Fmt.to_to_string pp_invalid_depth (expected, got, v) in
          Lwt.return (Error msg)
      | e -> Lwt.fail e)

  let integrity_check_inodes t k =
    protect_from_invalid_depth_exn @@ fun () ->
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
