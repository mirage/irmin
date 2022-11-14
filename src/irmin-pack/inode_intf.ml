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

module type Child_ordering = sig
  type step
  type key

  val key : step -> key
  val index : depth:int -> key -> int
end

module type Snapshot = sig
  type hash
  type metadata

  type kinded_hash = Contents of hash * metadata | Node of hash
  [@@deriving irmin]

  type entry = { step : string; hash : kinded_hash } [@@deriving irmin]

  type inode_tree = { depth : int; length : int; pointers : (int * hash) list }
  [@@deriving irmin]

  type v = Inode_tree of inode_tree | Inode_value of entry list
  [@@deriving irmin]

  type inode = { v : v; root : bool } [@@deriving irmin]
end

module type Value = sig
  type key

  include
    Irmin.Node.Generic_key.S
      with type node_key = key
       and type contents_key = key

  val pred :
    t ->
    (step option
    * [ `Node of node_key | `Inode of node_key | `Contents of contents_key ])
    list

  module Portable :
    Irmin.Node.Portable.S
      with type node := t
       and type hash = hash
       and type step := step
       and type metadata := metadata

  val nb_children : t -> int

  val recompute_hash : t -> hash
  (** Recompute hash for inodes, used in eager integrity checks.*)
end

module type Raw = sig
  include Pack_value.S

  val depth : t -> int option

  exception Invalid_depth of { expected : int; got : int; v : t }

  val decode_children_offsets :
    entry_of_offset:(int63 -> 'a) ->
    entry_of_hash:(hash -> 'a) ->
    string ->
    int ref ->
    'a list
end

module type S = sig
  include Irmin.Indexable.S
  module Hash : Irmin.Hash.S with type t = hash

  val unsafe_find : check_integrity:bool -> [< read ] t -> key -> value option

  module Val :
    Value
      with type t = value
       and type key = key
       and type hash = Hash.t
       and type Portable.hash := hash

  val decode_bin_length : string -> int -> int
  val integrity_check_inodes : [ `Read ] t -> key -> (unit, string) result Lwt.t
  val save : ?allow_non_root:bool -> 'a t -> value -> key
end

module type Compress = sig
  type step
  type hash
  type metadata
  type dict_key = int
  type pack_offset = int63
  type name = Indirect of dict_key | Direct of step
  type address = Offset of pack_offset | Hash of hash
  type ptr = { index : dict_key; hash : address }
  type tree = { depth : dict_key; length : dict_key; entries : ptr list }
  type value = Contents of name * address * metadata | Node of name * address
  type v = Values of value list | Tree of tree
  type v1 = { mutable length : int; v : v }

  type tagged_v =
    | V0_stable of v
    | V0_unstable of v
    | V1_root of v1
    | V1_nonroot of v1

  type t = { hash : hash; tv : tagged_v } [@@deriving irmin]
end

(** Unstable internal API agnostic about the underlying storage. Use it only to
    implement or test inodes. *)
module type Internal = sig
  type hash
  type key

  val pp_hash : hash Fmt.t

  module Snapshot : Snapshot with type hash = hash
  module Raw : Raw with type hash = hash and type key = key

  module Val : sig
    include
      Value
        with type hash = hash
         and type key = key
         and type metadata = Snapshot.metadata

    val of_raw : (expected_depth:int -> key -> Raw.t option) -> Raw.t -> t
    val to_raw : t -> Raw.t

    val save :
      ?allow_non_root:bool ->
      add:(hash -> Raw.t -> key) ->
      index:(hash -> key option) ->
      mem:(key -> bool) ->
      t ->
      key

    val stable : t -> bool
    val length : t -> int
    val index : depth:int -> step -> int

    val integrity_check : t -> bool
    (** Checks the integrity of an inode. *)

    module Concrete : sig
      (** {1 Concrete trees} *)

      (** The type for pointer kinds. *)
      type kinded_key =
        | Contents of contents_key
        | Contents_x of metadata * contents_key
        | Node of node_key
      [@@deriving irmin]

      type entry = { name : step; key : kinded_key } [@@deriving irmin]
      (** The type of entries. *)

      type 'a pointer = { index : int; pointer : hash; tree : 'a }
      [@@deriving irmin]
      (** The type for internal pointers between concrete {!tree}s. *)

      type 'a tree = { depth : int; length : int; pointers : 'a pointer list }
      [@@deriving irmin]
      (** The type for trees. *)

      (** The type for concrete trees. *)
      type t = Tree of t tree | Values of entry list | Blinded
      [@@deriving irmin]

      type len := [ `Eq of int | `Ge of int ]

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
      (** The type for errors. *)

      val pp_error : error Fmt.t
      (** [pp_error] is the pretty-printer for errors. *)
    end

    val to_concrete : t -> Concrete.t
    (** [to_concrete t] is the concrete inode tree equivalent to [t]. *)

    val of_concrete : Concrete.t -> (t, Concrete.error) result
    (** [of_concrete c] is [Ok t] iff [c] and [t] are equivalent.

        The result is [Error e] when a subtree tree of [c] has an integrity
        error. *)

    module Portable : sig
      (* Extend to the portable signature *)
      include module type of Portable

      module Proof : sig
        val of_concrete : Concrete.t -> proof

        val to_concrete : proof -> Concrete.t
        (** This function produces unfindable keys. Only use in tests *)
      end
    end

    val of_snapshot :
      Snapshot.inode ->
      index:(hash -> key) ->
      (expected_depth:int -> key -> Raw.t option) ->
      t
  end

  val to_snapshot : Raw.t -> Snapshot.inode

  module Compress :
    Compress
      with type hash := hash
       and type step := Val.step
       and type metadata := Val.metadata

  module Child_ordering : Child_ordering with type step := Val.step
end

module type Sigs = sig
  module type S = S
  module type Internal = Internal
  module type Child_ordering = Child_ordering
  module type Raw = Raw
  module type Snapshot = Snapshot

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
                 and type node_key = Key.t) :
    Internal
      with type hash = H.t
       and type key = Key.t
       and type Snapshot.metadata = Node.metadata
       and type Val.step = Node.step

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
                with type key = Key.t
                 and type hash = H.t
                 and type value = Inter.Raw.t) :
    S
      with type 'a t = 'a Pack.t
       and type key = Key.t
       and type hash = H.t
       and type Val.metadata = Node.metadata
       and type Val.step = Node.step
       and type value = Inter.Val.t
end
