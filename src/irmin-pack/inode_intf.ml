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

module type Value = sig
  include Irmin.Node.S

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
end

module type S = sig
  include Irmin.Indexable.S
  module Hash : Irmin.Hash.S with type t = hash

  module Val :
    Value
      with type t = value
       and type hash = Hash.t
       and type Portable.hash := hash

  val decode_bin_length : string -> int -> int
end

module type Persistent = sig
  include S

  type index

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    string ->
    read t Lwt.t

  include S.Checkable with type 'a t := 'a t and type key := key

  val sync : ?on_generation_change:(unit -> unit) -> 'a t -> unit
  val clear_caches : 'a t -> unit
  val integrity_check_inodes : [ `Read ] t -> key -> (unit, string) result Lwt.t
end

(** Unstable internal API agnostic about the underlying storage. Use it only to
    implement or test inodes. *)
module type Internal = sig
  type hash

  val pp_hash : hash Fmt.t

  exception Dangling_hash of { context : string; hash : hash }

  module Raw : Pack_value.S with type hash = hash

  module Val : sig
    include Value with type hash = hash

    val of_raw : (hash -> Raw.t option) -> Raw.t -> t
    val to_raw : t -> Raw.t
    val save : add:(hash -> Raw.t -> unit) -> mem:(hash -> bool) -> t -> unit
    val hash : t -> hash
    val stable : t -> bool
    val length : t -> int
    val index : depth:int -> step -> int

    val integrity_check : t -> bool
    (** Checks the integrity of an inode. *)

    module Concrete : sig
      (** {1 Concrete trees} *)

      (** The type for pointer kinds. *)
      type kind = Contents | Contents_x of metadata | Node [@@deriving irmin]

      type entry = { name : step; kind : kind; hash : hash } [@@deriving irmin]
      (** The type for entries. *)

      type 'a pointer = { index : int; pointer : hash; tree : 'a }
      [@@deriving irmin]
      (** The type for pointers. *)

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

    module Proof : sig
      val of_concrete : Concrete.t -> Portable.proof
      val to_concrete : Portable.proof -> Concrete.t
    end
  end
end

module type Sigs = sig
  module type S = S
  module type Persistent = Persistent
  module type Internal = Internal

  module Make_internal
      (Conf : Conf.S)
      (H : Irmin.Hash.S)
      (Node : Irmin.Node.S with type hash = H.t) :
    Internal
      with type hash = H.t
       and type Val.metadata = Node.metadata
       and type Val.step = Node.step

  module Make
      (H : Irmin.Hash.S)
      (Node : Irmin.Node.S with type hash = H.t)
      (Inter : Internal
                 with type hash = H.t
                  and type Val.metadata = Node.metadata
                  and type Val.step = Node.step)
      (Pack : Content_addressable.S
                with type key = H.t
                 and type value = Inter.Raw.t) :
    S
      with type 'a t = 'a Pack.t
       and type key = H.t
       and type hash = H.t
       and type Val.metadata = Node.metadata
       and type Val.step = Node.step
       and type value = Inter.Val.t

  module Make_persistent
      (H : Irmin.Hash.S)
      (Node : Irmin.Node.S with type hash = H.t)
      (Inter : Internal
                 with type hash = H.t
                  and type Val.metadata = Node.metadata
                  and type Val.step = Node.step)
      (CA : Pack_store.Maker
              with type key = H.t
               and type index = Pack_index.Make(H).t) : sig
    include
      Persistent
        with type key = H.t
         and type hash = H.t
         and type Val.metadata = Node.metadata
         and type Val.step = Node.step
         and type index = Pack_index.Make(H).t
         and type value = Inter.Val.t
  end
end
