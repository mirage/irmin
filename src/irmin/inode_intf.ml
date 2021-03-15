(*
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

open S

module type VAL = sig
  include Node.S

  val pred : t -> [ `Node of hash | `Inode of hash | `Contents of hash ] list
end

module type INODE_CONTENT_ADDRESSABLE_STORE = sig
  include Node.NODE_CONTENT_ADDRESSABLE_STORE
  module Val : VAL with type t = value and type hash = key
end

module type BIN = sig
  type hash
  type step
  type value
  type ptr = { index : int; hash : hash }
  type tree = { depth : int; length : int; entries : ptr list }
  type v = Values of (step * value) list | Tree of tree
  type t = { hash : hash Lazy.t; stable : bool; v : v }

  val t : t Type.ty
  val hash : t -> hash
  val v : stable:bool -> hash:hash Lazy.t -> v -> t
end

module type S = sig
  module Bin : BIN

  module Val : sig
    type t
    type metadata
    type hash

    include
      VAL
        with type hash := hash
         and type t := t
         and type metadata := metadata
         and type value = [ `Node of hash | `Contents of hash * metadata ]

    val pp_hash : hash Fmt.t
    val of_bin : (hash -> Bin.t option) -> Bin.t -> t
    val to_bin : t -> Bin.t
    val save : add:(hash -> Bin.t -> unit) -> mem:(hash -> bool) -> t -> unit
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
      type t = Tree of t tree | Value of entry list [@@deriving irmin]

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
  end
end

module type Inode = sig
  module type VAL = VAL
  module type BIN = BIN
  module type S = S
  module type INODE_CONTENT_ADDRESSABLE_STORE = INODE_CONTENT_ADDRESSABLE_STORE

  module Make
      (Conf : INODE_CONF)
      (H : Hash.S)
      (Node : Node.S with type hash = H.t) :
    S
      with type Val.hash = H.t
       and type Val.step = Node.step
       and type Val.metadata = Node.metadata
       and type Val.value = [ `Node of H.t | `Contents of H.t * Node.metadata ]
       and type Bin.hash = Node.hash
       and type Bin.step = Node.step
       and type Bin.value = [ `Node of H.t | `Contents of H.t * Node.metadata ]

  module Make_store
      (H : Hash.S)
      (Inter : S with type Val.hash = H.t and type Bin.hash = H.t)
      (Unsafe_CA : S.UNSAFE_CONTENT_ADDRESSABLE_STORE_MAKER) :
    INODE_CONTENT_ADDRESSABLE_STORE
      with type key = H.t
       and type value = Inter.Val.t
       and type Val.metadata = Inter.Val.metadata
       and type Val.step = Inter.Val.step
end
