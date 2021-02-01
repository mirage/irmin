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

module type CONFIG = sig
  val entries : int

  val stable_hash : int
end

module type S = sig
  include Irmin.CONTENT_ADDRESSABLE_STORE

  type index

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    string ->
    [ `Read ] t Lwt.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

  module Key : Irmin.Hash.S with type t = key

  module Val : sig
    include Irmin.Private.Node.S with type t = value and type hash = key

    val stable : t -> bool

    val hash : t -> Key.t
  end

  type integrity_error = [ `Wrong_hash | `Absent_value ]

  val integrity_check :
    offset:int64 -> length:int -> key -> 'a t -> (unit, integrity_error) result

  val close : 'a t -> unit Lwt.t

  module Concrete : sig
    (** {1 Concrete trees} *)

    type hash = Key.t [@@deriving irmin]

    type step = Val.step [@@deriving irmin]

    type metadata = Val.metadata [@@deriving irmin]

    (** The type for pointer kinds. *)
    type kind = Contents | Contents_x of metadata | Node [@@deriving irmin]

    type entry = { name : step; kind : kind; hash : hash } [@@deriving irmin]
    (** The type for entries. *)

    type 'a pointer = { index : int; pointer : hash; tree : 'a }
    (** The type for pointers. *)

    val pointer_t : 'a Irmin.Type.t -> 'a pointer Irmin.Type.t

    type 'a tree = { depth : int; length : int; pointers : 'a pointer list }
    (** The type for trees. *)

    val tree_t : 'a Irmin.Type.t -> 'a tree Irmin.Type.t

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

  val to_concrete : Val.t -> Concrete.t
  (** [to_concrete t] is the concrete inode tree equivalent to [t]. *)

  val of_concrete : Concrete.t -> (Val.t, Concrete.error) result
  (** [of_concrete c] is [Ok t] iff [c] and [t] are equivalent.

      The result is [Error e] when a subtree tree of [c] has an integrity error. *)
end

module Make
    (Conf : CONFIG)
    (H : Irmin.Hash.S)
    (P : Pack.MAKER with type key = H.t and type index = Pack_index.Make(H).t)
    (Node : Irmin.Private.Node.S with type hash = H.t) :
  S
    with type key = H.t
     and type Val.metadata = Node.metadata
     and type Val.step = Node.step
     and type index = Pack_index.Make(H).t
