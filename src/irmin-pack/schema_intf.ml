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

open! Import

module type Raw = sig
  type hash [@@deriving irmin]
  type step [@@deriving irmin]
  type metadata [@@deriving irmin]

  type value = [ `Node of hash | `Contents of hash * metadata ]
  [@@deriving irmin]

  type ptr = { index : int; hash : hash } [@@deriving irmin]

  type tree = { depth : int; length : int; entries : ptr list }
  [@@deriving irmin]

  type v = Values of (step * value) list | Tree of tree [@@deriving irmin]
  type t = { hash : hash Lazy.t; stable : bool; v : v } [@@deriving irmin]

  val v : stable:bool -> hash:hash Lazy.t -> v -> t
  val hash_v : v -> hash

  include S.Encodable with type t := t and type hash := hash
end

module type Concrete = sig
  type hash [@@deriving irmin]
  type step [@@deriving irmin]
  type metadata [@@deriving irmin]

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

module type Node = sig
  include Irmin.Node.S
  include S.Encodable with type t := t and type hash := hash

  val integrity_check : t -> bool
  val values : t -> [ `Node of hash | `Inode of hash | `Contents of hash ] list
  val hash : t -> hash
  val stable : t -> bool
  val length : t -> int
  val index : depth:int -> step -> int

  module Concrete :
    Concrete
      with type hash := hash
       and type step := step
       and type metadata := metadata

  val to_concrete : t -> Concrete.t
  (** [to_concrete t] is the concrete inode tree equivalent to [t]. *)

  val of_concrete : Concrete.t -> (t, Concrete.error) result
  (** [of_concrete c] is [Ok t] iff [c] and [t] are equivalent.

      The result is [Error e] when a subtree tree of [c] has an integrity error. *)

  module Raw :
    Raw
      with type hash := hash
       and type step := step
       and type metadata := metadata

  val of_raw : (hash -> Raw.t option) -> Raw.t -> t
  val to_raw : t -> Raw.t
  val save : add:(hash -> Raw.t -> unit) -> mem:(hash -> bool) -> t -> unit
end

module type S = sig
  type node
  type metadata
  type hash
  type step

  module Node :
    Node
      with type t = node
       and type metadata = metadata
       and type hash = hash
       and type step = step

  include
    Irmin.Schema.S
      with type node := node
       and type metadata := metadata
       and type hash := hash
       and type step := step
       and module Node := Node
end

module type Sigs = sig
  module type Node = Node
  module type Raw = Raw
  module type S = S
end
