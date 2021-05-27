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
  include Irmin.Private.Node.S

  val pred : t -> [ `Node of hash | `Inode of hash | `Contents of hash ] list
end

module type S = sig
  include Irmin.Content_addressable.S

  type index

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    string ->
    read t Lwt.t

  module Key : Irmin.Hash.S with type t = key
  module Val : Value with type t = value and type hash = key
  include S.Checkable with type 'a t := 'a t and type key := key

  val sync : ?on_generation_change:(unit -> unit) -> 'a t -> unit
  val clear_caches : 'a t -> unit

  val decode_bin :
    dict:(int -> string option) -> hash:(int63 -> key) -> string -> int -> int

  val integrity_check_inodes : [ `Read ] t -> key -> (unit, string) result Lwt.t
end

(** Unstable internal API agnostic about the underlying storage. Use it only to
    implement or test inodes. *)
module type Internal = sig
  type hash

  val pp_hash : hash Fmt.t

  module Raw : Content_addressable.Value with type hash = hash

  val decode_raw :
    dict:(int -> string option) ->
    hash:(int63 -> hash) ->
    string ->
    int ->
    int * Raw.t

  module Val : sig
    include Value with type hash = hash

    val of_raw : (hash -> Raw.t option) -> Raw.t -> t
    val to_raw : t -> Raw.t
    val save : add:(hash -> Raw.t -> unit) -> mem:(hash -> bool) -> t -> unit
    val hash : t -> hash
    val stable : t -> bool
    val length : t -> int
    val index : depth:int -> max_entries:int -> step -> int

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

      type 'a tree_node = {
        depth : int;
        length : int;
        pointers : 'a pointer list;
      }
      [@@deriving irmin]
      (** The type for trees. *)

      type tree = Tree of tree tree_node | Value of entry list

      type t = { root : tree; version : version } [@@deriving irmin]
      (** The type for concrete trees. *)

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

module type Sigs = sig
  module type S = S
  module type Internal = Internal

  module Make_internal
      (Conf : Conf.S)
      (H : Irmin.Hash.S)
      (Node : Irmin.Private.Node.S
                with type hash = H.t
                 and type version = Conf.version) :
    Internal
      with type hash = H.t
       and type Val.metadata = Node.metadata
       and type Val.step = Node.step
       and type Val.version = Conf.version

  module Make_ext
      (H : Irmin.Hash.S)
      (Node : Irmin.Private.Node.S with type hash = H.t)
      (Inter : Internal
                 with type hash = H.t
                  and type Val.metadata = Node.metadata
                  and type Val.step = Node.step
                  and type Val.version = Node.version)
      (_ : Content_addressable.Maker
             with type key = H.t
              and type index = Pack_index.Make(H).t) : sig
    include
      S
        with type key = H.t
         and type Val.metadata = Node.metadata
         and type Val.step = Node.step
         and type Val.version = Node.version
         and type index = Pack_index.Make(H).t
         and type value = Inter.Val.t
  end

  module Make
      (Conf : Conf.S)
      (H : Irmin.Hash.S)
      (_ : Content_addressable.Maker
             with type key = H.t
              and type index = Pack_index.Make(H).t)
      (Node : Irmin.Private.Node.S
                with type hash = H.t
                with type version = Conf.version) : sig
    include
      S
        with type key = H.t
         and type Val.metadata = Node.metadata
         and type Val.step = Node.step
         and type Val.version = Node.version
         and type index = Pack_index.Make(H).t
  end
end
