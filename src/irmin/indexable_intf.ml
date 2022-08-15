(*
 * Copyright (c) 2021 Craig Ferguson <craig@tarides.com>
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
open Store_properties

module type S_without_key_impl = sig
  include Read_only.S
  (** @inline *)

  type hash
  (** The type of hashes of [value]. *)

  val add : [> write ] t -> value -> key Lwt.t
  (** Write the contents of a value to the store, and obtain its key. *)

  val unsafe_add : [> write ] t -> hash -> value -> key Lwt.t
  (** Same as {!add} but allows specifying the value's hash directly. The
      backend might choose to discard that hash and/or can be corrupt if the
      hash is not consistent. *)

  val index : [> read ] t -> hash -> key option Lwt.t
  (** Indexing maps the hash of a value to a corresponding key of that value in
      the store. For stores that are addressed by hashes directly, this is
      typically [fun _t h -> Lwt.return (Key.of_hash h)]; for stores with more
      complex addressing schemes, [index] may attempt a lookup operation in the
      store.

      In general, indexing is best-effort and reveals no information about the
      membership of the value in the store. In particular:

      - [index t hash = Some key] doesn't guarantee [mem t key]: the value with
        hash [hash] may still be absent from the store;

      - [index t hash = None] doesn't guarantee that there is no [key] such that
        [mem t key] and [Key.to_hash key = hash]: the value may still be present
        in the store under a key that is not indexed. *)

  include Batch with type 'a t := 'a t
  (** @inline *)
end

module type S = sig
  (** An {i indexable} store is a read-write store in which values can be added
      and later found via their keys.

      Keys are not necessarily portable between different stores, so each store
      provides an {!val-index} mechanism to find keys by the hashes of the
      values they reference. *)

  include S_without_key_impl (* @inline *)
  module Key : Key.S with type t = key and type hash = hash
end

module type Maker = functor (Hash : Hash.S) (Value : Type.S) -> sig
  include S with type value = Value.t and type hash = Hash.t

  include Of_config with type 'a t := 'a t
  (** @inline *)
end

(** A {!Maker_concrete_key} is an indexable store in which the key type is
    uniquely determined by the hash type and is stated up-front. *)
module type Maker_concrete_key1 = sig
  type 'h key

  module Key : functor (Hash : Hash.S) ->
    Key.S with type t = Hash.t key and type hash = Hash.t

  module Make : functor (Hash : Hash.S) (Value : Type.S) -> sig
    include
      S
        with type value = Value.t
         and type hash = Hash.t
         and type key = Hash.t key

    include Of_config with type 'a t := 'a t
    (** @inline *)
  end
end

(** Like {!Maker_concrete_key1}, but the key type may also depend on type of the
    value that it references. *)
module type Maker_concrete_key2 = sig
  type ('h, 'v) key

  module Key : functor (Hash : Hash.S) (Value : Type.S) ->
    Key.S with type t = (Hash.t, Value.t) key and type hash = Hash.t

  module Make : functor (Hash : Hash.S) (Value : Type.S) -> sig
    include
      S
        with type value = Value.t
         and type hash = Hash.t
         and type key = (Hash.t, Value.t) key

    include Of_config with type 'a t := 'a t
    (** @inline *)
  end
end

module type Sigs = sig
  module type S = S
  module type S_without_key_impl = S_without_key_impl
  module type Maker = Maker
  module type Maker_concrete_key1 = Maker_concrete_key1
  module type Maker_concrete_key2 = Maker_concrete_key2

  module Maker_concrete_key2_of_1 (X : Maker_concrete_key1) :
    Maker_concrete_key2 with type ('h, _) key = 'h X.key

  module Of_content_addressable
      (Key : Type.S)
      (S : Content_addressable.S with type key = Key.t) :
    S
      with type 'a t = 'a S.t
       and type key = Key.t
       and type hash = Key.t
       and type value = S.value

  module Check_closed_store (CA : S) : sig
    include
      S with type key = CA.key and type hash = CA.hash and type value = CA.value

    val make_closeable : 'a CA.t -> 'a t
    (** [make_closeable t] returns a version of [t] that raises {!Irmin.Closed}
        if an operation is performed when it is already closed. *)

    val get_if_open_exn : 'a t -> 'a CA.t
    (** [get_if_open_exn t] returns the store (without close checks) if it is
        open; otherwise raises {!Irmin.Closed} *)
  end

  module Check_closed (M : Maker) : Maker
end
