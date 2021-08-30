(*
 * Copyright (c) 2021 Craig Ferguson <craig@tarides.com>
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

  include Clearable with type 'a t := 'a t
  (** @inline *)

  include Batch with type 'a t := 'a t
  (** @inline *)
end

module type S = sig
  (** An {i indexable} store is a read-write store in which values can be added
      and later found via their keys.

      Keys are not necessarily portable between different stores, so each store
      provides an {!index} mechanism to find keys by the hashes of the values
      they reference. *)

  include S_without_key_impl (* @inline *)

  module Key : Key.S with type t = key and type hash = hash
end

module type Maker = functor (Hash : Hash.S) (Value : Type.S) -> sig
  include S with type value = Value.t and type hash = Hash.t

  include Of_config with type 'a t := 'a t
  (** @inline *)
end

module type Sigs = sig
  module type S = S
  module type S_without_key_impl = S_without_key_impl
  module type Maker = Maker

  module Of_content_addressable
      (Key : Type.S)
      (S : Content_addressable.S with type key = Key.t) :
    S
      with type 'a t = 'a S.t
       and type key = Key.t
       and type hash = Key.t
       and type value = S.value

  module Check_closed (M : Maker) : Maker
end
