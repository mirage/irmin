(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Tags *)

module type S = sig

  (** Signature for tags. Tags are supposed to be easily convertible
      to and from string. *)

  include IrminKey.S

  val master: t
  (** The master branch. *)

end

module Simple: S with type t = private string
(** Simple tags. *)

(** {2 Store} *)

module type STORE = sig

  (** The *tag store* is a key/value store, where keys are names
      created by users (and/or global names created by convention) and
      values are keys from the low-level data-store. The tag data-store
      is neither immutable nor consistent, so it is very different from
      the low-level one.

      A typical Irminsule application should have a very low number of
      keys, are this store is not supposed to be really efficient.  *)

  type key
  (** Type of keys. *)

  type tag
  (** Type of tags. *)

  include IrminStore.M with type key := tag and type value := key
  (** The store tag is mutable. *)

  include S with type t := tag
  (** Type of tags. *)

end

module type MAKER =
  functor (T: S) ->
  functor (K: IrminBase.S) ->
    STORE with type tag = T.t
           and type key = K.t
(** Tag store maker. *)

module Make (M: IrminStore.M_MAKER): MAKER
(** Create a tag store. *)
