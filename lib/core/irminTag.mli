(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Tags handling. *)

(** Tags are branch pointers: they associad branch names to keys in
    the block store. *)

open IrminSig

module type S = sig

  (** Signature for mutable tags. *)

  include IrminKey.S

  val master: t
  (** The master branch. *)

end

module String: S with type t = string
(** Simple string tags. *)

(** {2 Store} *)

module type STORE = sig

  (** The *tag store* is a key / value store, where keys are names
      created by users (and/or global names created by convention) and
      values are keys from the block store.

      A typical Irminsule application should have a very low number of
      keys in the tag store, are this store is not supposed to be
      really efficient.  *)

  include IrminSig.RW

  module Key: S with type t = key
  (** Base functions over keys. *)

  module Value: Key with type t = value
  (** Base functions over values. *)

end

module Make
    (K: S)
    (V: Key)
    (S: RW with type key = K.t and type value = V.t)
  : STORE with type t = S.t
           and type key = K.t
           and type value = V.t
(** Build a tag store. *)
