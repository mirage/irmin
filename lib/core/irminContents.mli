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

(** Values. *)

open Core_kernel.Std

exception Invalid of string
(** Invalid parsing. *)

exception Conflict
(** Exception raised during merge conflicts. *)

val default_merge: compare:('a -> 'a -> int) -> old:'a option -> 'a -> 'a -> 'a
(** The default merge function, using the given comparator
    function. It checks if the two values are equals, or if only one
    of the two values has been modified. It raises [Conflict] in the
    other cases. *)

module type S = sig

  (** Signature for store contents. *)

  include Identifiable.S
  (** Base types. *)

  val to_json: t -> Ezjsonm.t
  (** Convert the contents to JSON. *)

  val of_json: Ezjsonm.t -> t
  (** Read some JSON encoded contents. *)

  val of_bytes: string -> t option
  (** Convert a raw sequence of bytes into structured contents. Return
      [None] if the sequence cannot be decoded. *)

  val of_bytes_exn: string -> t
  (** Same as [of_bytes] but raise [Invalid] if the sequence of bytes
      does not correspond to some valid contents. *)

  val merge: old:t option -> t -> t -> t
  (** Merge function. Raise [Conflict] if the values cannot be
      merged properly. *)

end

module String: S with type t = string
(** String values where only the last modified value is kept on
    merge. If the value has been modified concurrently, the [merge]
    function raises [Conflict]. *)

module JSON: S with type t = Ezjsonm.t
(** JSON values where only the last modified value is kept on
    merge. If the value has been modified concurrently, the [merge]
    function raises [Conflict]. *)

(** JSON values. *)
module type STORE = sig

  include IrminStore.AO
  (** Contents stores are append-only. *)

  module Key: IrminKey.S with type t = key
  (** Base functions for foreign keys. *)

  module Value: S with type t = value
  (** Base functions for values. *)

end
(** Blobs are stored in append-only stores. *)

module Make
    (K: IrminKey.S)
    (C: S)
    (Contents: IrminStore.AO with type key = K.t and type value = C.t)
  : STORE with type t = Contents.t
           and type key = K.t
           and type value = C.t
(** Build a contents store. *)
