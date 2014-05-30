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

module type S = sig

  (** Signature for store contents. *)

  include IrminIdent.S
  (** Base types. *)

  val merge: t IrminMerge.t
  (** Merge function. Raise [Conflict] if the values cannot be merged
      properly. *)

end

module String: S with type t = string
(** String values where only the last modified value is kept on
    merge. If the value has been modified concurrently, the [merge]
    function raises [Conflict]. *)

module JSON: S with type t = Ezjsonm.t
(** JSON values where only the last modified value is kept on
    merge. If the value has been modified concurrently, the [merge]
    function raises [Conflict]. *)


module type STORE = sig

  (** Store user-defined contents. *)

  include IrminStore.AO
  (** Contents stores are append-only. *)

  val merge: t -> key IrminMerge.t
  (** Store merge function. Lift [S.merge] to keys. *)

  module Key: IrminKey.S with type t = key
  (** Base functions for foreign keys. *)

  module Value: S with type t = value
  (** Base functions for values. *)

end

module Make
    (K: IrminKey.S)
    (C: S)
    (Contents: IrminStore.AO with type key = K.t and type value = C.t)
  : STORE with type t = Contents.t
           and type key = K.t
           and type value = C.t
(** Build a contents store. *)

module Rec (S: STORE): S with type t = S.key
(** Convert a contents store objects into storable keys, with the
    expected merge function (eg. read the contents, merge them and
    write back the restult to get the final key). *)
