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

exception Invalid of string
(** Invalid parsing. *)

module type S = sig

  (** Signature for store contents. *)

  include Misc.I0
  (** Base types. *)

  val merge: t Merge.t
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

  include S.AO
  (** Contents stores are append-only. *)

  val merge: t -> key Merge.t
  (** Store merge function. Lift [S.merge] to keys. *)

  module Value: S with type t = value
  (** Base functions for values. *)

  module Key: Key.S with type t = key
  (** Base functions for foreign keys. *)

end

module Make
    (K: Key.S)
    (C: S)
    (Contents: S.AO with type key = K.t and type value = C.t)
  : STORE with type t = Contents.t
           and type key = K.t
           and type value = C.t
(** Build a contents store. *)

module Rec (S: STORE): S with type t = S.key
(** Convert a contents store objects into storable keys, with the
    expected merge function (eg. read the contents, merge them and
    write back the restult to get the final key). *)
