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

(** Append-only stores. *)

module type S = sig

  (** Signature for append-only stores. *)

  include Ir_ro.S

  val add: t -> value -> key Lwt.t
  (** Write the contents of a value to the store. That's the
      responsibility of the append-only store to generate a consistent
      key. *)

end

module type BINARY = S with type key = Cstruct.t and type value = Cstruct.t
(** Binary append-only store. Keys and values are cstruct buffers. *)

module type MAKER = functor (K: Ir_uid.S) -> functor (V: Tc.I0) ->
  S with type key = K.t and type value = V.t
(** Signature of functor creating append-only stores. *)

module Binary (S: BINARY) (K: Ir_uid.S) (V: Tc.I0):
  S with type t = S.t and type key = K.t and type value = V.t
(** Create a typed append-only store from a binary one. *)
