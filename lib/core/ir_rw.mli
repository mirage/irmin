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

(** Read-write stores. *)

module type S = sig

  (** Mutable store. *)

  include Ir_ro.S

  val update: t -> key -> value -> unit Lwt.t
  (** Replace the contents of [key] by [value] if [key] is already
      defined and create it otherwise. *)

  val remove: t -> key -> unit Lwt.t
  (** Remove the given key. *)

  val watch: t -> key -> value Lwt_stream.t
  (** Watch a given key. *)

end

module type BINARY = S with type key = Cstruct.t and type value = Cstruct.t
(** Binary read-write store. Keys and values are cstruct buffers. *)

module type MAKER = functor (K: Tc.I0) -> functor (V: Tc.I0) ->
  S with type key = K.t and type value = V.t
(** Signature of functors creating read-write stores. *)

module Binary (S: BINARY) (K: Tc.I0) (V: Tc.I0):
  S with type t = S.t and type key = K.t and type value = V.t
(** Create a typed read-write store from a binary one. *)
