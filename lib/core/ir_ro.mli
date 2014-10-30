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

(** Read-only stores. *)

module type S = sig

  (** Read-only store. *)

  type t
  (** Type a store. *)

  type key
  (** Type of keys. *)

  type value
  (** Type of values. *)

  val create: unit -> t Lwt.t
  (** Create a store handle. The operation can be used multiple times
      as it is supposed to be very cheap (and usually
      non-blocking). *)

  val read: t -> key -> value option Lwt.t
  (** Read a value from the store. *)

  val read_exn: t -> key -> value Lwt.t
  (** Read a value from the store. Raise [Unknown k] if [k] does not
      have an associated value. *)

  val mem: t -> key -> bool Lwt.t
  (** Check if a key exists. *)

  val list: t -> key list -> key list Lwt.t
  (** Return all the keys that are allowed to access, knowing a given
      collection of keys (which might be seen as a passwords). *)

  val dump: t -> (key * value) list Lwt.t
  (** Return the store contents. *)

end

module type BINARY = S with type key = Cstruct.t and type value = Cstruct.t
(** Binary read-only stores. Keys and values are cstruct buffers. *)

module type MAKER = functor (K: Tc.I0) -> functor (V: Tc.I0) ->
  S with type key = K.t and type value = V.t
(** Signature for functor creating read-only stores. *)

module Binary (S: BINARY) (K: Tc.I0) (V: Tc.I0):
  S with type t = S.t and type key = K.t and type value = V.t
(** Create a typed read-only store from a binary one. *)
