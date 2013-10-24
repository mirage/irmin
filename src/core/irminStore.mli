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

(** Stores. *)

(** {2 Immutable stores} *)

module type I = sig

  (** Base types for immutable stores. *)

  type key
  (** Type of keys. *)

  type value
  (** Type of values. *)

  val init: unit -> unit Lwt.t
  (** Init the store. *)

  val write: value -> key Lwt.t
  (** Write the contents of a value to the store. *)

  val read: key -> value option Lwt.t
  (** Read a value from the store. *)

  val read_exn: key -> value Lwt.t
  (** Read a value from the store. Raise [Unknown k] if [k] does not
      have an associated value. *)

  val mem: key -> bool Lwt.t
  (** Check if a key exists. *)

end

module type IRAW = I with type value := IrminBuffer.t
(** Raw immutable stores. *)

module MakeI (S: IRAW) (K: IrminKey.S with type t = S.key) (V: IrminBase.S):
  I with type key = K.t
     and type value = V.t
(** Build a typed store. *)

(** {2 Mutable store} *)

module type M = sig

  (** Signature for mutable store. *)

  type key
  (** Type of tags. *)

  type value
  (** Type of values. *)

  val init: unit -> unit Lwt.t
  (** Init the store. *)

  val set: key -> value -> unit Lwt.t
  (** Replace the contents of [key] by [value] if [key] is already
      defined and create it otherwise. *)

  val remove: key -> unit Lwt.t
  (** Remove the given key. *)

  val read: key -> value option Lwt.t
  (** Read a key. Return [None] if the the key is not defined. *)

  val read_exn: key -> value Lwt.t
  (** Read a key, raise [Unknown k] if the key is not defined. *)

  val mem: key -> bool Lwt.t
  (** Check if a key exist. *)

  val list: key -> key list Lwt.t
  (** Return all the keys that you can access, knowing a password
      key. *)

end

module type MRAW = M with type key = string
(** Raw mutable stores. *)

module MakeM
    (S: MRAW)
    (K: IrminBase.STRINGABLE)
    (V: IrminBase.S with type t = S.value)
  : M with type key = K.t
       and type value = V.t
(** Build a mutable store. *)
