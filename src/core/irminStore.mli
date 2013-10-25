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

(** {2 Append-only Stores} *)

module type A = sig

  (** Base types for append-only stores. *)

  type t
  (** Type a store. *)

  type key
  (** Type of keys. *)

  type value
  (** Type of values. *)

  val write: t -> value -> key Lwt.t
  (** Write the contents of a value to the store. *)

  val read: t -> key -> value option Lwt.t
  (** Read a value from the store. *)

  val read_exn: t -> key -> value Lwt.t
  (** Read a value from the store. Raise [Unknown k] if [k] does not
      have an associated value. *)

  val mem: t -> key -> bool Lwt.t
  (** Check if a key exists. *)

end

module type ARAW = A with type value := IrminBuffer.t
(** Raw immutable stores. *)

module MakeI (S: ARAW) (K: IrminKey.S with type t = S.key) (V: IrminBase.S):
  A with type t = S.t
     and type key = K.t
     and type value = V.t
(** Build a typed store. *)

(** {2 Mutable store} *)

module type M = sig

  (** Signature for mutable store. *)

  type t
  (** Type of stores. *)

  type key
  (** Type of tags. *)

  type value
  (** Type of values. *)

  val set: t -> key -> value -> unit Lwt.t
  (** Replace the contents of [key] by [value] if [key] is already
      defined and create it otherwise. *)

  val remove: t -> key -> unit Lwt.t
  (** Remove the given key. *)

  val read: t -> key -> value option Lwt.t
  (** Read a key. Return [None] if the the key is not defined. *)

  val read_exn: t -> key -> value Lwt.t
  (** Read a key, raise [Unknown k] if the key is not defined. *)

  val mem: t -> key -> bool Lwt.t
  (** Check if a key exist. *)

  val list: t -> key -> key list Lwt.t
  (** Return all the keys that you can access, knowing a password
      key. *)

end

module type MRAW = M with type key = string
(** Raw mutable stores. *)

module MakeM
    (S: MRAW)
    (K: IrminBase.STRINGABLE)
    (V: IrminBase.S with type t = S.value)
  : M with type t = S.t
       and type key = K.t
       and type value = V.t
(** Build a mutable store. *)

(** {2 Irminsule Stores} *)

module type S = sig

  (** At high-level,Irminsule exposes the same interface as a
      low-level mutable store, but you gain the commit, rollback and
      notification mechanisms. *)

  include M

  type revision
  (** Type of revisions. *)

  val snapshot: t -> revision Lwt.t
  (** Get a snapshot of the current store state. *)

  val revert: t -> revision -> unit Lwt.t
  (** Revert the store to a previous state. *)

  val watch: t -> key -> (key * revision option) Lwt_stream.t
  (** Subscribe to the stream of modification events attached to a
      given key. *)

end
