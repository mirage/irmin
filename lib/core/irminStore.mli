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

(** {2 Read-only store} *)

module type RO = sig

  (** Base types for read-only stores. *)

  type t
  (** Type a store. *)

  type key
  (** Type of keys. *)

  type value
  (** Type of values. *)

  val create: unit -> t Lwt.t
  (** Create a store handle. *)

  val read: t -> key -> value option Lwt.t
  (** Read a value from the store. *)

  val read_exn: t -> key -> value Lwt.t
  (** Read a value from the store. Raise [Unknown k] if [k] does not
      have an associated value. *)

  val mem: t -> key -> bool Lwt.t
  (** Check if a key exists. *)

  val list: t -> key -> key list Lwt.t
  (** Return all the keys that are allowed to access, knowing a key
      (which might be seen as a password). *)

  val contents: t -> (key * value) list Lwt.t
  (** Return the store contents. *)

end

module type RO_BINARY = RO with type key = string
                            and type value = Cstruct.buffer
(** Read-only store which associate strings to big arrays. *)

module type RO_MAKER = functor (K: IrminKey.S) -> functor (V: IrminBase.S) ->
  RO with type key = K.t
      and type value = V.t
(** Read-only store makers. *)

module RO_MAKER (B: RO_BINARY): RO_MAKER
(** Build typed read-only from a binary one. *)

(** {2 Append-only Stores} *)

module type AO = sig

  (** Base types for append-only stores. *)

  include RO

  val add: t -> value -> key Lwt.t
  (** Write the contents of a value to the store. That's the
      responsibility of the append-only store to generate a consistent
      key. *)

end

module type AO_BINARY = AO with type key = string
                            and type value = Cstruct.buffer
(** Append-only store which associate strings to big arrays. *)

module type AO_MAKER = functor (K: IrminKey.S) -> functor (V: IrminBase.S) ->
  AO with type key = K.t
      and type value = V.t
(** Append-only store makers. *)

module AO_MAKER (B: AO_BINARY): AO_MAKER
(** Build a typed append-only store from a binary one. *)

(** {2 Mutable store} *)

module type RW = sig

  (** Signature for mutable (ie. read/write) stores. *)

  include RO

  val update: t -> key -> value -> unit Lwt.t
  (** Replace the contents of [key] by [value] if [key] is already
      defined and create it otherwise. *)

  val remove: t -> key -> unit Lwt.t
  (** Remove the given key. *)

end

module type RW_BINARY = RW with type key = string
                            and type value = Cstruct.buffer
(** read-write store which associate strings to big arrays. *)

module type RW_MAKER = functor (K: IrminKey.S) -> functor (V: IrminBase.S) ->
  RW with type key = K.t
      and type value = V.t
(** Mutable store makers. *)

module RW_MAKER (B: RW_BINARY): RW_MAKER
(** Build a typed read-write store from a binary one. *)

(** {2 Irminsule Stores} *)

module type S = sig

  (** On an high-level view, Irminsule exposes the same interface as a
      low-level mutable store, but you gain the commit, rollback and
      notification mechanisms. *)

  include RW

  type snapshot
  (** Abstract snapshot values. *)

  val snapshot: t -> snapshot Lwt.t
  (** Get a snapshot of the current store state. *)

  val revert: t -> snapshot -> unit Lwt.t
  (** Revert the store to a previous state. *)

  val watch: t -> key -> (key * snapshot) Lwt_stream.t
  (** Subscribe to the stream of modification events attached to a
      given key. *)

  type dump
  (** Raw dump. *)

  val export: t -> snapshot list -> dump Lwt.t
  (** Return all the new contents in the store *from* which has been
      added after the revisions. If the revision is [None], then
      export everything. *)

  val import: t -> dump -> unit Lwt.t
  (** Import some raw contents. This does not change the tags. *)

end

module type S_MAKER =
  functor (K: IrminKey.S) ->
  functor (V: IrminBase.S) ->
  functor (S: IrminBase.S) ->
  functor (D: IrminBase.S) ->
    S with type key = K.t
       and type value = V.t
       and type snapshot = S.t
       and type dump = D.t
(** Irminsule store makers. *)
