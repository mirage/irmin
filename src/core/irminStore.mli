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

(** Common signature for all stores. *)

module type X = sig

  (** Base types for stores. *)

  type t
  (** Type a store. *)

  type key
  (** Type of keys. *)

  type value
  (** Type of values. *)

  val create: unit -> t Lwt.t
  (** Create a new store. *)

  val read: t -> key -> value option Lwt.t
  (** Read a value from the store. *)

  val read_exn: t -> key -> value Lwt.t
  (** Read a value from the store. Raise [Unknown k] if [k] does not
      have an associated value. *)

  val mem: t -> key -> bool Lwt.t
  (** Check if a key exists. *)

  val list: t -> key -> key list Lwt.t
  (** Return all the keys that are allowed to access, knowing a
      key (which might be seen as a password). *)

end

(** {2 Append-only Stores} *)

module type A = sig

  (** Base types for append-only stores. *)

  include X

  val add: t -> value -> key Lwt.t
  (** Write the contents of a value to the store. That's the
      responsibility of the append-only store to generate a consistent
      key. *)

end

module type A_RAW = A with type value := IrminBuffer.t
(** Raw immutable stores. *)

module MakeI (S: A_RAW) (K: IrminKey.S with type t = S.key) (V: IrminBase.S):
  A with type t = S.t
     and type key = K.t
     and type value = V.t
(** Build a typed append-only store from a raw one. *)

(** {2 Mutable store} *)

module type M = sig

  (** Signature for mutable store. *)

  include X

  val update: t -> key -> value -> unit Lwt.t
  (** Replace the contents of [key] by [value] if [key] is already
      defined and create it otherwise. *)

  val remove: t -> key -> unit Lwt.t
  (** Remove the given key. *)

end

module type M_RAW = M with type key = string
(** Raw mutable stores. *)

module MakeM
    (S: M_RAW)
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
