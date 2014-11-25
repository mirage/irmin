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

(** Store Synchronisation. *)

module type STORE = sig

  (** Store with import/export capabilities. *)

  type db
  (** The type for store handlers. *)

  type head
  (** The type for head values. *)

  type remote
  (** The type for remote stores. *)

  val uri: string -> remote
  (** [uri s] is the remote store located at [uri]. Use the optimized
      native synchronisation protocol when available for the given
      backend. *)

  val store: db -> remote
  (** [store t] is the remote corresponding to the local store
      [t]. Synchronisation is done by importing and exporting store
      slices, so this is usually much slower than native
      synchronisation using [uri] remotes. *)

  val fetch: db -> ?depth:int -> remote -> head option Lwt.t
  (** [create t last] fetch an object in the local database. The local
      database can then be either [merged], or [updated] to the new
      contents. The [depth] parameter limits the history depth.*)

  val fetch_exn: db -> ?depth:int -> remote -> head Lwt.t
  (** Same as [create] but raise [Failure] is the fetch operation
      fails. *)

  val push: db -> ?depth:int -> remote -> head option Lwt.t
  (** [push t f] push the contents of the currnet branch of the
      database to the remote database -- also update the remote branch
      with the same name as the local one to points to the new
      state. *)

  val push_exn: db -> ?depth:int -> remote -> head Lwt.t
  (** Same as [push] but raise [Failure] is the push operation
      fails. *)

end

module type REMOTE = sig

  type db
  (** The type for store handles. *)

  type head
  (** The type for store heads. *)

  val fetch: db -> ?depth:int -> string -> head option Lwt.t
  (** [fetch t uri] fetches the contents of the remote store located
      at [uri] into the local store [t]. Return [None] if the remote
      store is empty, otherwise, return the head of [uri]. *)

  val push : db -> ?depth:int -> string -> head option Lwt.t
  (** [push t uri] pushes the contents of the local store [t] into the
      remote store located at [uri]. Return [None] is the local store
      is empty, otherwise, return the head of [t]. *)

end

module None (S: Ir_bc.STORE):
  REMOTE with type db = S.t and type head = S.head
(** An implementation of remote synchronisation which does nothing,
    i.e. it always return [None] on [fetch] and [push]. *)

module Make
    (S: Ir_bc.STORE)
    (R: REMOTE with type db = S.t and type head = S.head):
  STORE with type db = S.t and type head = S.head
(** Use [R] to synchronize stores using some native (and usually fast)
    backend-specific protocols. *)
