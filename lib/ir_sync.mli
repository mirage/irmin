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

(** Common URI resolver. *)

type ('head, 'origin, 'slice) remote
(** Remote store hanlders. *)

type ('head, 'tag, 'origin, 'slice) store =
  (module Ir_bc.STORE with type head = 'head
                       and type origin = 'origin
                       and type tag = 'tag
                       and type slice = 'slice)
(** A store handler. *)

val store: ('a, 'b, 'c, 'd) store -> 'b -> ('a, 'c, 'd) remote
(** local stores. *)

val uri: string -> ('a, 'b, 'c) remote
(** Remote URI. *)

module type STORE = sig

  (** Store with import/export capabilities. *)

  type t
  (** Type for database handlers. *)

  type head
  (** Type for head values. *)

  type origin
  (** Type for values tracking provenance. *)

  type slice
  (** Type for import/export values. *)

  val fetch: t -> origin -> ?depth:int -> (head, origin, slice) remote
    -> head option Lwt.t
  (** [create t last] fetch an object in the local database. The local
      database can then be either [merged], or [updated] to the new
      contents. The [depth] parameter limits the history depth.*)

  val fetch_exn: t -> origin -> ?depth:int -> (head, origin, slice) remote
    -> head Lwt.t
  (** Same as [create] but raise [Failure] is the fetch operation
      fails. *)

  val push: t -> origin -> ?depth:int -> (head, origin, slice) remote
    -> head option Lwt.t
  (** [push t f] push the contents of the currnet branch of the
      database to the remote database -- also update the remote branch
      with the same name as the local one to points to the new
      state. *)

  val push_exn: t -> origin -> ?depth:int -> (head, origin, slice) remote
    -> head Lwt.t
  (** Same as [push] but raise [Failure] is the push operation
      fails. *)

end

module type REMOTE = sig

  type head
  (** Type for unique identifiers. *)

  val fetch: ?depth:int -> string -> head option Lwt.t
  (** How to fetch a given URI. *)

  val push : ?depth:int -> string -> head option Lwt.t
  (** How to push to a given URI. *)

end

module Slow (B: Ir_bc.STORE):
  STORE with type head = B.head
         and type origin = B.origin
         and type slice = B.slice
(** This functor iterate through *all* the k/v pairs in the database
    so it is *very* slow. Use the [Fast] one when possible. *)

module Fast (B: Ir_bc.STORE) (R: REMOTE with type head = B.head):
  STORE with type head = B.head
         and type origin = B.origin
         and type slice = B.slice
(** Use [R] to synchronize stores using some native (and usually fast)
    backend-specific protocols. *)
