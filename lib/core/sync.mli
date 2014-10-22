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

type ('contents, 'tag) remote
(** Remote store hanlders. *)

type ('key, 'contents, 'tag) store =
  (module Branch.STORE with type Block.key = 'key
                        and type value = 'contents
                        and type tag = 'tag)
(** A pair (store implementation * branch name). *)

val store: ('a, 'b, 'c) store -> 'c -> ('a, 'b) remote
(** Remote stores. *)

val uri: string -> ('a, 'b) remote
(** Remote URI. *)

module type STORE = sig

  (** Store with import/export capabilities. *)

  type t
  (** Type for database handlers. *)

  type uid
  (** Type for unique identifiers. *)

  type contents
  (** Type for database contents. *)

  type tag
  (** Type for branch names. *)

  type origin
  (** Type for values tracking provenance. *)

  val fetch: t -> ?depth:int -> (contents, tag) remote -> uid option Lwt.t
  (** [create t last] fetch an object in the local database. The local
      database can then be either [merged], or [updated] to the new
      contents. The [depth] parameter limits the history depth.*)

  val fetch_exn: t -> ?depth:int -> (contents, tag) remote -> uid Lwt.t
  (** Same as [create] but raise [Failure] is the fetch operation
      fails. *)

  val push: t -> ?depth:int -> (contents, tag) remote -> uid option Lwt.t
  (** [push t f] push the contents of the currnet branch of the
      database to the remote database -- also update the remote branch
      with the same name as the local one to points to the new
      state. *)

  val push_exn: t -> ?depth:int -> (contents, tag) remote -> uid Lwt.t
  (** Same as [push] but raise [Failure] is the push operation
      fails. *)

  val update: t -> uid -> unit Lwt.t
  (** [update t f] imports the contents of [f] in the database. This
      replace the current branch with the imported contents.  *)

  val merge: t -> ?origin:origin -> uid -> unit Merge.result Lwt.t
  (** Same as [update] but merge with the current branch. *)

  val merge_exn: t -> ?origin:origin -> uid -> unit Lwt.t
  (** Same as [merge] but merge raise an exception in case of conflict. *)

  module Uid: Sig.Uid

end

module type REMOTE = sig

  type uid
  (** Type for unique identifiers. *)

  val fetch: ?depth:int -> string -> uid option Lwt.t
  (** How to fetch a given URI. *)

  val push : ?depth:int -> string -> uid option Lwt.t
  (** How to push to a given URI. *)

end

module Slow (B: Block.STORE) (T: Tag.STORE):
  STORE with type contents = B.value and type tag = T.key
(** This functor copies iterate through *all* the k/v pairs in the
    database so it is *very* slow. Use the [Fast] one when
    possible. *)

module Fast (B: Block.STORE) (T: Tag.STORE) (R: REMOTE with type uid = B.key):
  STORE with type uid = B.key and type contents = B.value and type tag = T.key
(** Use [R] to synchronize stores using some native (and usually fast)
    backend-specific protocols. *)
