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

type remote
(** Remote store hanlders. *)

type 'a store = (module IrminBranch.STORE with type branch = 'a)
(** A tuple store implementation / branch name. *)

val store: 'a store -> 'a -> remote
(** Remote stores. *)

val uri: string -> remote
(** Remote URI. *)

module type STORE = sig

  (** Store with import/export capabilities. *)

  type t
  (** Local dump handler. *)

  type db
  (** Local database handlers. *)

  type origin
  (** Provenance tracker. *)

  val fetch: db -> ?depth:int -> remote -> t option Lwt.t
  (** [create t last] create a dump object in the local database. The
      local database can then be either [merged] or [updated] to the
      new contents. The [depth] parameter limits the history depth.*)

  val fetch_exn: db -> ?depth:int -> remote -> t Lwt.t
  (** Same as [create] but raise [Failure] is the fetch operation
      fails. *)

  val push: db -> ?depth:int -> remote -> t option Lwt.t
  (** [push t dump] push the contents of the currnet branch of the
      database to the remote database -- also update the remote branch
      with the same name as the local one to points to the new
      state. *)

  val push_exn: db -> ?depth:int -> remote -> t Lwt.t
  (** Same as [push] but raise [Failure] is the push operation
      fails. *)

  val update: db -> t -> unit Lwt.t
  (** [update t dump] imports the contents of [dump] in the
      database. This replace the current branch with the imported
      contents.  *)

  val merge: db -> ?origin:origin -> t -> unit IrminMerge.result Lwt.t
  (** Same as [update] but merge with the current branch. *)

  val merge_exn: db -> ?origin:origin -> t -> unit Lwt.t
  (** Same as [merge] but merge raise an exception in case of conflict. *)

  include IrminIdent.S with type t := t
  (** Base functions over database dumps. *)

end

module type BACKEND = sig

  type t
  (** Database handler. *)

  type key
      (** Key values. *)

  val fetch: t -> ?depth:int -> string -> key option Lwt.t
  (** How to fetch a given URI. *)

  val push : t -> ?depth:int -> string -> key option Lwt.t
  (** How to push to a given URI. *)

end

module Slow (S: IrminBranch.STORE):
  STORE with type db = S.t and type t = S.Block.key
(** Only local (and slow) synchronisation. *)

module Fast
    (S: IrminBranch.STORE)
    (B: BACKEND with type t = S.t and type key = S.Block.key):
  STORE with type db = S.t and type t = S.Block.key
(** Use a fast-path to synchronize when possible. *)
