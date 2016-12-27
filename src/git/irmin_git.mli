(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Git backend *)

(* Discard the hash implementation passed in parameter of the functors. *)

module Metadata: Irmin.Metadata.S with type t = [`Normal | `Exec | `Link]

val config:
  ?config:Irmin.config ->
  ?root:string -> ?head:Git.Reference.t -> ?bare:bool -> ?level:int -> unit ->
  Irmin.config

val bare: bool Irmin.Private.Conf.key
val head: Git.Reference.t option Irmin.Private.Conf.key
val level: int option Irmin.Private.Conf.key

module type VALUE_STORE = sig
  (** This is the subset of Git.Store.S needed for [Value_store]. *)
  type t
  val read: t -> Git.Hash.t -> Git.Value.t option Lwt.t
  val mem: t -> Git.Hash.t -> bool Lwt.t
  val write: t -> Git.Value.t -> Git.Hash.t Lwt.t
  val contents: t -> (Git.Hash.t * Git.Value.t) list Lwt.t
  module Digest : Git.Hash.DIGEST
end

(** [Hash] is an implementation of Irmin hashes based on Git
    hashes. *)
module Hash (G: VALUE_STORE): Irmin.Hash.S with type t = Git.Hash.t

(** Privides a subset of Irmin.Private.S (excludes tags and sync).
    This is useful if you want to store data in Git format, but do
    your own locking and sync. *)
module Irmin_value_store
    (G: VALUE_STORE)
    (C: Irmin.Contents.S)
    (H: Irmin.Hash.S) : sig

  module Contents: Irmin.Contents.STORE
    with type key = H.t
     and type value = C.t
     and module Key = H
     and module Path = C.Path

  module Node: Irmin.Private.Node.STORE
    with type key = H.t
     and type Val.raw_contents = Contents.key
     and module Key = H
     and module Path = Contents.Val.Path
     and module Val.Metadata = Metadata

  module Commit: Irmin.Private.Commit.STORE
    with type key = H.t
     and module Key = H
     and type Val.node = Node.key
end


module type LOCK = sig
  val with_lock: string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

module AO (G: Git.Store.S) (K: Irmin.Hash.S) (V: Tc.S0) : Irmin.AO
  with type t = G.t
   and type key = K.t
   and type value = V.t

module RW (L: LOCK) (G: Git.Store.S) (K: Irmin.Ref.S) (V: Irmin.Hash.S):
  Irmin.RW with type key = K.t and type value = V.t

module type S = sig
  include Irmin.S

  module Internals: sig

    (** {1 Access to the Git objects} *)

    val commit_of_id: Repo.t -> commit_id -> Git.Commit.t option Lwt.t
    (** [commit_of_id repo h] is the commit corresponding to [h] in the
        repository [repo]. *)

  end
end

module type S_MAKER =
  functor (C: Irmin.Contents.S) ->
  functor (R: Irmin.Ref.S) ->
  functor (H: Irmin.Hash.S) ->
    S with type key = C.Path.t
       and module Key = C.Path
       and type value = C.t
       and type branch_id = R.t
       and type commit_id = H.t
       and module Private.Node.Val.Metadata = Metadata

module Memory (IO: Git.Sync.IO) (I: Git.Inflate.S):
  S_MAKER

module FS (IO: Git.Sync.IO) (I: Git.Inflate.S) (L: LOCK) (FS: Git.FS.IO):
  S_MAKER

module type CONTEXT = sig type t val v: unit -> t option Lwt.t end

module Memory_ext (C: CONTEXT)
    (IO: Git.Sync.IO with type ctx = C.t) (I: Git.Inflate.S):
  S_MAKER
