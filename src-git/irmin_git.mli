(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  ?head:Git.Reference.t -> ?bare:bool -> ?level:int -> string ->
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

(** Privides a subset of Irmin.Private.S (excludes branches and sync).
    This is useful if you want to store data in Git format, but do
    your own locking and sync. *)
module Irmin_value_store
    (G: VALUE_STORE)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
    (H: Irmin.Hash.S) : sig

  module Contents: Irmin.Contents.STORE
    with type key = H.t
     and type value = C.t
     and module Key = H

  module Node: Irmin.Private.Node.STORE
    with type key = H.t
     and type Val.contents = Contents.key
     and module Key = H
     and module Metadata = Metadata

  module Commit: Irmin.Private.Commit.STORE
    with type key = H.t
     and module Key = H
     and type Val.node = Node.key
end

module AO (G: Git.Store.S) (K: Irmin.Hash.S) (V: Irmin.Contents.Conv) : Irmin.AO
  with type t = G.t
   and type key = K.t
   and type value = V.t

module RW (G: Git.Store.S) (K: Irmin.Branch.S) (V: Irmin.Hash.S):
  Irmin.RW with type key = K.t and type value = V.t

module type S = sig
  include Irmin.S

  (** {1 Access to the Git objects} *)
  module Git: sig

    include Git.Store.S

    val git_commit: Repo.t -> commit -> Git.Commit.t option Lwt.t
    (** [git_commit repo h] is the commit corresponding to [h] in the
        repository [repo]. *)

    val of_repo: Repo.t -> t
    (** [of_repo r] is the Git store associated to [r]. *)

    val to_repo: ?head:Git.Reference.t -> ?bare:bool -> t -> Repo.t Lwt.t
    (** [to_repo t] is the Irmin repository associated to [t]. *)

  end

end

module type S_MAKER =
  functor (C: Irmin.Contents.S) ->
  functor (P: Irmin.Path.S) ->
  functor (B: Irmin.Branch.S) ->
  functor (H: Irmin.Hash.S) ->
    S with type key = P.t
       and type step = P.step
       and module Key = P
       and type contents = C.t
       and type branch = B.t
       and type metadata = Metadata.t
       and type Commit.Hash.t = H.t
       and type Tree.Hash.t = H.t
       and type Contents.Hash.t = H.t

module FS (IO: Git.Sync.IO) (I: Git.Inflate.S) (FS: Git.FS.IO): S_MAKER

module type S_mem = sig
  include S
  module Git_mem: sig
    val clear: ?root:string -> unit -> unit
    (** [clear ?root ()] clear the store located at [root]. *)

    val clear_all: unit -> unit
    (** [clear_all] clears all the known store.  *)
  end
end

module type S_MAKER_mem =
  functor (C: Irmin.Contents.S) ->
  functor (P: Irmin.Path.S) ->
  functor (B: Irmin.Branch.S) ->
  functor (H: Irmin.Hash.S) ->
    S_mem with type key = P.t
           and type step = P.step
           and module Key = P
           and type contents = C.t
           and type branch = B.t
           and type metadata = Metadata.t
           and type Commit.Hash.t = H.t
           and type Tree.Hash.t = H.t
           and type Contents.Hash.t = H.t

module Memory (IO: Git.Sync.IO) (I: Git.Inflate.S): S_MAKER_mem

module type CONTEXT = sig type t val v: unit -> t option Lwt.t end

module Memory_ext (C: CONTEXT)
    (IO: Git.Sync.IO with type ctx = C.t) (I: Git.Inflate.S):
  S_MAKER_mem
