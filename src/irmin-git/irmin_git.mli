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

module Metadata:
  Irmin.Metadata.S with type t = [`Normal | `Exec | `Link | `Everybody]

val config:
  ?config:Irmin.config ->
  ?head:Git.Reference.t -> ?bare:bool -> ?level:int -> ?dot_git:string ->
  string -> Irmin.config

val bare: bool Irmin.Private.Conf.key
val head: Git.Reference.t option Irmin.Private.Conf.key
val level: int option Irmin.Private.Conf.key
val dot_git: string option Irmin.Private.Conf.key

module AO (G: Git.S) (V: Irmin.Contents.Conv) : Irmin.AO
  with type t = G.t
   and type key = G.Hash.t
   and type value = V.t

module RW (G: Git.S) (K: Irmin.Branch.S): Irmin.RW
  with type key = K.t
   and type value = G.Hash.t

module type G = sig
  include Git.S
  val v:
    ?dotgit:Fpath.t ->
    ?compression:int ->
    ?buffers:buffer Lwt_pool.t ->
    Fpath.t -> (t, error) result Lwt.t
end

module Mem (H: Digestif.S): G
(** In-memory Git store. *)

module type S = sig

  (** The Git backend specializes a few types:

      {ul
      {- the allowed metadata are {!Metadata.t}.}
      {- the hash algorithm is SHA1.}
      }. *)

  (** Access to the underlying Git store. *)
  module Git: Git.S

  include Irmin.S with type metadata = Metadata.t
                   and type Commit.Hash.t = Git.Hash.t
                   and type Contents.Hash.t = Git.Hash.t
                   and type Tree.Hash.t = Git.Hash.t

  val git_commit: Repo.t -> commit -> Git.Value.Commit.t option Lwt.t
  (** [git_commit repo h] is the commit corresponding to [h] in the
      repository [repo]. *)

  val git_of_repo: Repo.t -> Git.t
  (** [of_repo r] is the Git store associated to [r]. *)

  val repo_of_git: ?head:Git.Reference.t -> ?bare:bool -> ?lock:Lwt_mutex.t ->
    Git.t -> Repo.t Lwt.t
  (** [to_repo t] is the Irmin repository associated to [t]. *)

end

module type S_MAKER =
  functor (G: G) ->
  functor (C: Irmin.Contents.S) ->
  functor (P: Irmin.Path.S) ->
  functor (B: Irmin.Branch.S) ->
    S with type key = P.t
       and type step = P.step
       and module Key = P
       and type contents = C.t
       and type branch = B.t
       and module Git = G

module type KV_MAKER =
  functor (G: G) ->
  functor (C: Irmin.Contents.S) ->
    S with type key = string list
       and type step = string
       and type contents = C.t
       and type branch = string
       and module Git = G

type reference = [
  | `Branch of string
  | `Remote of string
  | `Tag of string
  | `Other of string
]

module type REF_MAKER =
  functor (G: G) ->
  functor (C: Irmin.Contents.S) ->
    S with type key = string list
       and type step = string
       and type contents = C.t
       and type branch = reference
       and module Git = G

module Make (Net: Git.Sync.NET) : S_MAKER
module Ref (Net: Git.Sync.NET): REF_MAKER
module KV (Net: Git.Sync.NET): KV_MAKER

module type BRANCH = sig
  include Irmin.Branch.S
  val pp_ref: t Fmt.t
  val of_ref: string -> (t, [`Msg of string]) result
end

module Branch (B: Irmin.Branch.S): BRANCH

module Reference: BRANCH with type t = reference

module Make_ext
    (Net: Git.Sync.NET)
    (S  : G)
    (C  : Irmin.Contents.S)
    (P  : Irmin.Path.S)
    (B  : BRANCH):
  S with type key = P.t
     and type step = P.step
     and module Key = P
     and type contents = C.t
     and type branch = B.t
