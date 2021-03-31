(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Metadata = Metadata
module Conf = Conf
module Branch = Branch
module Reference = Reference

val config :
  ?config:Irmin.config ->
  ?head:Git.Reference.t ->
  ?bare:bool ->
  ?level:int ->
  ?dot_git:string ->
  ?buffers:int ->
  string ->
  Irmin.config

type reference = Reference.t [@@deriving irmin]

module Content_addressable (G : Git.S) : sig
  (** Use Git as a content-addressable store. Values will be stored into
      [.git/objects].*)

  module type S = Irmin.Content_addressable.S with type key = G.Hash.t

  module Make (V : Irmin.Type.S) : S with type value = V.t
end

module Atomic_write (G : Git.S) : sig
  (** Use Git as an atomic-write store. Values will be stored into [.git/refs].
      When using the Git filesystem backend, branch names .*)

  module type S = Irmin.Atomic_write.S with type value = G.Hash.t

  module Make (K : Irmin.Branch.S) : S with type key = K.t
end

module type G = sig
  include Git.S

  val v : ?dotgit:Fpath.t -> Fpath.t -> (t, error) result Lwt.t
end

(** In-memory Git store. *)
module Mem :
  G with type t = Digestif.SHA1.t Git.Mem.t and type hash = Digestif.SHA1.t

module type S = sig
  (** The Git backend specializes a few types:

      - the allowed metadata are {!Metadata.t}.
      - the hash algorithm is SHA1. *)

  module Git : Git.S
  (** Access to the underlying Git store. *)

  include Irmin.S with type metadata = Metadata.t and type hash = Git.hash

  val git_commit : Repo.t -> commit -> Git.Value.Commit.t option Lwt.t
  (** [git_commit repo h] is the commit corresponding to [h] in the repository
      [repo]. *)

  val git_of_repo : Repo.t -> Git.t
  (** [of_repo r] is the Git store associated to [r]. *)

  val repo_of_git :
    ?head:Git.Reference.t ->
    ?bare:bool ->
    ?lock:Lwt_mutex.t ->
    Git.t ->
    Repo.t Lwt.t
  (** [to_repo t] is the Irmin repository associated to [t]. *)
end

module type Maker = functor
  (G : G)
  (S : Git.Sync.S with type hash = G.hash and type store = G.t)
  (C : Irmin.Contents.S)
  (P : Irmin.Path.S)
  (B : Irmin.Branch.S)
  ->
  S
    with type key = P.t
     and type step = P.step
     and module Key = P
     and type contents = C.t
     and type branch = B.t
     and module Git = G
     and type Private.Remote.endpoint = Mimic.ctx * Smart_git.Endpoint.t

module type KV_maker = functor
  (G : G)
  (S : Git.Sync.S with type hash = G.hash and type store = G.t)
  (C : Irmin.Contents.S)
  ->
  S
    with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = string
     and module Git = G
     and type Private.Remote.endpoint = Mimic.ctx * Smart_git.Endpoint.t

module type Ref_maker = functor
  (G : G)
  (S : Git.Sync.S with type hash = G.hash and type store = G.t)
  (C : Irmin.Contents.S)
  ->
  S
    with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = reference
     and module Git = G
     and type Private.Remote.endpoint = Mimic.ctx * Smart_git.Endpoint.t

module Make : Maker
module Ref : Ref_maker
module KV : KV_maker

module Make_ext
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Branch.S) :
  S
    with type key = P.t
     and type step = P.step
     and module Key = P
     and type contents = C.t
     and type branch = B.t

module Generic
    (CA : Irmin.Content_addressable.Maker)
    (AW : Irmin.Atomic_write.Maker)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S) :
  Irmin.S
    with type contents = C.t
     and type key = P.t
     and type branch = B.t
     and type step = P.step
     and type metadata = Metadata.t
     and type hash = Digestif.SHA1.t

module Generic_KV
    (CA : Irmin.Content_addressable.Maker)
    (AW : Irmin.Atomic_write.Maker)
    (C : Irmin.Contents.S) : Irmin.KV with type contents = C.t
