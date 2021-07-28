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

module type G = sig
  include Git.S

  val v : ?dotgit:Fpath.t -> Fpath.t -> (t, error) result Lwt.t
end

module type S = sig
  (** The Git backend specializes a few types:

      - the allowed metadata are {!Metadata.t}.
      - the hash algorithm is SHA1. *)

  module Git : Git.S
  (** Access to the underlying Git store. *)

  module Schema :
    Irmin.Schema.S with type Metadata.t = Metadata.t and type Hash.t = Git.hash

  include Irmin.S with module Schema := Schema

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

(** Same as {!Irmin.Maker} but with a fixed hash (SHA1) and metadata (Git
    metadata) implemtations. *)
module type Maker = sig
  module G : G

  type endpoint = Mimic.ctx * Smart_git.Endpoint.t

  module Make
      (Schema : Schema.S
                  with type Hash.t = G.hash
                   and type Node.t = G.Value.Tree.t
                   and type Commit.t = G.Value.Commit.t) :
    S
      with module Git = G
       and module Schema := Schema
       and type Private.Remote.endpoint = endpoint
end

module type KV_maker = sig
  module G : G

  type endpoint = Mimic.ctx * Smart_git.Endpoint.t
  type branch

  module Make (C : Irmin.Contents.S) :
    S
      with module Git = G
       and type Schema.Contents.t = C.t
       and type Schema.Metadata.t = Metadata.t
       and type Schema.Info.t = Irmin.Info.default
       and type Schema.Path.step = string
       and type Schema.Path.t = string list
       and type Schema.Hash.t = G.hash
       and type Schema.Branch.t = branch
       and type Private.Remote.endpoint = endpoint
end

module type Sigs = sig
  module Metadata = Metadata
  module Conf = Conf
  module Branch = Branch
  module Reference = Reference
  module Schema = Schema

  (** {2 Module types} *)

  module type G = G
  module type S = S
  module type Maker = Maker
  module type KV_maker = KV_maker

  val config :
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
    (** Use Git as an atomic-write store. Values will be stored into
        [.git/refs]. When using the Git filesystem backend, branch names .*)

    module type S = Irmin.Atomic_write.S with type value = G.Hash.t

    module Make (K : Irmin.Branch.S) : S with type key = K.t
  end

  module Maker
      (G : G)
      (S : Git.Sync.S with type hash := G.hash and type store := G.t) :
    Maker with module G := G

  module KV
      (G : G)
      (S : Git.Sync.S with type hash := G.hash and type store := G.t) :
    KV_maker with module G := G and type branch = string

  module Ref
      (G : G)
      (S : Git.Sync.S with type hash := G.hash and type store := G.t) :
    KV_maker with module G := G and type branch = Reference.t

  module Generic_KV
      (CA : Irmin.Content_addressable.Maker)
      (AW : Irmin.Atomic_write.Maker) : Irmin.KV_maker with type endpoint = unit

  (** In-memory Git store. *)
  module Mem :
    G with type t = Digestif.SHA1.t Git.Mem.t and type hash = Digestif.SHA1.t
end
