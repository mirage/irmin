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
    Irmin.Schema.S with type metadata = Metadata.t and type hash = Git.hash

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
                  with type hash = G.hash
                   and type node = G.Value.Tree.t
                   and type commit = G.Value.Commit.t) :
    S
      with module Git = G
       and type Private.Remote.endpoint = endpoint
       and module Schema := Schema
end

module type KV_maker = sig
  module G : G

  type endpoint = Mimic.ctx * Smart_git.Endpoint.t
  type branch

  module Make (C : Irmin.Contents.S) :
    S
      with module Git = G
       and type Schema.contents = C.t
       and type Schema.metadata = Metadata.t
       and type Schema.info = Irmin.Info.default
       and type Schema.step = string
       and type Schema.path = string list
       and type Schema.hash = G.hash
       and type Schema.branch = branch
       and type Private.Remote.endpoint = endpoint
end

module type Sigs = sig
  module type G = G
  module type S = S
  module type Maker = Maker
  module type KV_maker = KV_maker
end
