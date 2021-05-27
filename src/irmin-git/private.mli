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

module Make
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Branch.S) : sig
  type t := bool ref * G.t

  include
    Irmin.Private.S
      with type 'a Contents.t = t
       and type 'a Node.t = t * t
       and type 'a Commit.t = (t * t) * t
       and type Remote.endpoint = Mimic.ctx * Smart_git.Endpoint.t
       and type Contents.value = C.t
       and type Hash.t = G.Hash.t
       and type Node.Path.t = P.t
       and type Node.Path.step = P.step
       and type Node.Metadata.t = Metadata.t
       and type Branch.key = B.t
       and type Version.t = unit
       and module Commit.Info = Irmin.Info.Default

  val git_of_repo : Repo.t -> G.t

  val repo_of_git :
    ?head:Git.Reference.t ->
    ?bare:bool ->
    ?lock:Lwt_mutex.t ->
    G.t ->
    Repo.t Lwt.t
end
