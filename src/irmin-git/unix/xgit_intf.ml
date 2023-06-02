(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  include
    Irmin_git.S
      with type Backend.Remote.endpoint = Mimic.ctx * Smart_git.Endpoint.t

  val remote :
    ?ctx:Mimic.ctx -> ?headers:Cohttp.Header.t -> string -> unit -> Irmin.remote
end

module type Backend = sig
  (* FIXME: remove signature duplication *)

  module G : Irmin_git.G

  type endpoint = Mimic.ctx * Smart_git.Endpoint.t

  module Make
      (Schema : Irmin_git.Schema.S
                  with type Hash.t = G.hash
                   and type Node.t = G.Value.Tree.t
                   and type Commit.t = G.Value.Commit.t) :
    S
      with module Git = G
       and type Backend.Remote.endpoint = endpoint
       and module Schema := Schema

  module KV (C : Irmin.Contents.S) :
    S
      with module Git = G
       and type Schema.Contents.t = C.t
       and type Schema.Metadata.t = Irmin_git.Metadata.t
       and type Schema.Info.t = Irmin.Info.default
       and type Schema.Path.step = string
       and type Schema.Path.t = string list
       and type Schema.Hash.t = G.hash
       and type Schema.Branch.t = string
       and type Backend.Remote.endpoint = endpoint

  module Ref (C : Irmin.Contents.S) :
    S
      with module Git = G
       and type Schema.Contents.t = C.t
       and type Schema.Metadata.t = Irmin_git.Metadata.t
       and type Schema.Info.t = Irmin.Info.default
       and type Schema.Path.step = string
       and type Schema.Path.t = string list
       and type Schema.Hash.t = G.hash
       and type Schema.Branch.t = Irmin_git.reference
       and type Backend.Remote.endpoint = endpoint
end

module type Sigs = sig
  module type G = G
  module type S = S
  module type Backend = Backend
end
