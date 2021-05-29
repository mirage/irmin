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

module type S = sig
  include
    Irmin_git.S
      with type Private.Remote.endpoint = Mimic.ctx * Smart_git.Endpoint.t

  val remote :
    ?ctx:Mimic.ctx -> ?headers:Cohttp.Header.t -> string -> Irmin.remote
end

module type Maker = sig
  module G : Irmin_git.G

  type endpoint = Mimic.ctx * Smart_git.Endpoint.t

  module Make
      (Schema : Irmin_git.Schema.S
                  with type hash = G.hash
                  with type node = G.Value.Tree.t
                   and type commit = G.Value.Commit.t) :
    S
      with module Git = G
       and type Private.Remote.endpoint = endpoint
       and module Schema := Schema
end

module type KV_maker = sig
  module G : Irmin_git.G

  type endpoint = Mimic.ctx * Smart_git.Endpoint.t
  type branch

  module Make (C : Irmin.Contents.S) :
    S
      with module Git = G
       and type Schema.contents = C.t
       and type Schema.metadata = Irmin_git.Metadata.t
       and type Schema.info = Irmin.Info.default
       and type Schema.step = string
       and type Schema.hash = G.hash
       and type Schema.branch = branch
       and type Private.Remote.endpoint = endpoint
end

module type KV_RO = sig
  type git

  include Mirage_kv.RO

  val connect :
    ?depth:int ->
    ?branch:string ->
    ?root:key ->
    ?ctx:Mimic.ctx ->
    ?headers:Cohttp.Header.t ->
    git ->
    string ->
    t Lwt.t
  (** [connect ?depth ?branch ?path g uri] clones the given [uri] into [g]
      repository, using the given [branch], [depth] and ['/']-separated
      sub-[path]. By default, [branch] is master, [depth] is [1] and [path] is
      empty, ie. reads will be relative to the root of the repository. *)
end

module type KV_RW = sig
  type git

  include Mirage_kv.RW

  val connect :
    ?depth:int ->
    ?branch:string ->
    ?root:key ->
    ?ctx:Mimic.ctx ->
    ?headers:Cohttp.Header.t ->
    ?author:(unit -> string) ->
    ?msg:([ `Set of key | `Remove of key | `Batch ] -> string) ->
    git ->
    string ->
    t Lwt.t
  (** [connect ?depth ?branch ?path ?author ?msg g c uri] clones the given [uri]
      into [g] repository, using the given [branch], [depth] and ['/']-separated
      sub-[path]. By default, [branch] is master, [depth] is [1] and [path] is
      empty, ie. reads will be relative to the root of the repository. [author],
      [msg] and [c] are used to create new commit info values on every update.
      By defaut [author] is [fun () -> "irmin" <irmin@mirage.io>] and [msg]
      returns basic information about the kind of operations performed. *)
end

module type Sigs = sig
  module type S = S
  module type Maker = Maker
  module type KV_maker = KV_maker
  module type KV_RO = KV_RO
  module type KV_RW = KV_RW
end
