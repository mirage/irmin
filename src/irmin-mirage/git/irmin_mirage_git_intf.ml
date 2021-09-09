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
    ?ctx:Mimic.ctx -> ?headers:(string * string) list -> string -> Irmin.remote
end

module type Maker = sig
  module G : Irmin_git.G

  type endpoint = Mimic.ctx * Smart_git.Endpoint.t

  module Make
      (Schema : Irmin_git.Schema.S
                  with type Hash.t = G.hash
                  with type Node.t = G.Value.Tree.t
                   and type Commit.t = G.Value.Commit.t) :
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
       and type Schema.Contents.t = C.t
       and module Schema.Metadata = Irmin_git.Metadata
       and type Schema.Info.t = Irmin.Info.default
       and type Schema.Path.step = string
       and type Schema.Path.t = string list
       and type Schema.Hash.t = G.hash
       and type Schema.Branch.t = branch
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
    ?headers:(string * string) list ->
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
    ?headers:(string * string) list ->
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

  module Maker (G : Irmin_git.G) : Maker with module G := G

  module KV (G : Irmin_git.G) :
    KV_maker with type branch = string and module G := G

  module Ref (G : Irmin_git.G) :
    KV_maker with type branch = Irmin_git.reference and module G := G

  (** Functor to create a MirageOS' KV_RO store from a Git repository. The key
      ["/HEAD"] always shows the current HEAD. *)
  module KV_RO (G : Irmin_git.G) : KV_RO with type git := G.t

  (** Functor to create a MirageOS' KV_RW store from a Git repository. *)
  module KV_RW (G : Irmin_git.G) (C : Mirage_clock.PCLOCK) :
    KV_RW with type git := G.t

  (** Embed an Irmin store into an in-memory Git repository. *)
  module Mem : sig
    module G : Irmin_git.G

    type endpoint = Mimic.ctx * Smart_git.Endpoint.t

    module Make
        (Schema : Irmin_git.Schema.S
                    with type Hash.t = G.hash
                     and type Node.t = G.Value.Tree.t
                     and type Commit.t = G.Value.Commit.t) :
      S
        with module Git = G
         and type Private.Remote.endpoint = endpoint
         and module Schema := Schema

    module Ref : KV_maker with type branch = Irmin_git.reference
    module KV : KV_maker with type branch = string
    module KV_RO : KV_RO with type git := G.t
    module KV_RW (C : Mirage_clock.PCLOCK) : KV_RW with type git := G.t
  end
end
