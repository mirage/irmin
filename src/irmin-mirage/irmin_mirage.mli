(*
 * Copyright (c) 2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** MirageOS backend, with bi-directional compatibility with Git *)

(** The context to use for synchronisation. *)

module Info (N: sig val name: string end)(C: Mirage_clock.PCLOCK): sig

  (** {1 Commit info creators} *)

  val f: C.t -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
  (** Commit info function, using [N.name] and [C.now_d_ps] provided
      in the functor arguments. *)

end

module type S = sig
  include Irmin_git.S with type Private.Sync.endpoint = Git_mirage.endpoint
  val remote:
    ?conduit:Conduit_mirage.conduit ->
    ?resolver:Resolver_lwt.t ->
    ?headers:Cohttp.Header.t ->
    string -> Irmin.remote
end

module Git: sig

  module Make
      (G: Irmin_git.G)
      (C: Irmin.Contents.S)
      (P: Irmin.Path.S)
      (B: Irmin.Branch.S):
    S with type key = P.t
       and type step = P.step
       and module Key = P
       and type contents = C.t
       and type branch = B.t
       and module Git = G

  module KV
      (G: Irmin_git.G)
      (C: Irmin.Contents.S):
    S with type key = string list
       and type step = string
       and type contents = C.t
       and type branch = string
       and module Git = G

  module Ref
      (G: Irmin_git.G)
      (C: Irmin.Contents.S):
    S with type key = string list
       and type step = string
       and type contents = C.t
       and type branch = Irmin_git.reference
       and module Git = G

  module type KV_RO = sig

    type git

    include Mirage_kv_lwt.RO

    val connect:
      ?depth:int ->
      ?branch:string ->
      ?path:string ->
      ?conduit:Conduit_mirage.t ->
      ?resolver:Resolver_lwt.t ->
      ?headers:Cohttp.Header.t ->
      git -> string -> t Lwt.t
      (** [connect ?depth ?branch ?path g uri] clones the given [uri] into
          [g] repository, using the given [branch], [depth] and
          ['/']-separated sub-[path]. By default, [branch] is master,
          [depth] is [1] and [path] is empty, ie. reads will be relative to
          the root of the repository. *)

  end

  (** Functor to create a MirageOS' KV_RO store from a Git
      repository. *)
  module KV_RO (G: Irmin_git.G): KV_RO with type git := G.t

  (** Embed an Irmin store into an in-memory Git repository. *)
  module Mem: sig

    module G: Irmin_git.G

    module Make
        (C: Irmin.Contents.S)
        (P: Irmin.Path.S)
        (B: Irmin.Branch.S):
      S with type key = P.t
         and type step = P.step
         and module Key = P
         and type contents = C.t
         and type branch = B.t
         and module Git = G

    module Ref (C: Irmin.Contents.S):
      S with type key = string list
         and type step = string
         and type contents = C.t
         and type branch = Irmin_git.reference
         and module Git = G

    module KV (C: Irmin.Contents.S):
      S with type key = Irmin.Path.String_list.t
         and type step = string
         and module Key = Irmin.Path.String_list
         and type contents = C.t
         and type branch = string
         and module Git = G

    module KV_RO: KV_RO with type git := G.t
  end
end

module Graphql: sig
  module Server: sig
    module type S = sig
      module Pclock: Mirage_clock_lwt.PCLOCK
      module Http: Cohttp_lwt.S.Server
      module Store: Irmin.S with type Private.Sync.endpoint = Git_mirage.endpoint

      val start:
        pclock:Pclock.t
        -> http:(Conduit_mirage.server -> Http.t -> unit Lwt.t)
        -> Conduit_mirage.server
        -> Store.t -> unit Lwt.t
    end

    module Make
        (Store: Irmin.S with type Private.Sync.endpoint = Git_mirage.endpoint)
        (Pclock: Mirage_clock_lwt.PCLOCK)
        (Http: Cohttp_lwt.S.Server):
      S with module Pclock = Pclock
         and module Store = Store
         and module Http = Http
  end

  module Client: sig
    module type S = sig
      module Store: Irmin.S
      module Http: Cohttp_lwt.S.Client
      include Irmin_graphql_client.S with module Store := Store

      type client
      val init: ?ctx:Http.ctx -> ?headers:Cohttp.Header.t -> Uri.t -> client
    end

    module Make
        (Store: Irmin.S)
        (Http: Cohttp_lwt.S.Client):
      S with module Store = Store
  end
end
