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

module Git: sig

  module Make
    (G: Irmin_git.G)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
    (B: Irmin.Branch.S):
    Irmin_git.S with type key = P.t
                 and type step = P.step
                 and module Key = P
                 and type contents = C.t
                 and type branch = B.t
                 and module Git = G
                 and type endpoint = Git_mirage.endpoint

  module KV
      (G: Irmin_git.G)
      (C: Irmin.Contents.S):
    Irmin_git.S with type key = string list
                 and type step = string
                 and type contents = C.t
                 and type branch = string
                 and module Git = G
                 and type endpoint = Git_mirage.endpoint

  module Ref
      (G: Irmin_git.G)
      (C: Irmin.Contents.S):
    Irmin_git.S with type key = string list
                 and type step = string
                 and type contents = C.t
                 and type branch = Irmin_git.reference
                 and module Git = G
                 and type endpoint = Git_mirage.endpoint

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
      git -> Uri.t -> t Lwt.t
    (** [connect ?depth ?branch ?path g uri] clones the given [uri] into
        [g] repository, using the given [branch], [depth] and
        ['/']-separated sub-[path]. By default, [branch] is master,
        [depth] is [1] and [path] is empty, ie. reads will be relative to
        the root of the repository. *)

  end

  (** Functor to create a MirageOS' KV_RO store from a Git
      repository. *)
  module KV_RO (G: Irmin_git.G): KV_RO with type git := G.t

  (** Embed an Irmin store into a local Git repository. *)
  module FS (S: Mirage_fs_lwt.S): sig

    module G: Irmin_git.G
    val set: S.t -> unit (* XXX(samoht): particularly ugly and wrong ... *)

    module Make
        (C: Irmin.Contents.S)
        (P: Irmin.Path.S)
        (B: Irmin.Branch.S):
      Irmin_git.S with type key = P.t
                   and type step = P.step
                   and module Key = P
                   and type contents = C.t
                   and type branch = B.t
                   and module Git = G
                   and type endpoint = Git_mirage.endpoint

    module Ref
        (C: Irmin.Contents.S):
      Irmin_git.S with type key = string list
                   and type step = string
                   and type contents = C.t
                   and type branch = Irmin_git.reference
                   and module Git = G
                   and type endpoint = Git_mirage.endpoint

    module KV
        (C: Irmin.Contents.S):
      Irmin_git.S with type key = Irmin.Path.String_list.t
                   and type step = string
                   and module Key = Irmin.Path.String_list
                   and type contents = C.t
                   and type branch = string
                   and module Git = G
                   and type endpoint = Git_mirage.endpoint

    module KV_RO: KV_RO with type git := G.t
  end

  (** Embed an Irmin store into an in-memory Git repository. *)
  module Mem: sig

    module G: Irmin_git.G

    module Make
        (C: Irmin.Contents.S)
        (P: Irmin.Path.S)
        (B: Irmin.Branch.S):
      Irmin_git.S with type key = P.t
                   and type step = P.step
                   and module Key = P
                   and type contents = C.t
                   and type branch = B.t
                   and module Git = G
                   and type endpoint = Git_mirage.endpoint

    module Ref (C: Irmin.Contents.S):
      Irmin_git.S with type key = string list
                   and type step = string
                   and type contents = C.t
                   and type branch = Irmin_git.reference
                   and module Git = G
                   and type endpoint = Git_mirage.endpoint

    module KV (C: Irmin.Contents.S):
      Irmin_git.S with type key = Irmin.Path.String_list.t
                   and type step = string
                   and module Key = Irmin.Path.String_list
                   and type contents = C.t
                   and type branch = string
                   and module Git = G
                   and type endpoint = Git_mirage.endpoint

    module KV_RO: KV_RO with type git := G.t
  end
end
