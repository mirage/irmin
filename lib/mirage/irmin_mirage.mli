(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Bidirectional Git backends. *)

module type CONTEXT = sig val v: unit -> Git_mirage.Sync.IO.ctx option Lwt.t end
(** The context to use for synchronisation. *)

module Irmin_git: sig

  (** {1 Git Store} *)

  val config:
    ?root:string -> ?head:Git.Reference.t -> ?bare:bool -> unit -> Irmin.config
  (** Create a configuration value.

      {ul
      {- [root] is the local Git repository's root (the parent of the
      {e .git/} directory).}
      {- [head] is the name of the local Git repository's current
      branch. If set, this will cause the file {i [root]/.git/HEAD} to
      be modified to contain {i ref: refs/heads/[branch]}.}
      {- If [bare] is set (default is {e unset}), then the local Git
      repository's contents will be expanded into the filesystem on
      each update. This might cause some performance issues.}
      } *)

  val head: Git.Reference.t option Irmin.Private.Conf.key
  (** The configuration key to set the local Git repository's current
      branch. See {!Irmin_git.config}. *)

  val bare: bool Irmin.Private.Conf.key
  (** The configuration key to set the local Git repository's bare
      attribute. See {!Irmin_git.config}.*)

  module AO (G: Git.Store.S): Irmin.AO_MAKER
  (** Embed an append-only store into a Git repository. Contents will
      be written in {i .git/objects/} and might be cleaned-up if you
      run {i git gc} manually. *)

  module Memory (C: CONTEXT) (I: Git.Inflate.S): Irmin.S_MAKER
  (** Embed an Irmin store into an in-memory Git repository. *)

end

module Task (N: sig val name: string end)(C: V1.CLOCK): sig

  (** {1 Task creators} *)

  val f: string Irmin.Task.f
  (** Task creators, using [N.name] and [C.time ()] provided in the
      functor arguments. *)

end

(** Functor to create a MirageOS' KV_RO store from a Git
    repository. *)
module KV_RO (C: CONTEXT) (I: Git.Inflate.S): sig

  include V1_LWT.KV_RO

  val connect: ?depth:int -> ?branch:string -> ?path:string ->
    Uri.t -> t Lwt.t
  (** [connect ?depth ?branch ?path uri] clones the given [uri] using
      the given [branch], [depth] and ['/']-separated sub-[path]. By
      defaut, [branch] is master, [depth] is [1] and [path] is empty,
      ie. reads will be relative to the root of the repository. *)

end
