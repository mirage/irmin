(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** {1 Unix backends}

    This module provides Irmin backends for Unix applications. The
    currently supported backends are:

    {ul
    {- An {{!Irmin_mem}in-memory} store, internally using hash tables. }
    {- A {{!Irmin_fs}file-system} backend, using
    {{:https://github.com/janestreet/bin_prot}bin_prot} to serialize
    internal structures.}
    {- A fully compatible, bidirectional encoding of Irmin into
    {{!Irmin_git}Git}. You can view and edit your store using both the
    library and your usual Git tools. }
    {- The HTTP {{!module:Irmin_http}clients} and
    {{!module:Http.Server}servers} provides a high-level REST API, with
    1 RTT for the {{!Irmin.S.Private}private} and {{!Irmin.S}public}
    functions.}
    }

*)

val info: ?author:string ->
  ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
(** [info fmt ()] creates a fresh commit info, with the
    {{!Irmin.Info.date}date} set to [Unix.gettimeoday ()] and the
    {{!Irmin.Info.author}author} built using [Unix.gethostname()] and
    [Unix.getpid()] if [author] is not provided.  *)

(** File system backends, using
    {{:https://github.com/janestreet/bin_prot}bin_prot}. *)
module FS: sig

  (** {1 File-system Store} *)

  module AO: Irmin.AO_MAKER
  (** Append-only store maker. *)

  module Link: Irmin.LINK_MAKER
  (** Immutable store for links. *)

  module RW: Irmin.RW_MAKER
  (** Read-write store maker. *)

  module Make: Irmin.S_MAKER
  (** Irmin store maker. *)

  module KV: Irmin.KV_MAKER
  (** Irmin store make, where only the Contents have to be specified:
      branches are strings and paths are string lists. *)

  module AO_ext (C: Irmin_fs.Config): Irmin.AO_MAKER
  (** Append-only store maker, with control over the filenames shapes. *)

  module RW_ext (C: Irmin_fs.Config): Irmin.RW_MAKER
  (** Read-write store maker, with control over the filename shapes. *)

  module Make_ext (Obj: Irmin_fs.Config) (Ref: Irmin_fs.Config): Irmin.S_MAKER
  (** Irmin store maker, with control over the filename shapes. *)

end

(** Bidirectional Git backends. *)
module Git: sig

  (** {1 Git Store} *)

  module AO (G: Git.Store.S) (V: Irmin.Contents.Conv):
    Irmin.AO with type t = G.t
              and type key = Irmin.Hash.SHA1.t
              and type value = V.t
  (** Embed an append-only store into a Git repository. Contents will
      be written in {i .git/objects/} and might be cleaned-up if you
      run {i git gc} manually. *)

  module RW (G: Git.Store.S) (K: Irmin.Branch.S): Irmin.RW
    with type key = K.t and type value = Irmin.Hash.SHA1.t
  (** Embed a read-write store into a Git repository. Contents will be
      written in {i .git/refs}. *)

  (** Embed an Irmin store into an in-memory Git repository. *)
  module Mem: sig
    module Make: Irmin_git.S_MAKER
    module KV: Irmin_git.KV_MAKER
  end

  (** Embed an Irmin store into a local Git repository. *)
  module FS: sig
    module Make: Irmin_git.S_MAKER
    module KV: Irmin_git.KV_MAKER
  end

end

(** REST (over HTTP) backend.. *)
module Http: sig

  (** {1 HTTP client} *)

  module Make: Irmin.S_MAKER
  (** [Make] provides bindings to the remote HTTP server.

      Only the {{!Irmin.S.Private}low-level operations} are forwarded
      to the server, all the high-level logic is done on the
      client. Hence a high-level operation might take multiple
      RTTs. *)

  module KV: Irmin.KV_MAKER

  (** {1 HTTP server} *)

  (** Server-side of the REST API over HTTP. *)
  module Server (S: Irmin.S): Irmin_http_server.S
    with type repo = S.Repo.t
     and type t = Cohttp_lwt_unix.Server.t

end

val set_listen_dir_hook: unit -> unit
(** Install {!Irmin_watcher.hook} as the listen hook for watching
    changes in directories. *)
