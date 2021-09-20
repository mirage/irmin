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

(** {1 Unix backends}

    This module provides Irmin backends for Unix applications. The currently
    supported backends are:

    - An {{!Irmin_mem} in-memory} store, internally using hash tables.
    - A {{!Irmin_fs} file-system} backend, using
      {{:https://github.com/janestreet/bin_prot} bin_prot} to serialize internal
      structures.
    - A fully compatible, bidirectional encoding of Irmin into {{!Irmin_git}
      Git}. You can view and edit your store using both the library and your
      usual Git tools.
    - The HTTP {{!module:Irmin_http} clients} and {{!module:Http.Server}
      servers} provides a high-level REST API, with 1 RTT for the
      {{!Irmin.S.Backend} private} and {{!Irmin.S} public} functions. *)

module Info = Info.Make

val info :
  ?author:string ->
  ('a, Format.formatter, unit, unit -> Irmin.Info.default) format4 ->
  'a
(** [info fmt ()] creates a fresh commit info, with the {{!Irmin.Info.date}
    date} set to [Unix.gettimeoday ()] and the {{!Irmin.Info.author} author}
    built using [Unix.gethostname()] and [Unix.getpid()] if [author] is not
    provided. *)

(** File system backends, using {{:https://github.com/janestreet/bin_prot}
    bin_prot}. *)
module FS : sig
  (** {1 File-system Store} *)

  module Append_only : Irmin.Append_only.Maker
  (** Append-only store maker. *)

  module Atomic_write : Irmin.Atomic_write.Maker
  (** Atomic-write store maker. *)

  include Irmin.Maker
  (** Irmin store maker. *)

  module KV : Irmin.KV_maker
  (** Irmin store make, where only the Contents have to be specified: branches
      are strings and paths are string lists. *)

  (** Append-only store maker, with control over the filenames shapes. *)
  module Append_only_ext (C : Irmin_fs.Config) : Irmin.Append_only.Maker

  (** Read-write store maker, with control over the filename shapes. *)
  module Atomic_write_ext (C : Irmin_fs.Config) : Irmin.Atomic_write.Maker

  (** Irmin store maker, with control over the filename shapes. *)
  module Maker_ext (Obj : Irmin_fs.Config) (Ref : Irmin_fs.Config) : Irmin.Maker
end

(** Bidirectional Git backends. *)
module Git : sig
  (** {1 Git Store} *)

  include Xgit_intf.Sigs
  (** @inline *)

  module Maker (G : Irmin_git.G) : Backend with module G = G

  module FS : Backend with module G = Git_unix.Store
  (** Embed an Irmin store into a local Git repository. *)

  module Mem : Backend with module G = Irmin_git.Mem
  (** Embed an Irmin store into an in-memory Git repository. *)
end

(** REST (over HTTP) backend.. *)
module Http : sig
  (** {1 HTTP client} *)

  (** [Make] provides bindings to the remote HTTP server.

      Only the {{!Irmin.S.Backend} low-level operations} are forwarded to the
      server, all the high-level logic is done on the client. Hence a high-level
      operation might take multiple RTTs. *)
  module Client (S : Irmin.S) :
    Irmin.S
      with type hash = S.Hash.t
       and module Schema = S.Schema
       and type Backend.Remote.endpoint = unit

  (** {1 HTTP server} *)

  (** Server-side of the REST API over HTTP. *)
  module Server (S : Irmin.S) :
    Irmin_http.SERVER
      with type repo = S.Repo.t
       and type t = Cohttp_lwt_unix.Server.t
end

val set_listen_dir_hook : unit -> unit
(** Install {!Irmin_watcher.hook} as the listen hook for watching changes in
    directories. *)

module Graphql = Graphql
module Cli = Cli
module Resolver = Resolver
