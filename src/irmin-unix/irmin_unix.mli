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
(** [info fmt ()] creates a fresh commit info, with the {{!Irmin.Info.S.date}
    date} set to [Unix.gettimeoday ()] and the {{!Irmin.Info.S.author} author}
    built using [Unix.gethostname()] and [Unix.getpid()] if [author] is not
    provided. *)

module FS : module type of Irmin_fs_unix
(** File system backends, using {{:https://github.com/janestreet/bin_prot}
    bin_prot}. *)

module Git : module type of Irmin_git_unix
(** Bidirectional Git backends. *)

module Http : module type of Irmin_http_unix
(** REST (over HTTP) backend.. *)

val set_listen_dir_hook : unit -> unit
(** Install {!Irmin_watcher.hook} as the listen hook for watching changes in
    directories. *)

module Graphql : module type of Irmin_graphql_unix
module Cli = Cli
module Resolver = Resolver
