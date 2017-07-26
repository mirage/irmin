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

(** Irmin store resolver. *)

(** {1 Contents} *)

type contents = (module Irmin.Contents.S)

val contents: contents Cmdliner.Term.t

(** {1 Global Configuration} *)

type t = S: (module Irmin.S with type t = 'a) * 'a Lwt.t -> t
(** The type for store configurations. A configuration value contains:
    the store implementation a creator of store's state. *)

val store: t Cmdliner.Term.t
(** Parse the command-line arguments and then the config file. *)

val remote: Irmin.remote Lwt.t Cmdliner.Term.t
(** Parse a remote store location. *)

(** {1 Stores} *)

val mem_store: contents -> (module Irmin.S)
val irf_store: contents -> (module Irmin.S)
val http_store: contents -> (module Irmin.S)
val git_store: contents -> (module Irmin.S)
