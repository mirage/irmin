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

(** JSON REST/CRUD interface. *)

val config : ?config:Irmin.config -> Uri.t -> Irmin.config

val uri : Uri.t option Irmin.Private.Conf.key

module type HTTP_CLIENT = sig
  include Cohttp_lwt.S.Client

  val ctx : unit -> ctx option
end

module Client (C : HTTP_CLIENT) (S : Irmin.S) :
  Irmin.S
    with type key = S.key
     and type contents = S.contents
     and type branch = S.branch
     and type hash = S.hash
     and type step = S.step
     and type metadata = S.metadata
     and type Key.step = S.Key.step
     and type Private.Sync.endpoint = unit

(** HTTP server *)

module type SERVER = sig
  type repo
  (** The type for Irmin repository. *)

  type t
  (** The type for HTTP configuration. *)

  val v : ?strict:bool -> repo -> t
  (** [v repo] returns the configuration for a server serving the contents of
      [repo]. If [strict] is set, incoming connections will fail if they do not
      have the right {i X-IrminVersion} headers. *)
end

(** Create an HTTP server, serving the contents of an Irmin database. *)
module Server (HTTP : Cohttp_lwt.S.Server) (S : Irmin.S) :
  SERVER with type repo = S.Repo.t and type t = HTTP.t
