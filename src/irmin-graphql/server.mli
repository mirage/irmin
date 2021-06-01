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

module Schema = Graphql_lwt.Schema

(** GraphQL server *)
module type S = sig
  module IO : Cohttp_lwt.S.IO

  type repo
  type server

  type response_action =
    [ `Expert of Cohttp.Response.t * (IO.ic -> IO.oc -> unit Lwt.t)
    | `Response of Cohttp.Response.t * Cohttp_lwt.Body.t ]

  val schema : repo -> unit Schema.schema

  val execute_request :
    unit Schema.schema ->
    Cohttp_lwt.Request.t ->
    Cohttp_lwt.Body.t ->
    response_action Lwt.t

  val v : repo -> server
end

(** GraphQL server config *)
module type CONFIG = sig
  type info

  val remote : (?headers:Cohttp.Header.t -> string -> Irmin.remote) option

  val info :
    ?author:string -> ('a, Format.formatter, unit, unit -> info) format4 -> 'a
end

(** Custom GraphQL schema type and argument type for [type t]. *)
module type CUSTOM_TYPE = sig
  type t

  val schema_typ : (unit, t option) Schema.typ
  val arg_typ : t option Schema.Arg.arg_typ
end

(** GraphQL types for Irmin concepts (key, metadata, contents, hash and branch). *)
module type CUSTOM_TYPES = sig
  type key
  type metadata
  type contents
  type hash
  type branch

  module Key : CUSTOM_TYPE with type t := key
  module Metadata : CUSTOM_TYPE with type t := metadata
  module Contents : CUSTOM_TYPE with type t := contents
  module Hash : CUSTOM_TYPE with type t := hash
  module Branch : CUSTOM_TYPE with type t := branch
end

(** Default GraphQL types for the Irmin store [S]. *)
module Default_types (S : Irmin.S) :
  CUSTOM_TYPES
    with type key := S.key
     and type metadata := S.metadata
     and type contents := S.contents
     and type hash := S.hash
     and type branch := S.branch

(** Create a GraphQL server with default GraphQL types for [S]. *)
module Make
    (Server : Cohttp_lwt.S.Server)
    (Config : CONFIG)
    (Store : Irmin.S with type Schema.info = Config.info) :
  S with type repo = Store.repo and type server = Server.t

(** Create a GraphQL server with custom GraphQL types. *)
module Make_ext
    (Server : Cohttp_lwt.S.Server)
    (Config : CONFIG)
    (Store : Irmin.S with type Schema.info = Config.info)
    (Types : CUSTOM_TYPES
               with type key := Store.key
                and type metadata := Store.metadata
                and type contents := Store.contents
                and type hash := Store.hash
                and type branch := Store.branch) :
  S with type repo = Store.repo and type server = Server.t
