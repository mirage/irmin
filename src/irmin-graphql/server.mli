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
  val remote : (?headers:Cohttp.Header.t -> string -> Irmin.remote) option

  val info :
    ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
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
module Make (Server : Cohttp_lwt.S.Server) (Config : CONFIG) (Store : Irmin.S) :
  S with type repo = Store.repo and type server = Server.t

(** Create a GraphQL server with custom GraphQL types. *)
module Make_ext
    (Server : Cohttp_lwt.S.Server)
    (Config : CONFIG)
    (Store : Irmin.S)
    (Types : CUSTOM_TYPES
               with type key := Store.key
                and type metadata := Store.metadata
                and type contents := Store.contents
                and type hash := Store.hash
                and type branch := Store.branch) :
  S with type repo = Store.repo and type server = Server.t
