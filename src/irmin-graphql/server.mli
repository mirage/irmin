module Schema = Graphql_lwt.Schema

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

module type CONFIG = sig
  val remote : (?headers:Cohttp.Header.t -> string -> Irmin.remote) option

  val info :
    ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end

module type PRESENTER = sig
  type t

  type key

  type tree

  type src

  val to_src : tree -> key -> t -> src

  val schema_typ : (unit, src option) Schema.typ
end

module type PRESENTATION = sig
  type tree

  type key

  type contents

  type metadata

  module Contents :
    PRESENTER with type tree := tree and type key := key and type t := contents

  module Metadata :
    PRESENTER with type tree := tree and type key := key and type t := metadata
end

module Default_presentation (S : Irmin.S) :
  PRESENTATION
    with type contents := S.contents
     and type metadata := S.metadata
     and type tree := S.tree
     and type key := S.key

module Make (Server : Cohttp_lwt.S.Server) (Config : CONFIG) (Store : Irmin.S) :
  S with type repo = Store.repo and type server = Server.t

module Make_ext
    (Server : Cohttp_lwt.S.Server)
    (Config : CONFIG)
    (Store : Irmin.S)
    (Presentation : PRESENTATION
                      with type contents := Store.contents
                       and type metadata := Store.metadata
                       and type tree := Store.tree
                       and type key := Store.key) :
  S with type repo = Store.repo and type server = Server.t
