module Schema : Graphql_intf.Schema with type 'a Io.t = 'a Lwt.t

module type S = sig
  module IO : Cohttp_lwt.S.IO
  type repo
  type server

  type response_action =
    [ `Expert of Cohttp.Response.t
                 * (IO.ic
                    -> IO.oc
                    -> unit Lwt.t)
    | `Response of Cohttp.Response.t * Cohttp_lwt.Body.t ]

  val schema : repo -> unit Schema.schema

  val execute_request :
      unit Schema.schema ->
      Cohttp_lwt.Request.t ->
      Cohttp_lwt.Body.t -> response_action Lwt.t
  val v : repo -> server
end

module type CONFIG = sig
  val remote: (?headers:Cohttp.Header.t -> string -> Irmin.remote) option
  val info: ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end

module type PRESENTER = sig
  type t
  type src

  val to_src : t -> src
  val schema_typ : (unit, src option) Schema.typ
end

module type PRESENTATION = sig
  module Contents : PRESENTER
  module Metadata : PRESENTER
end

module Default_presenter (T : Irmin.Type.S) : PRESENTER with type t = T.t

module Make(Server: Cohttp_lwt.S.Server)(Config: CONFIG)(Store : Irmin.S): S
  with type repo = Store.repo
   and type server = Server.t

module Make_ext(Server: Cohttp_lwt.S.Server)(Config: CONFIG)(Store : Irmin.S)(Presentation : PRESENTATION with type Contents.t = Store.contents and type Metadata.t = Store.metadata): S
  with type repo = Store.repo
   and type server = Server.t
