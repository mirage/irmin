module Schema : Graphql_intf.Schema with type 'a Io.t = 'a Lwt.t

module type S = sig
  module IO : Cohttp_lwt.S.IO
  type store
  type server

  type response_action =
    [ `Expert of Cohttp.Response.t
                 * (IO.ic
                    -> IO.oc
                    -> unit Lwt.t)
    | `Response of Cohttp.Response.t * Cohttp_lwt.Body.t ]

  val schema : store -> unit Schema.schema
  val execute_request :
      unit Schema.schema ->
      Cohttp_lwt.Request.t ->
      Cohttp_lwt.Body.t -> response_action Lwt.t
  val server : store -> server
end

module type CONFIG = sig
  val remote: (?headers:Cohttp.Header.t -> string -> Irmin.remote) option
  val info: ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end

module Make(Server: Cohttp_lwt.S.Server)(Config: CONFIG)(Store : Irmin.S): S
  with type store = Store.t
   and type server = Server.t
