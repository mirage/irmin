module Schema : sig
  include Graphql_intf.Schema with type 'a io = 'a Lwt.t
end

module type S = sig
  type store
  type server

  val schema : store -> unit Schema.schema
  val execute_request : unit Schema.schema -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
  val run_server: server -> store -> unit Lwt.t
end

module type STORE = sig
  include Irmin.S
  val remote: (?headers:Cohttp.Header.t -> string -> Irmin.remote) option
  val info: ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end

module type SERVER = sig
  type conn
  type server

  val respond_string :
    ?flush:bool ->
    ?headers:Cohttp.Header.t ->
    status:Cohttp.Code.status_code ->
    body:string -> unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

  val run : server -> (conn -> Cohttp_lwt.Request.t -> Cohttp_lwt.Body.t -> (Cohttp_lwt.Response.t * Cohttp_lwt.Body.t) Lwt.t) -> unit Lwt.t
end

module Make(Store : STORE)(Server : SERVER) : S
  with type store = Store.t
   and type server = Server.server
