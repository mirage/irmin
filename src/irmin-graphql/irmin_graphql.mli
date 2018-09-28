module type S = sig
  type store

  val schema : store -> unit Graphql_lwt.Schema.schema
  val start_server : ?port:int -> store -> unit Lwt.t
end

module type STORE = sig
  include Irmin.S
  val remote: (?headers:Cohttp.Header.t -> string -> Irmin.remote) option
  val info: ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end

module Make(Store : STORE) : S with type store = Store.t
