module type S = sig
  type store 

  val schema : store -> unit Graphql_lwt.Schema.schema
  val start_server : store -> unit Lwt.t
end

module Make(Store : Irmin.S) : S with type store = Store.t
