module Error = Irmin_client.Error
module IO : Irmin_client.Client.IO

module Info (I : Irmin.Info.S) : sig
  include Irmin.Info.S with type t = I.t

  val init : ?author:string -> ?message:string -> int64 -> t
  val v : ?author:string -> ('b, Format.formatter, unit, f) format4 -> 'b
end

module Make (Store : Irmin.Generic_key.S) :
  Irmin_client.S
    with module Schema = Store.Schema
     and type Backend.Remote.endpoint = unit
     and type commit_key = Store.commit_key
     and type contents_key = Store.contents_key
     and type node_key = Store.node_key

module Make_codec
    (Codec : Irmin_server.Conn.Codec.S)
    (Store : Irmin.Generic_key.S) :
  Irmin_client.S
    with module Schema = Store.Schema
     and type Backend.Remote.endpoint = unit
     and type commit_key = Store.commit_key
     and type contents_key = Store.contents_key
     and type node_key = Store.node_key

module Make_json (Store : Irmin.Generic_key.S) :
  Irmin_client.S
    with module Schema = Store.Schema
     and type Backend.Remote.endpoint = unit
     and type commit_key = Store.commit_key
     and type contents_key = Store.contents_key
     and type node_key = Store.node_key

val config : ?tls:bool -> ?hostname:string -> Uri.t -> Irmin.config
