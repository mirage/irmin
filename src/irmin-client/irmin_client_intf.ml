open Irmin_server
module Error = Error

module type Irmin_client = sig
  module type S = Client.S

  module Error = Error
  module Client = Client

  type addr = Client.addr

  val config : ?tls:bool -> ?hostname:string -> Uri.t -> Irmin.config

  module Make (IO : Client.IO) (Store : Irmin.Generic_key.S) :
    S
      with module Schema = Store.Schema
       and type Backend.Remote.endpoint = unit
       and type commit_key = Store.commit_key
       and type contents_key = Store.contents_key
       and type node_key = Store.node_key

  module Make_codec
      (IO : Client.IO)
      (Codec : Conn.Codec.S)
      (Store : Irmin.Generic_key.S) :
    S
      with module Schema = Store.Schema
       and type Backend.Remote.endpoint = unit
       and type commit_key = Store.commit_key
       and type contents_key = Store.contents_key
       and type node_key = Store.node_key

  module Make_json (IO : Client.IO) (Store : Irmin.Generic_key.S) :
    S
      with module Schema = Store.Schema
       and type Backend.Remote.endpoint = unit
       and type commit_key = Store.commit_key
       and type contents_key = Store.contents_key
       and type node_key = Store.node_key
end
