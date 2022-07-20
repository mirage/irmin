open Irmin_server
module Error = Error

module type S = Client.S

module Client = Client

type addr = Client_intf.addr

let config = Client.config

module Make_codec
    (IO : Client.IO)
    (Codec : Conn.Codec.S)
    (Store : Irmin.Generic_key.S) =
struct
  include Client.Make (IO) (Codec) (Store)
end

module Make (IO : Client.IO) (Store : Irmin.Generic_key.S) =
  Make_codec (IO) (Conn.Codec.Bin) (Store)

module Make_json (IO : Client.IO) (Store : Irmin.Generic_key.S) =
  Make_codec (IO) (Conn.Codec.Json) (Store)
