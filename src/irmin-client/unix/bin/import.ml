type client = S : ((module Irmin_client.S with type t = 'a) * 'a) -> client
