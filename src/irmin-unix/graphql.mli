module Server: sig
  type server = Cohttp_lwt_unix.Net.ctx option * Conduit_lwt_unix.server
  val init: ?ctx:Cohttp_lwt_unix.Net.ctx -> Conduit_lwt_unix.server -> server

  module Make(S: Irmin.S)(Remote: sig
      val remote: Resolver.Store.remote_fn option
    end):
    Irmin_graphql.Server.S
    with type store = S.t
     and type server = server
end

module Client: sig
  type client
  val init: ?ctx:Cohttp_lwt_unix.Net.ctx -> ?headers:Cohttp.Header.t -> Uri.t -> client
  module Make(S: Irmin.S) : Irmin_graphql.Client.S with module Store = S with type t = client
end
