module Server: sig
  module Make(S: Irmin.S)(Remote: sig
      val remote: Resolver.Store.remote_fn option
    end):
    Irmin_graphql.S
    with type store = S.t
     and type server = (Conduit_lwt_unix.ctx option * Conduit_lwt_unix.server)
end

module Client: sig
  type client
  val init: ?ctx:Cohttp_lwt_unix.Net.ctx -> ?headers:Cohttp.Header.t -> Uri.t -> client
  module Make(S: Irmin.S) : Irmin_graphql_client.S with module Store = S
end
