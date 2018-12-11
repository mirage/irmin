module Remote: sig
  module None: sig
    val remote: Resolver.Store.remote_fn option
  end
end

module Make(S: Irmin.S)(Remote: sig
  val remote: Resolver.Store.remote_fn option
end):
  Irmin_graphql.S
    with type store = S.t
     and type server = (Cohttp_lwt_unix.Net.ctx option * Conduit_lwt_unix.server)
