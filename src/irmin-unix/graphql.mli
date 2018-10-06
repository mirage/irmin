module Make(S: Irmin.S)(Remote: sig
  val remote: Resolver.Store.remote_fn option
end):
  Irmin_graphql.S
    with type store = S.t
     and type server = (Conduit_lwt_unix.ctx option * Conduit_lwt_unix.server)
