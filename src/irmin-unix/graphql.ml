module Server = struct
  include Cohttp_lwt_unix.Server

  type server = (Conduit_lwt_unix.ctx option * Conduit_lwt_unix.server)

  let run (ctx, mode) callback =
    let ctx =
      match ctx with
      | Some ctx -> ctx
      | None -> Conduit_lwt_unix.default_ctx
    in
    let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
    let server = Cohttp_lwt_unix.Server.make ~callback () in
    Cohttp_lwt_unix.Server.create ~ctx ~mode server
end

module Make(S: Irmin.S)(Remote: sig
  val remote: Resolver.Store.remote_fn option
end) = struct
  include Irmin_graphql.Make(struct
    include S
    let info = Info.v
    let remote = Remote.remote
  end) (Server)
end

