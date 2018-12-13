module Server = struct
  include Cohttp_lwt_unix.Server

  type server = (Cohttp_lwt_unix.Net.ctx option * Conduit_lwt_unix.server)

  let run (ctx, mode) callback =
    let ctx = match ctx with
      | Some ctx -> ctx
      | None -> Cohttp_lwt_unix.Net.default_ctx
    in
    let server = Cohttp_lwt_unix.Server.make ~callback () in
    let on_exn = fun e ->
      Logs.debug (fun l -> l "Server exception: %s" (Printexc.to_string e)) in
    Cohttp_lwt_unix.Server.create ~on_exn ~ctx ~mode server
end

module Remote = struct
  module None = struct
    let remote = None
  end
end

module Make(S: Irmin.S)(Remote: sig
  val remote: Resolver.Store.remote_fn option
end) = struct
  include Irmin_graphql.Make(Server)(struct
    let info = Info.v
    let remote = Remote.remote
  end)(S)
end
