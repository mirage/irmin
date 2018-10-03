open Lwt.Infix

module Server = struct
  include Cohttp_lwt_unix.Server

  let create ?(hostname = "127.0.0.1") ?(port = 8080) callback =
    Conduit_lwt_unix.init ~src:hostname () >>= fun ctx ->
    let srv = Cohttp_lwt_unix.Server.make ~callback () in
    let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
    Cohttp_lwt_unix.Server.create ~ctx ~mode:(`TCP (`Port port)) srv
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

