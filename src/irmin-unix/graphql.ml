open Lwt.Infix

module Server = struct
  module Server = struct
    include Cohttp_lwt_unix.Server

    type server = (Cohttp_lwt_unix.Net.ctx option * Conduit_lwt_unix.server)

    let run (ctx, mode) callback =
      let ctx = match ctx with
        | Some ctx -> ctx
        | None -> Cohttp_lwt_unix.Net.default_ctx
      in
      let server = Cohttp_lwt_unix.Server.make ~callback () in
      Cohttp_lwt_unix.Server.create ~ctx ~mode server
  end

  module Make(S: Irmin.S)(Remote: sig
      val remote: Resolver.Store.remote_fn option
    end) = struct
    include Irmin_graphql.Make(Server)(struct
        let info = Info.v
        let remote = Remote.remote
      end)(S)
  end

  type server = Server.server

  let init ?ctx mode = (ctx, mode)
end

module Client = struct
  type client = {
    ctx : Cohttp_lwt_unix.Client.ctx option;
    addr : Uri.t;
    headers : Cohttp.Header.t option
  }

  module Client = struct
    type t = client

    let post t s =
      let body = Cohttp_lwt.Body.of_string s in
      Cohttp_lwt_unix.Client.post ?ctx:t.ctx ~body ?headers:t.headers t.addr
      >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
  end

  let init ?ctx ?headers addr = {ctx; addr; headers}

  module Make(Store: Irmin.S) = Irmin_graphql_client.Make (Client)(Store)
end
