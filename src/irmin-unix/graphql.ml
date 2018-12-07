open Lwt.Infix

module Server = struct
  module Backend = struct
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
    end) (Backend)
  end
end

module Client = struct
  module Backend = struct
    type t =
      { ctx : Cohttp_lwt_unix.Client.ctx option
      ; addr : Uri.t
      ; headers : Cohttp.Header.t option }

    let post t s =
      let body = Cohttp_lwt.Body.of_string s in
      Cohttp_lwt_unix.Client.post ?ctx:t.ctx ~body ?headers:t.headers t.addr
      >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
  end

  type client = Backend.t

  let init ?ctx ?headers addr = Backend.{ctx; addr; headers}

  module Make(Store: Irmin.S) = Irmin_graphql_client.Make (Backend)(Store)
end


