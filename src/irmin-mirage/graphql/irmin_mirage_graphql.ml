module Server = struct
  module type S = sig
    module Pclock : Mirage_clock.PCLOCK
    module Http : Cohttp_lwt.S.Server

    module Store :
      Irmin.S with type Private.Sync.endpoint = Smart_git.Endpoint.t

    val start : http:(Http.t -> unit Lwt.t) -> Store.repo -> unit Lwt.t
  end

  module Make
      (Http : Cohttp_lwt.S.Server)
      (Store : Irmin.S with type Private.Sync.endpoint = Smart_git.Endpoint.t)
      (Pclock : Mirage_clock.PCLOCK) =
  struct
    module Store = Store
    module Pclock = Pclock
    module Http = Http

    let init () =
      let module Config = struct
        let info ?(author = "irmin-graphql") fmt =
          let module I = Irmin_mirage.Info (Pclock) in
          I.f ~author fmt

        let remote =
          Some
            (fun ?headers uri ->
              let ( ! ) f a b = f b a in
              let headers = Option.map Cohttp.Header.to_list headers in
              match Smart_git.Endpoint.of_string uri with
              | Ok
                  ({ Smart_git.Endpoint.scheme = `HTTP _ | `HTTPS _; _ } as edn)
                ->
                  let edn =
                    Option.fold ~none:edn
                      ~some:(!Smart_git.Endpoint.with_headers_if_http edn)
                      headers
                  in
                  Store.E edn
              | Ok _ -> Fmt.invalid_arg "invalid remote: %s" uri
              | Error (`Msg err) -> Fmt.invalid_arg "invalid remote: %s" err)
      end in
      (module Irmin_graphql.Server.Make (Http) (Config) (Store)
      : Irmin_graphql.Server.S
        with type server = Http.t
         and type repo = Store.repo)

    let start ~http store =
      let (module G) = init () in
      let server = G.v store in
      http server
  end
end
