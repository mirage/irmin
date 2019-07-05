module Server = struct
  module type S = sig
    module Pclock : Mirage_clock_lwt.PCLOCK

    module Http : Cohttp_lwt.S.Server

    module Store :
      Irmin.S with type Private.Sync.endpoint = Git_mirage.endpoint

    val start :
      pclock:Pclock.t ->
      http:(Http.t -> unit Lwt.t) ->
      Store.repo ->
      unit Lwt.t
  end

  module Make
      (Http : Cohttp_lwt.S.Server)
      (Store : Irmin.S with type Private.Sync.endpoint = Git_mirage.endpoint)
      (Pclock : Mirage_clock_lwt.PCLOCK) =
  struct
    module Store = Store
    module Pclock = Pclock
    module Http = Http

    let init p =
      let module Config = struct
        let info ?(author = "irmin-graphql") fmt =
          let module I = Irmin_mirage.Info (Pclock) in
          I.f ~author p fmt

        let remote =
          Some
            (fun ?headers uri ->
              let e = Git_mirage.endpoint ?headers (Uri.of_string uri) in
              Store.E e)
      end in
      ( module Irmin_graphql.Server.Make (Http) (Config) (Store)
      : Irmin_graphql.Server.S
        with type server = Http.t
         and type repo = Store.repo )

    let start ~pclock ~http store =
      let (module G) = init pclock in
      let server = G.v store in
      http server
  end
end
