(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Server = struct
  module type S = sig
    module Pclock : Mirage_clock.PCLOCK
    module Http : Cohttp_lwt.S.Server

    module Store :
      Irmin.S with type Backend.Remote.endpoint = Smart_git.Endpoint.t

    val start : http:(Http.t -> unit Lwt.t) -> Store.repo -> unit Lwt.t
  end

  module Make
      (Http : Cohttp_lwt.S.Server)
      (Store : Irmin.S with type Backend.Remote.endpoint = Smart_git.Endpoint.t)
      (Pclock : Mirage_clock.PCLOCK) =
  struct
    module Store = Store
    module Pclock = Pclock
    module Http = Http

    let init () =
      let module Config = struct
        type info = Store.info

        let info ?(author = "irmin-graphql") fmt =
          let module I = Irmin_mirage.Info (Store.Info) (Pclock) in
          I.f ~author fmt

        let remote =
          Some
            (fun ?headers uri () ->
              Lwt_eio.run_lwt @@ fun () ->
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
                  Lwt.return (Store.E edn)
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
