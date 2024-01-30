(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

open Irmin_client_unix
open Util
module Info = Info (Client.Info)

let info = Info.v

let () =
  let style_renderer = `None in
  Fmt_tty.setup_std_outputs ~style_renderer ();
  Logs.set_level (Some Logs.Error);
  Logs.set_reporter (Logs_fmt.reporter ())

module type R = sig
  val pid : int
  val uri : Uri.t
  val kind : string
end

module Make (R : R) = struct
  let () = at_exit (fun () -> try Unix.kill R.pid Sys.sigint with _ -> ())
  let config = Irmin_client_unix.config R.uri

  module X = Irmin_mem.KV.Make (Irmin.Contents.String)
  module Store = Irmin_client_unix.Make (X)

  let suite () =
    Eio.Switch.run @@ fun sw ->
    let client = Client.Repo.v ~sw config in
    let clean ~config:_ = Client.Branch.remove client "main" in
    Irmin_test.Suite.create_generic_key ~name:R.kind
      ~store:(module Store)
      ~config ~clean ()
end

let error =
  Alcotest.testable (Fmt.using Error.to_string Fmt.string) (fun a b ->
      Error.to_string a = Error.to_string b)

let ty t =
  Alcotest.testable
    (Fmt.using (Irmin.Type.to_string t) Fmt.string)
    (fun a b -> Irmin.Type.(unstage (equal t)) a b)

let ping client () =
  let open Client in
  let client = client () in
  Logs.debug (fun l -> l "BEFORE PING");
  let r = ping client in
  Logs.debug (fun l -> l "AFTER PING");
  Alcotest.(check (result unit error)) "ping" (Ok ()) r

let misc client = [ ("ping", `Quick, ping client) ]
let misc client = [ ("misc", misc client) ]

let main () =
  Eio.Switch.run @@ fun sw ->
  let kind, pid, uri = run_server ~sw `Unix_domain in
  let config = Irmin_client_unix.config uri in
  let client = Client.Repo.v ~sw config in
  let client () = Lwt_eio.run_lwt @@ fun () -> Client.dup client in
  let module Unix_socket = Make (struct
    let pid = pid
    let uri = uri
    let kind = kind
  end) in
  let module Tcp_socket = Make (struct
    let kind, pid, uri = run_server ~sw `Tcp
  end) in
  let module Websocket = Make (struct
    let kind, pid, uri = run_server ~sw `Websocket
  end) in
  let slow = Sys.getenv_opt "SLOW" |> Option.is_some in
  let only = Sys.getenv_opt "ONLY" in
  let tests =
    match only with
    | Some "ws" -> [ (`Quick, Websocket.suite ()) ]
    | Some "tcp" -> [ (`Quick, Tcp_socket.suite ()) ]
    | Some "unix" -> [ (`Quick, Unix_socket.suite ()) ]
    | Some s -> failwith ("Invalid selection: " ^ s)
    | None ->
        [
          (`Quick, Unix_socket.suite ());
          (`Quick, Tcp_socket.suite ());
          (`Quick, Websocket.suite ());
        ]
  in
  Irmin_test.Store.run "irmin-server" ~sleep:Eio_unix.sleep ~slow
    ~misc:(misc client) tests

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> main ()
