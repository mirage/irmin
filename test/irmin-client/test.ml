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

let () =
  let style_renderer = `None in
  Fmt_tty.setup_std_outputs ~style_renderer ();
  Logs.set_level (Some Logs.Error);
  Logs.set_reporter (Logs_fmt.reporter ())

module type R = sig
  val uri : Uri.t
  val kind : string
end

module Make (R : R) = struct
  let config = Irmin_client_unix.config R.uri

  module X = Irmin_mem.KV.Make (Irmin.Contents.String)
  module Store = Irmin_client_unix.Make (X)

  let suite () =
    let client = Client.Repo.v config in
    let clean ~config:_ = Client.Branch.remove client "main" in
    Irmin_test.Suite.create_generic_key ~name:R.kind
      ~store:(module Store)
      ~config ~clean ()
end

let error =
  Alcotest.testable (Fmt.using Error.to_string Fmt.string) (fun a b ->
      Error.to_string a = Error.to_string b)

let ping client () =
  let open Client in
  let client = client () in
  Logs.debug (fun l -> l "BEFORE PING");
  let r = ping client in
  Logs.debug (fun l -> l "AFTER PING");
  Alcotest.(check (result unit error)) "ping" (Ok ()) r

let origin s =
  match Irmin_server_unix.Origin.of_string s with
  | Ok origin -> origin
  | Error (`Msg msg) -> failwith msg

(* A websocket opened by a web page is only accepted from the allowed
   origins. Other clients do not send the header. *)
let websocket_origin ~clock () =
  let port = free_port () in
  Eio.Switch.run @@ fun sw ->
  start_server ~sw ~clock
    ~allowed_origins:[ origin "HTTPS://Allowed.Example:443/" ]
    "Origin" (websocket_uri port);
  let check_headers msg expected headers =
    Alcotest.(check bool)
      msg expected
      (with_connection ~port (upgrade ~headers ~port))
  in
  let check ?(header = "Origin") msg expected origins =
    check_headers msg expected
      (List.map (fun origin -> header ^ ": " ^ origin) origins)
  in
  check "no origin" true [];
  check "allowed origin" true [ "https://allowed.example" ];
  check "case of the origin" true [ "HTTPS://Allowed.Example" ];
  check "default port" true [ "https://allowed.example:443" ];
  check "other port" false [ "https://allowed.example:8443" ];
  check "other scheme" false [ "http://allowed.example" ];
  check "other origin" false [ "https://evil.example" ];
  check "not an origin" false [ "null" ];
  check "prefix of an allowed origin" false [ "https://allowed.example.evil" ];
  check "several origins" false
    [ "https://evil.example"; "https://allowed.example" ];
  check ~header:"Sec-WebSocket-Origin" "origin of the old drafts" false
    [ "https://evil.example" ];
  check ~header:"Sec-WebSocket-Origin" "allowed origin of the old drafts" true
    [ "https://allowed.example" ];
  check_headers "both origin headers" false
    [
      "Origin: https://allowed.example";
      "Sec-WebSocket-Origin: https://allowed.example";
    ]

let origin_of_string () =
  let normalised s expected =
    Alcotest.(check (result string reject))
      s (Ok expected)
      (Result.map Irmin_server_unix.Origin.to_string
         (Irmin_server_unix.Origin.of_string s))
  in
  normalised "https://example.org" "https://example.org";
  normalised "HTTPS://Example.ORG:443/" "https://example.org";
  normalised "http://example.org:80" "http://example.org";
  normalised "http://example.org:443" "http://example.org:443";
  normalised "http://localhost:8080" "http://localhost:8080";
  normalised "http://127.0.0.1:65535" "http://127.0.0.1:65535";
  normalised "http://[::1]:8080" "http://[::1]:8080";
  normalised "https://xn--bcher-kva.example" "https://xn--bcher-kva.example";
  normalised "https://example.org." "https://example.org.";
  normalised "http://10.0.0.255" "http://10.0.0.255";
  normalised "http://1.example" "http://1.example";
  normalised "http://foo.0xg" "http://foo.0xg";
  normalised "http://[2001:DB8::1]" "http://[2001:db8::1]";
  normalised "http://[1:0:0:2::3]" "http://[1:0:0:2::3]";
  normalised "http://[1::]" "http://[1::]";
  List.iter
    (fun s ->
      match Irmin_server_unix.Origin.of_string s with
      | Ok origin ->
          Alcotest.failf "%S should not be an origin, got %a" s
            Irmin_server_unix.Origin.pp origin
      | Error _ -> ())
    [
      "";
      "null";
      "example.org";
      "//example.org";
      "https://";
      "https:///";
      "https://example.org/page";
      "https://example.org//";
      "https://example.org?";
      "https://example.org#";
      "https://user@example.org";
      "https://example.org:";
      "https://example.org:0";
      "https://example.org:0443";
      "https://example.org:65536";
      "https://example.org:99999999999999999999";
      "https://example%2Eorg";
      " https://example.org";
      "https://example.org ";
      "https://exa mple.org";
      "https://example.org, https://example.com";
      "https://[::1";
      "https://[]";
      "https://[1.2.3.4]";
      "http://127.1";
      "http://0x7f.0.0.1";
      "http://foo.0x1f";
      "http://127.0.0.01";
      "http://127.0.0.256";
      "http://127.0.0.1.";
      "http://[0:0::1]";
      "http://[::0:1]";
      "http://[0001::1]";
      "http://[:::::]";
      "http://[1::2::3]";
      "http://[1::2:0:0:0:3]";
      "http://[1:2:3:4:5:6:7::]";
      "http://[1:2:3:4:5:6:7:8:9]";
      "http://[::ffff:127.0.0.1]";
      "https://example..org";
      "https://.example.org";
      "https://bücher.example";
    ]

let misc client = [ ("ping", `Quick, ping client) ]
let misc client = [ ("misc", misc client) ]

let websocket_misc ~clock =
  [ ("misc", [ ("websocket origin", `Quick, websocket_origin ~clock) ]) ]

let unix_suite ~env () =
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let kind, uri = run_server ~sw ~clock `Unix_domain in
  let module Unix_socket = Make (struct
    let uri = uri
    let kind = kind
  end) in
  let tests = Unix_socket.suite () in
  let config = Irmin_client_unix.config uri in
  let client = Client.Repo.v config in
  let client () = Lwt_eio.run_lwt @@ fun () -> Client.dup client in
  Irmin_test.Store.run "irmin-server.unix" ~and_exit:false ~sleep:Eio_unix.sleep
    ~misc:(misc client)
    [ (`Quick, tests) ]

let tcp_suite ~env () =
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let module Tcp_socket = Make (struct
    let kind, uri = run_server ~sw ~clock `Tcp
  end) in
  let tests = Tcp_socket.suite () in
  Irmin_test.Store.run "irmin-server.tcp" ~and_exit:false ~sleep:Eio_unix.sleep
    ~misc:[]
    [ (`Quick, tests) ]

let websocket_suite ~env () =
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let module Websocket = Make (struct
    let kind, uri = run_server ~sw ~clock `Websocket
  end) in
  let tests = Websocket.suite () in
  Irmin_test.Store.run "irmin-server.ws" ~and_exit:false ~sleep:Eio_unix.sleep
    ~misc:(websocket_misc ~clock)
    [ (`Quick, tests) ]

(* The parser of the origins does not need a server *)
let origin_suite ~env:_ () =
  Alcotest.run "irmin-server.origin" ~and_exit:false
    [ ("origin", [ Alcotest.test_case "of_string" `Quick origin_of_string ]) ]

let main ~env () =
  let suites =
    [
      ("tcp", tcp_suite);
      ("unix", unix_suite);
      ("ws", websocket_suite);
      ("origin", origin_suite);
    ]
  in
  match Sys.getenv_opt "ONLY" with
  | None -> List.iter (fun (_, suite) -> suite ~env ()) suites
  | Some suite_name -> (
      match List.assoc_opt suite_name suites with
      | Some suite -> suite ~env ()
      | None -> failwith ("Invalid selection: " ^ suite_name))

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> main ~env ()
