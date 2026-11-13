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

open Lwt.Syntax
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
  val uri : Uri.t
  val kind : string
end

module Make (R : R) = struct
  let config = Irmin_client_unix.config R.uri
  let client = Lwt_main.run (Client.Repo.v config)
  let clean ~config:_ = Client.Branch.remove client "main"

  module X = Irmin_mem.KV.Make (Irmin.Contents.String)
  module Store = Irmin_client_unix.Make (X)

  let suite =
    Irmin_test.Suite.create_generic_key ~name:R.kind
      ~store:(module Store)
      ~config ~clean ()
end

let unix_server = run_server `Unix_domain
let tcp_server = run_server `Tcp
let websocket_server = run_server `Websocket

(* A websocket server accepting the pages of one origin. *)
let origin_port = free_port ()
let origin_uri = websocket_uri origin_port

let origin s =
  match Irmin_server_unix.Origin.of_string s with
  | Ok origin -> origin
  | Error (`Msg msg) -> failwith msg

let (_ : int) =
  fork_server
    ~allowed_origins:[ origin "HTTPS://Allowed.Example:443/" ]
    origin_uri

module Unix_socket = Make (struct
  let kind, _, uri = unix_server
end)

module Tcp_socket = Make (struct
  let kind, _, uri = tcp_server
end)

module Websocket = Make (struct
  let kind, _, uri = websocket_server
end)

let _, _, uri = unix_server
let config = Irmin_client_unix.config uri
let client = Lwt_main.run (Client.Repo.v config)
let client () = Client.dup client

let error =
  Alcotest.testable (Fmt.using Error.to_string Fmt.string) (fun a b ->
      Error.to_string a = Error.to_string b)

let ty t =
  Alcotest.testable
    (Fmt.using (Irmin.Type.to_string t) Fmt.string)
    (fun a b -> Irmin.Type.(unstage (equal t)) a b)

let ping () =
  let open Client in
  let* client = client () in
  Logs.debug (fun l -> l "BEFORE PING");
  let+ r = ping client in
  Logs.debug (fun l -> l "AFTER PING");
  Alcotest.(check (result unit error)) "ping" (Ok ()) r

(* A websocket opened by a web page is only accepted from the allowed
   origins. Other clients do not send the header. *)
let websocket_origin () =
  let check_headers msg expected headers =
    let+ accepted =
      with_local_connection origin_port (upgrade ~headers origin_port)
    in
    Alcotest.(check bool) msg expected accepted
  in
  let check ?(header = "Origin") msg expected origins =
    check_headers msg expected
      (List.map (fun origin -> header ^ ": " ^ origin) origins)
  in
  let* () = check "no origin" true [] in
  let* () = check "allowed origin" true [ "https://allowed.example" ] in
  let* () = check "case of the origin" true [ "HTTPS://Allowed.Example" ] in
  let* () = check "other origin" false [ "https://evil.example" ] in
  let* () = check "default port" true [ "https://allowed.example:443" ] in
  let* () = check "other port" false [ "https://allowed.example:8443" ] in
  let* () = check "other scheme" false [ "http://allowed.example" ] in
  let* () = check "not an origin" false [ "null" ] in
  let* () =
    check "prefix of an allowed origin" false [ "https://allowed.example.evil" ]
  in
  let* () =
    check "several origins" false
      [ "https://evil.example"; "https://allowed.example" ]
  in
  let* () =
    check ~header:"Sec-WebSocket-Origin" "origin of the old drafts" false
      [ "https://evil.example" ]
  in
  let* () =
    check ~header:"Sec-WebSocket-Origin" "allowed origin of the old drafts" true
      [ "https://allowed.example" ]
  in
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
    ];
  Lwt.return_unit

let misc =
  [
    ("ping", `Quick, ping);
    ("websocket origin", `Quick, websocket_origin);
    ("origin of_string", `Quick, origin_of_string);
  ]

let misc = [ ("misc", misc) ]

let () =
  let slow = Sys.getenv_opt "SLOW" |> Option.is_some in
  let only = Sys.getenv_opt "ONLY" in
  let tests =
    match only with
    | Some "ws" -> [ (`Quick, Websocket.suite) ]
    | Some "tcp" -> [ (`Quick, Tcp_socket.suite) ]
    | Some "unix" -> [ (`Quick, Unix_socket.suite) ]
    | Some s -> failwith ("Invalid selection: " ^ s)
    | None ->
        [
          (`Quick, Unix_socket.suite);
          (`Quick, Tcp_socket.suite);
          (`Quick, Websocket.suite);
        ]
  in
  Lwt_main.run
    (Irmin_test.Store.run "irmin-server" ~sleep:Lwt_unix.sleep ~slow ~misc tests)
