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

(* A frame which is not valid closes its connection, not the server. *)
let websocket_bad_frame ~clock () =
  let port = free_port () in
  Eio.Switch.run @@ fun sw ->
  start_server ~sw ~clock "Frames" (websocket_uri port);
  ( with_connection ~port @@ fun (ic, oc) ->
    let open Lwt.Syntax in
    let* accepted = upgrade ~port (ic, oc) in
    Alcotest.(check bool) "upgrade" true accepted;
    (* A control frame cannot be this long *)
    let* () = Lwt_io.write oc "\x89\xfe\x00\x80" in
    let* () = Lwt_io.flush oc in
    (* The server closes the connection *)
    let+ _ = read_frames (ic, oc) in
    () );
  Alcotest.(check bool)
    "server still running" true
    (with_connection ~port (upgrade ~port))

(* A connection reset by the client does not stop the server either. *)
let websocket_reset ~clock () =
  let port = free_port () in
  Eio.Switch.run @@ fun sw ->
  start_server ~sw ~clock "Reset" (websocket_uri port);
  Lwt_eio.run_lwt (fun () -> reset_websocket ~port);
  Alcotest.(check bool)
    "server still running" true
    (with_connection ~port (upgrade ~port))

(* Send a handshake and then a Close frame on a new websocket, and read the
   frames of the server until it closes the connection. *)
let close_connection ~port =
  with_connection ~port @@ fun (ic, oc) ->
  let open Lwt.Syntax in
  let* accepted = upgrade ~port (ic, oc) in
  Alcotest.(check bool) "upgrade" true accepted;
  (* At once, so that the server reads the Close frame before it answers *)
  let* () = write_frames oc [ handshake_frame (); close_frame ] in
  read_frames (ic, oc)

(* The server sends nothing after the Close frame which answers the one of
   the client, not even the answer to a message received before. *)
let websocket_close ~clock () =
  let port = free_port () in
  Eio.Switch.run @@ fun sw ->
  start_server ~sw ~clock "Close" (websocket_uri port);
  check_close (close_connection ~port)

(* The server keeps nothing of a websocket which is closed, before or after
   the handshake. The file descriptors can only be counted where [/dev/fd]
   gives them. *)
let websocket_closed ~clock () =
  let dir = "/dev/fd" in
  if not (Sys.file_exists dir) then Alcotest.skip ();
  let port = free_port () in
  Eio.Switch.run @@ fun sw ->
  start_server ~sw ~clock "Closed" (websocket_uri port);
  let count () = Array.length (Sys.readdir dir) in
  (* The server has released a connection when it closes it *)
  ignore (close_connection ~port : _ list);
  let before = count () in
  for i = 1 to 10 do
    with_connection ~port @@ fun connection ->
    let open Lwt.Syntax in
    let* accepted = upgrade ~port connection in
    Alcotest.(check bool) "upgrade" true accepted;
    if i mod 2 = 0 then check_handshake connection else Lwt.return_unit
  done;
  Alcotest.(check bool)
    "file descriptors released" true
    (eventually ~clock (fun () -> count () <= before))

let websocket_misc ~clock =
  [
    ( "misc",
      [
        ("websocket bad frame", `Quick, websocket_bad_frame ~clock);
        ("websocket reset", `Quick, websocket_reset ~clock);
        ("websocket close", `Quick, websocket_close ~clock);
        ("websocket closed", `Quick, websocket_closed ~clock);
      ] );
  ]

let misc client = [ ("ping", `Quick, ping client) ]
let misc client = [ ("misc", misc client) ]

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

let main ~env () =
  let suites =
    [ ("tcp", tcp_suite); ("unix", unix_suite); ("ws", websocket_suite) ]
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
