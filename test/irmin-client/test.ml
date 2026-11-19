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

let websocket_pid, websocket_port =
  let _, pid, uri = websocket_server in
  match Uri.port uri with
  | Some port -> (pid, port)
  | None -> Fmt.failwith "No port in %a" Uri.pp uri

let with_websocket_server f = with_local_connection websocket_port f
let upgrade_websocket = upgrade websocket_port

(* A frame which is not valid closes its connection, not the server. *)
let websocket_bad_frame () =
  let* () =
    with_websocket_server @@ fun (ic, oc) ->
    let* accepted = upgrade_websocket (ic, oc) in
    Alcotest.(check bool) "upgrade" true accepted;
    (* A control frame cannot be this long *)
    let* () = Lwt_io.write oc "\x89\xfe\x00\x80" in
    let* () = Lwt_io.flush oc in
    (* The server closes the connection *)
    let+ _ = read_frames (ic, oc) in
    ()
  in
  let+ accepted = with_websocket_server upgrade_websocket in
  Alcotest.(check bool) "server still running" true accepted

(* A connection reset by the client does not stop the server either. *)
let websocket_reset () =
  let* () = reset_websocket ~port:websocket_port in
  let+ accepted = with_websocket_server upgrade_websocket in
  Alcotest.(check bool) "server still running" true accepted

(* Send a handshake and then a Close frame on a new websocket, and read the
   frames of the server until it closes the connection. *)
let close_connection () =
  with_websocket_server @@ fun (ic, oc) ->
  let* accepted = upgrade_websocket (ic, oc) in
  Alcotest.(check bool) "upgrade" true accepted;
  (* At once, so that the server reads the Close frame before it answers *)
  let* () = write_frames oc [ handshake_frame (); close_frame ] in
  read_frames (ic, oc)

(* The server sends nothing after the Close frame which answers the one of
   the client, not even the answer to a message received before. *)
let websocket_close () =
  let+ frames = close_connection () in
  check_close frames

(* The server keeps nothing of a websocket which is closed, before or after
   the handshake. Its file descriptors can only be counted where [/proc]
   gives them. *)
let websocket_closed () =
  let dir = Fmt.str "/proc/%d/fd" websocket_pid in
  if not (Sys.file_exists dir) then Alcotest.skip ();
  let count () = Array.length (Sys.readdir dir) in
  (* The server has released a connection when it closes it *)
  let* (_ : _ list) = close_connection () in
  let before = count () in
  let* () =
    Lwt_list.iter_s
      (fun handshake ->
        with_websocket_server @@ fun connection ->
        let* accepted = upgrade_websocket connection in
        Alcotest.(check bool) "upgrade" true accepted;
        if handshake then check_handshake connection else Lwt.return_unit)
      (List.init 10 (fun i -> i mod 2 = 0))
  in
  let+ released = eventually (fun () -> count () <= before) in
  Alcotest.(check bool) "file descriptors released" true released

let misc =
  [
    ("ping", `Quick, ping);
    ("websocket bad frame", `Quick, websocket_bad_frame);
    ("websocket reset", `Quick, websocket_reset);
    ("websocket close", `Quick, websocket_close);
    ("websocket closed", `Quick, websocket_closed);
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
