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

open Lwt.Infix

let () = Mirage_crypto_rng_unix.use_default ()

module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_unix.Make (Store)
module Server = Irmin_server_unix.Make (Store)

let test name f client _switch () =
  Logs.debug (fun l -> l "Running: %s" name);
  f client

let unix_uri name =
  Uri.of_string ("unix://" ^ Filename.concat (Unix.getcwd ()) name)

(* The processes forked by the tests which have not been reaped yet. *)
let children = Hashtbl.create 8
let main_process = Unix.getpid ()

(* SIGTERM lets a server remove its Unix socket. *)
let terminate pid = Unix.kill pid Sys.sigterm

(* Reap [pid] if it has ended. *)
let reaped pid =
  match Unix.waitpid [ Unix.WNOHANG ] pid with
  | 0, _ -> false
  | _ -> true
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> true

(* The children are stopped when the tests exit, and given a moment to
   remove their Unix sockets: a new run must not lose its own sockets. A
   server calls [exit] on SIGTERM, and so runs the [at_exit] functions it
   inherits: this one must do nothing in a child. A child which is already
   gone is skipped. *)
let () =
  at_exit @@ fun () ->
  if Unix.getpid () = main_process then (
    Hashtbl.iter
      (fun pid () ->
        try terminate pid with Unix.Unix_error (Unix.ESRCH, _, _) -> ())
      children;
    let rec wait tries =
      Hashtbl.filter_map_inplace
        (fun pid () -> if reaped pid then None else Some ())
        children;
      if Hashtbl.length children > 0 && tries > 0 then (
        Unix.sleepf 0.05;
        wait (tries - 1))
    in
    wait 40)

(* Run [f] in a new process. The child does not come back to the code of
   the tests: it ends with [_exit] when [f] returns. The buffers are flushed
   before the fork, so that the child does not print them again. [fork] must
   be called outside [Lwt_main.run]: the child cannot run its own loop
   inside the loop of its parent. *)
let fork f =
  let flush () =
    Format.pp_print_flush Format.std_formatter ();
    Format.pp_print_flush Format.err_formatter ();
    flush_all ()
  in
  flush ();
  match Lwt_unix.fork () with
  | 0 ->
      let code =
        match f () with
        | () -> 0
        | exception exn ->
            Fmt.epr "Child process: %a@." Fmt.exn exn;
            1
      in
      flush ();
      Unix._exit code
  | pid ->
      Hashtbl.replace children pid ();
      pid

(* Reap a child. Its process id may then be reused: it must not be stopped
   at exit any more. *)
let wait_child pid =
  Lwt_unix.waitpid [] pid >|= fun (_, status) ->
  Hashtbl.remove children pid;
  status

let exited pid =
  reaped pid
  &&
  (Hashtbl.remove children pid;
   true)

(* The address of [uri], on the loopback interface for a network one. *)
let sockaddr uri =
  match (Uri.scheme uri, Uri.port uri) with
  | Some "unix", _ -> Unix.ADDR_UNIX (Uri.path uri)
  | _, Some port -> Unix.ADDR_INET (Unix.inet_addr_loopback, port)
  | _, None -> Fmt.failwith "No port in %a" Uri.pp uri

(* Wait until a server accepts the connections, or [server] exits. *)
let wait_for_server ?server uri =
  let addr = sockaddr uri in
  let connect () =
    let socket =
      Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0
    in
    Fun.protect
      ~finally:(fun () -> Unix.close socket)
      (fun () ->
        match Unix.connect socket addr with
        | () ->
            (* The port of a server which does not listen yet can be given
               to the probe, which is then connected to itself. *)
            Unix.getsockname socket <> Unix.getpeername socket
        | exception Unix.Unix_error ((Unix.ECONNREFUSED | Unix.ENOENT), _, _) ->
            false)
  in
  let rec wait tries =
    if not (connect ()) then
      if Option.fold ~none:false ~some:exited server then
        Fmt.failwith "The server on %a has exited" Uri.pp uri
      else if tries = 0 then Fmt.failwith "No server on %a" Uri.pp uri
      else (
        Unix.sleepf 0.05;
        wait (tries - 1))
  in
  wait 400

(* Fork a server and wait until it is listening. The processes are forked
   before the first client is created: a child must not inherit the
   connections of the tests. *)
let fork_server uri =
  (* The server of a run which was killed may still hold the Unix socket: it
     must not be taken for this one. *)
  (match Uri.scheme uri with
  | Some "unix" -> (
      try Unix.unlink (Uri.path uri)
      with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
  | _ -> ());
  let server =
    fork @@ fun () ->
    let () = Irmin.Backend.Watch.set_listen_dir_hook Irmin_watcher.hook in
    let conf = Irmin_mem.config () in
    Lwt_main.run (Server.v ~uri conf >>= Server.serve)
  in
  wait_for_server ~server uri;
  server

(* A port which is free for now. *)
let free_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname socket with
      | Unix.ADDR_INET (_, port) -> port
      | Unix.ADDR_UNIX _ -> failwith "free_port: not an internet address")

let loopback_uri scheme port =
  Uri.of_string (Fmt.str "%s://127.0.0.1:%d" scheme port)

let run_server s =
  let local scheme = loopback_uri scheme (free_port ()) in
  let kind, uri =
    match s with
    | `Websocket -> ("Websocket", local "ws")
    | `Unix_domain -> ("Unix_domain", unix_uri "test.sock")
    | `Tcp -> ("Tcp", local "tcp")
  in
  (kind, fork_server uri, uri)

let websocket_uri port = loopback_uri "ws" port

(* A test waiting for a peer must fail rather than hang. *)
let timeout f = Lwt_unix.with_timeout 20. f

(* [f] must fail with an exception accepted by [expected]. The timeout is
   not given to [expected]: a test which does not end fails. *)
let fails msg expected f =
  Lwt.catch
    (fun () ->
      timeout @@ fun () ->
      Lwt.try_bind f
        (fun _ -> Alcotest.fail msg)
        (fun exn -> if expected exn then Lwt.return_unit else Lwt.fail exn))
    (function
      | Lwt_unix.Timeout -> Alcotest.failf "%s: no answer" msg
      | exn -> Lwt.fail exn)

(* Wait until [f ()] holds, for a few seconds. *)
let rec eventually ?(tries = 50) f =
  if f () then Lwt.return_true
  else if tries = 0 then Lwt.return_false
  else Lwt_unix.sleep 0.1 >>= fun () -> eventually ~tries:(tries - 1) f

(* Connections over the channels of [Lwt_io]. *)
module Lwt_io_channels = struct
  type flow = unit
  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel

  exception Timeout = Lwt_unix.Timeout

  let is_closed (ic : ic) = Lwt_io.is_closed ic
  let write_int64_be = Lwt_io.BE.write_int64
  let read_int64_be = Lwt_io.BE.read_int64
  let flush = Lwt_io.flush
  let write = Lwt_io.write
  let read_into_exactly = Lwt_io.read_into_exactly
  let write_char = Lwt_io.write_char
  let read_char = Lwt_io.read_char
  let with_timeout = Lwt_unix.with_timeout
  let time = Unix.time
end

module Conn =
  Irmin_server.Conn.Make (Lwt_io_channels) (Irmin_server.Conn.Codec.Bin)

(* A client speaking the protocol by hand with the server on [uri]. *)
let raw_connection uri =
  Lwt_io.open_connection (sockaddr uri) >>= fun (ic, oc) ->
  let conn = Conn.v ic oc in
  Conn.Handshake.V1.send (module Store) conn >|= fun ok ->
  Alcotest.(check bool) "handshake" true ok;
  conn

let send_command conn command =
  Conn.Request.write_header conn (Conn.Request.v_header ~command)

(* Ask the websocket server on [port] to upgrade a connection, and read the
   whole response. *)
let upgrade ?origin port (ic, oc) =
  let headers =
    [
      Fmt.str "Host: 127.0.0.1:%d" port;
      "Upgrade: websocket";
      "Connection: Upgrade";
      "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==";
      "Sec-WebSocket-Version: 13";
    ]
    @ match origin with None -> [] | Some o -> [ "Origin: " ^ o ]
  in
  let request =
    String.concat "\r\n" (("GET / HTTP/1.1" :: headers) @ [ ""; "" ])
  in
  Lwt_io.write oc request >>= fun () ->
  Lwt_io.flush oc >>= fun () ->
  let rec headers () =
    Lwt_io.read_line ic >>= function "" -> Lwt.return_unit | _ -> headers ()
  in
  timeout (fun () ->
      Lwt_io.read_line ic >>= fun status ->
      headers () >|= fun () -> status)
  >|= fun status -> String.length status >= 12 && String.sub status 9 3 = "101"

(* The frames of a websocket client, as the library of the server writes and
   reads them. The mask of the client does not matter here. *)
let client_mode = Websocket_lwt_unix.Client (fun n -> String.make n '\000')

(* Write [frames] at once. *)
let write_frames oc frames =
  let buf = Buffer.create 128 in
  List.iter (Websocket_lwt_unix.write_frame_to_buf ~mode:client_mode buf) frames;
  Lwt_io.write oc (Buffer.contents buf) >>= fun () -> Lwt_io.flush oc

let write_frame oc frame = write_frames oc [ frame ]
let close_frame = Websocket.Frame.close 1000

let read_frame (ic, oc) =
  timeout (Websocket_lwt_unix.make_read_frame ~mode:client_mode ic oc)

(* The frames sent by the server, until it closes the connection. *)
let read_frames connection =
  let rec loop frames =
    Lwt.try_bind
      (fun () -> read_frame connection)
      (fun frame -> loop (frame :: frames))
      (function
        | End_of_file -> Lwt.return (List.rev frames) | exn -> Lwt.fail exn)
  in
  loop []

(* The handshake of a client, as the content of a websocket frame. *)
let handshake_frame () =
  let fingerprint = Conn.Handshake.V1.fingerprint (module Store) in
  let buf = Buffer.create 64 in
  Buffer.add_int64_be buf (Int64.of_int (String.length fingerprint));
  Buffer.add_string buf fingerprint;
  Websocket.Frame.create ~opcode:Binary ~content:(Buffer.contents buf) ()

(* Upgrade a connection to the websocket server on [port], start a frame,
   and then reset the connection. *)
let reset_websocket ~port =
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt.finalize
    (fun () ->
      Lwt_unix.connect fd Unix.(ADDR_INET (inet_addr_loopback, port))
      >>= fun () ->
      let ic = Lwt_io.of_fd ~mode:Input fd in
      let oc = Lwt_io.of_fd ~mode:Output fd in
      upgrade port (ic, oc) >>= fun accepted ->
      Alcotest.(check bool) "upgrade" true accepted;
      (* The server then waits for the length of this Binary frame *)
      Lwt_io.write oc "\x82\x7e" >>= fun () ->
      Lwt_io.flush oc >|= fun () ->
      Lwt_unix.setsockopt_optint fd Unix.SO_LINGER (Some 0))
    (fun () -> Lwt_unix.close fd)

(* Send the handshake of a client on a websocket, and check its answer. *)
let check_handshake ((_, oc) as connection) =
  let handshake = handshake_frame () in
  write_frame oc handshake >>= fun () ->
  read_frame connection >|= fun (answer : Websocket.Frame.t) ->
  Alcotest.(check string) "handshake answer" handshake.content answer.content

(* The server has answered a Close frame of the client with the last of
   its [frames]. *)
let check_close frames =
  let rec after_close = function
    | [] -> None
    | (frame : Websocket.Frame.t) :: rest ->
        if frame.opcode = Websocket.Frame.Opcode.Close then Some rest
        else after_close rest
  in
  let opcodes =
    List.map (fun (frame : Websocket.Frame.t) ->
        Websocket.Frame.Opcode.to_string frame.opcode)
  in
  Alcotest.(check (option (list string)))
    "frames after Close" (Some [])
    (Option.map opcodes (after_close frames))

let with_local_connection port f =
  Lwt_io.with_connection Unix.(ADDR_INET (inet_addr_loopback, port)) f

let suite client all =
  List.map
    (fun (name, speed, f) ->
      Alcotest_lwt.test_case name speed (test name f client))
    all
