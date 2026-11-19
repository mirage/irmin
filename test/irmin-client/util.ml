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

(* The address of [uri], on the loopback interface for a network one. *)
let sockaddr uri =
  match (Uri.scheme uri, Uri.port uri) with
  | Some "unix", _ -> Unix.ADDR_UNIX (Uri.path uri)
  | _, Some port -> Unix.ADDR_INET (Unix.inet_addr_loopback, port)
  | _, None -> Fmt.failwith "No port in %a" Uri.pp uri

(* Wait until a server accepts the connections. *)
let wait_for_server ~clock uri =
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
      if tries = 0 then Fmt.failwith "No server on %a" Uri.pp uri
      else (
        Eio.Time.sleep clock 0.05;
        wait (tries - 1))
  in
  wait 400

let mem_config kind =
  let spec = Irmin.Backend.Conf.Spec.v kind in
  let key = Irmin.Backend.Conf.root spec in
  Irmin.Backend.Conf.singleton spec key kind

(* Run a server for as long as [sw] is on. *)
let start_server ~sw ~clock ?max_message_size kind uri =
  let stop, set_stop = Lwt.wait () in
  Eio.Switch.on_release sw (fun () -> Lwt.wakeup_later set_stop ());
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let conf = mem_config kind in
      Lwt_eio.run_lwt (fun () ->
          Server.v ?max_message_size ~uri conf >>= Server.serve ~stop);
      `Stop_daemon);
  wait_for_server ~clock uri

let unix_uri name =
  Uri.of_string ("unix://" ^ Filename.concat (Unix.getcwd ()) name)

let loopback_uri scheme port =
  Uri.of_string (Fmt.str "%s://127.0.0.1:%d" scheme port)

let websocket_uri port = loopback_uri "ws" port

let run_server ~sw ~clock s =
  let local scheme = loopback_uri scheme (free_port ()) in
  let kind, uri =
    match s with
    | `Websocket -> ("Websocket", local "ws")
    | `Unix_domain -> ("Unix_domain", unix_uri "test.sock")
    | `Tcp -> ("Tcp", local "tcp")
  in
  start_server ~sw ~clock kind uri;
  (kind, uri)

(* A test waiting for a peer must fail rather than hang. *)
let timeout_delay = 20.
let timeout f = Lwt_unix.with_timeout timeout_delay f

(* [f ()] must raise an exception accepted by [expected], before the
   timeout. The timeout is not given to [expected]: a test which does not
   end fails. *)
let check_raises msg expected f =
  let exception No_answer in
  (* [f] may raise too when it is cancelled: [Fiber.first] then gives both
     exceptions together. *)
  let timed_out = ref false in
  let no_answer () =
    Eio_unix.sleep timeout_delay;
    timed_out := true;
    raise No_answer
  in
  match Eio.Fiber.first no_answer f with
  | _ -> Alcotest.fail msg
  | exception _ when !timed_out -> Alcotest.failf "%s: no answer" msg
  | exception exn when expected exn -> ()

(* Wait until [f ()] holds, for a few seconds. *)
let rec eventually ~clock ?(tries = 50) f =
  f ()
  || tries > 0
     &&
     (Eio.Time.sleep clock 0.1;
      eventually ~clock ~tries:(tries - 1) f)

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

(* Ask the websocket server on [port] to upgrade a connection. *)
let upgrade ?origin ~port (ic, oc) =
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
  timeout (fun () -> Lwt_io.read_line ic) >|= fun status ->
  String.length status >= 12 && String.sub status 9 3 = "101"

let with_connection ~port f =
  Lwt_eio.run_lwt @@ fun () ->
  Lwt_io.with_connection Unix.(ADDR_INET (inet_addr_loopback, port)) f

(* The commands of the server, for their names and request types *)
module Command =
  Irmin_server.Command.Make (Lwt_io_channels) (Irmin_server.Conn.Codec.Bin)
    (Store)

module Watch = Command.Commands.Branch.Watch
module Watch_key = Command.Commands.Branch.Watch_key

(* Read the request of [Branch.watch] or [Branch.watch_key]: it must have no
   [init]. *)
let read_watch_request conn command =
  if command = Watch.name then Conn.read conn Watch.req_t >|= ( = ) (Ok None)
  else if command = Watch_key.name then
    Conn.read conn Watch_key.req_t
    >|= Result.fold ~ok:(fun (init, _) -> init = None) ~error:(fun _ -> false)
  else Lwt.return_false

(* Run [f] with a server on the Unix socket [name], in the event loop of the
   tests. The server checks the handshake of each connection, and gives a
   watch request without [init] to [answer]. It refuses any other request. *)
let with_watch_server name answer f =
  let uri = unix_uri name in
  let path = Uri.path uri in
  if Sys.file_exists path then Unix.unlink path;
  let handler _ (ic, oc) =
    let conn = Conn.v ic oc in
    Lwt.catch
      (fun () ->
        Conn.Handshake.V1.check (module Store) conn >>= function
        | false -> Lwt.return_unit
        | true -> (
            Conn.Request.read_header conn >>= fun { command } ->
            read_watch_request conn command >>= function
            | true -> answer conn
            | false -> Conn.err conn "unexpected request"))
      (function
        | End_of_file -> Lwt.return_unit
        | exn ->
            (* The test fails on its own, without the answer *)
            Logs.err (fun l -> l "Watch server: %a" Fmt.exn exn);
            Lwt.return_unit)
  in
  let server =
    Lwt_eio.run_lwt @@ fun () ->
    Lwt_io.establish_server_with_client_address (Unix.ADDR_UNIX path) handler
  in
  Fun.protect
    (fun () -> f uri)
    ~finally:(fun () ->
      Lwt_eio.run_lwt @@ fun () -> Lwt_io.shutdown_server server)

(* Run [f] with a server on [sockaddr], in the test process. The server reads
   the handshake of a client, then gives the socket and its output channel to
   [refuse], which must not accept it. [f] gets a promise telling whether the
   client has then closed its side of the connection. Only the first
   connection is looked at. *)
let with_handshake_server sockaddr refuse f =
  let closed, set_closed = Lwt.wait () in
  let handler _ fd =
    let set_closed result =
      if Lwt.is_sleeping closed then Lwt.wakeup_later_result set_closed result
    in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd
    and oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    Lwt.try_bind
      (fun () ->
        Lwt_io.BE.read_int64 ic >>= fun length ->
        let handshake = Bytes.create (Int64.to_int length) in
        Lwt_io.read_into_exactly ic handshake 0 (Bytes.length handshake)
        >>= fun () ->
        refuse fd oc >>= fun () ->
        Lwt.catch
          (fun () -> Lwt_io.read_char ic >|= fun (_ : char) -> false)
          (function End_of_file -> Lwt.return_true | exn -> Lwt.reraise exn))
      (fun closed ->
        set_closed (Ok closed);
        Lwt.return_unit)
      (fun exn ->
        set_closed (Error exn);
        Lwt.return_unit)
  in
  (match sockaddr with
  | Unix.ADDR_UNIX path when Sys.file_exists path -> Unix.unlink path
  | _ -> ());
  let server =
    Lwt_eio.run_lwt @@ fun () ->
    Lwt_io.establish_server_with_client_socket sockaddr handler
  in
  Fun.protect
    (fun () -> f closed)
    ~finally:(fun () ->
      Lwt_eio.run_lwt @@ fun () -> Lwt_io.shutdown_server server)
