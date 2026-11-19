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

let with_pipe ?max_message_size f =
  let ic, oc = Lwt_io.pipe () in
  Lwt.finalize
    (fun () -> f (Conn.v ?max_message_size ic oc))
    (fun () -> Lwt.join [ Lwt_io.close ic; Lwt_io.close oc ])

let check_too_large len f =
  Lwt.try_bind
    (fun () -> timeout f)
    (fun _ -> Alcotest.failf "a message of %Ld bytes was accepted" len)
    (function
      | Irmin_server.Conn.Message_too_large l ->
          Alcotest.(check int64) "length" len l;
          Lwt.return_unit
      | exn -> Lwt.fail exn)

(* The length of a message is chosen by the peer: it must be checked before
   anything is allocated. *)
let message_size () =
  Lwt_eio.run_lwt @@ fun () ->
  let open Lwt.Syntax in
  let too_large ?max_message_size len =
    with_pipe ?max_message_size @@ fun conn ->
    let* () = Lwt_io.BE.write_int64 conn.oc len in
    let* () = Lwt_io.flush conn.oc in
    check_too_large len (fun () -> Conn.read conn Irmin.Type.string)
  in
  let* () = too_large 0x1_0000_0000L in
  let* () = too_large Int64.max_int in
  let* () = too_large ~max_message_size:16 17L in
  (* The length is unsigned: a negative one is longer than any limit *)
  let* () = too_large (-1L) in
  (* The limit is the same in both directions *)
  let* () =
    with_pipe ~max_message_size:16 @@ fun conn ->
    check_too_large 17L (fun () ->
        Conn.write conn Irmin.Type.string (String.make 17 'x'))
  in
  (* Nothing is written of a message which is too long, not even its header *)
  let* () =
    with_pipe ~max_message_size:16 @@ fun conn ->
    let request command = Conn.Request.v_header ~command in
    let* () =
      check_too_large 17L (fun () ->
          Conn.Request.write conn (request "long") Irmin.Type.string
            (String.make 17 'x'))
    in
    let* () = Conn.Request.write conn (request "short") Irmin.Type.string "x" in
    let+ { command } = timeout (fun () -> Conn.Request.read_header conn) in
    Alcotest.(check string) "command" "short" command
  in
  (* The same for a response, like the notification of a watch *)
  let* () =
    with_pipe ~max_message_size:16 @@ fun conn ->
    let header = Conn.Response.v_header ~status:0 in
    let* () =
      check_too_large 17L (fun () ->
          Conn.Response.write conn header Irmin.Type.string (String.make 17 'x'))
    in
    let* () = Conn.Response.write conn header Irmin.Type.string "x" in
    let* (_ : Conn.Response.header) =
      timeout (fun () -> Conn.Response.read_header conn)
    in
    let+ msg = timeout (fun () -> Conn.read conn Irmin.Type.string) in
    Alcotest.(check (result string error)) "response" (Ok "x") msg
  in
  (* Long messages are read by pieces *)
  with_pipe @@ fun conn ->
  let msg = String.init 200_000 (fun i -> Char.chr (i mod 256)) in
  let+ (), msg' =
    timeout @@ fun () ->
    Lwt.both
      (Conn.write conn Irmin.Type.string msg)
      (Conn.read conn Irmin.Type.string)
  in
  Alcotest.(check (result string error)) "long message" (Ok msg) msg'

(* Nothing longer than the handshake of the store is read before the handshake
   is checked, on both sides. *)
let handshake_size () =
  Lwt_eio.run_lwt @@ fun () ->
  let open Lwt.Syntax in
  let long =
    Int64.of_int
      (String.length (Conn.Handshake.V1.fingerprint (module Store)) + 1)
  in
  let* () =
    with_pipe @@ fun conn ->
    let* () = Lwt_io.BE.write_int64 conn.oc long in
    let* () = Lwt_io.flush conn.oc in
    check_too_large long (fun () -> Conn.Handshake.V1.check (module Store) conn)
  in
  with_pipe @@ fun conn ->
  (* The answer of the server is read first *)
  let* () = Lwt_io.BE.write_int64 conn.oc long in
  Lwt.try_bind
    (fun () -> timeout (fun () -> Conn.Handshake.V1.send (module Store) conn))
    (fun _ -> Alcotest.fail "a long handshake was read")
    (function
      | Error.Error msg ->
          Alcotest.(check string) "error" "invalid handshake" msg;
          Lwt.return_unit
      | exn -> Lwt.fail exn)

(* The server closes the connection of a client announcing a message which is
   too long, and goes on serving the other clients. *)
let oversized_request ~uri client () =
  ( Lwt_eio.run_lwt @@ fun () ->
    let open Lwt.Syntax in
    let* conn = raw_connection uri in
    let ic = conn.ic and oc = conn.oc in
    let* () = send_command conn "ping" in
    let* () = Lwt_io.BE.write_int64 oc 0x1_0000_0000L in
    let* () = Lwt_io.flush oc in
    let* header = timeout (fun () -> Conn.Response.read_header conn) in
    let* err = timeout (fun () -> Conn.Response.get_error conn header) in
    Alcotest.(check (option string))
      "error" (Some "ERROR message too large") err;
    let* closed =
      Lwt.try_bind
        (fun () -> timeout (fun () -> Lwt_io.read_char ic))
        (fun _ -> Lwt.return_false)
        (function End_of_file -> Lwt.return_true | exn -> Lwt.fail exn)
    in
    Alcotest.(check bool) "connection closed" true closed;
    Lwt_io.close ic );
  ping client ()

let too_large = function
  | Irmin_server.Conn.Message_too_large _ -> true
  | _ -> false

let closed = function Irmin.Closed -> true | _ -> false

let check_ping msg repo =
  Alcotest.(check (result unit error)) msg (Ok ()) (Client.ping repo)

let small_size = 4096

(* [f uri] with a server at [uri] whose messages have a small limit. Each
   test has its own socket: the server of another test may still be stopping. *)
let with_small_server ~clock name f =
  Eio.Switch.run @@ fun sw ->
  let uri = unix_uri name in
  start_server ~sw ~clock ~max_message_size:small_size "Sizes" uri;
  f uri

let client_message_size ~clock () =
  with_small_server ~clock "client-size.sock" @@ fun small_uri ->
  let repo = Client.Repo.v (Irmin_client_unix.config small_uri) in
  let t = Client.of_branch repo "sizes" in
  let value = String.make 1000 'x' in
  let set t step = Client.set_exn t ~info:Client.Info.none [ step ] value in
  set t "a";
  let small =
    Client.Repo.v (Irmin_client_unix.config ~max_message_size:256 small_uri)
  in
  let t' = Client.of_branch small "sizes" in
  (* Nothing is sent of a request which is too long: the client goes on *)
  check_raises "a long request was sent" too_large (fun () -> set t' "b");
  check_ping "ping after a long request" small;
  (* A client is closed when it cannot read an answer: what follows on the
     connection would not make sense. *)
  check_raises "a long answer was accepted" too_large (fun () ->
      Client.get t' [ "a" ]);
  check_raises "the client is still open" closed (fun () -> Client.ping small);
  (* The server answers with an error instead of a response which is too
     long for it, and the connection goes on. *)
  List.iter (set t) [ "1"; "2"; "3"; "4"; "5"; "6" ];
  check_raises "a long response was sent"
    (function Error.Error _ -> true | _ -> false)
    (fun () -> Client.export repo);
  check_ping "ping after a long response" repo

(* The server closes the connection when a request is too long for it. The
   client cannot go on with this connection: it is closed. *)
let server_message_size ~clock () =
  with_small_server ~clock "server-size.sock" @@ fun small_uri ->
  let repo = Client.Repo.v (Irmin_client_unix.config small_uri) in
  let t = Client.of_branch repo "sizes" in
  let any _ = true in
  check_raises "a long request was accepted" any (fun () ->
      Client.set_exn t ~info:Client.Info.none [ "a" ]
        (String.make 1_000_000 'x'));
  check_raises "the connection is still open" any (fun () -> Client.ping repo);
  check_raises "the client is still open" closed (fun () -> Client.ping repo)

(* A limit which is too short for the handshake, or longer than a string, is
   refused. *)
let invalid_message_size () =
  let invalid = function Invalid_argument _ -> true | _ -> false in
  let small_uri = unix_uri "invalid-size.sock" in
  List.iter
    (fun max_message_size ->
      check_raises "client" invalid (fun () ->
          Client.Repo.v (Irmin_client_unix.config ~max_message_size small_uri));
      check_raises "server" invalid (fun () ->
          Lwt_eio.run_lwt @@ fun () ->
          Server.v ~max_message_size ~uri:small_uri (mem_config "Sizes")))
    [ 10; Sys.max_string_length + 1 ];
  check_raises "connection" invalid (fun () ->
      Conn.v ~max_message_size:(-1) Lwt_io.stdin Lwt_io.stdout)

(* [with_watchdog msg f] exits if [f] does not return: a scheduler which is
   kept busy cannot report the failure itself. It waits longer than the
   timeouts of the tests, which report the other failures. *)
let with_watchdog msg f =
  let finished = Atomic.make false in
  let deadline = Unix.gettimeofday () +. (2. *. timeout_delay) in
  let rec wait () =
    if Atomic.get finished then ()
    else if Unix.gettimeofday () > deadline then (
      prerr_endline msg;
      Unix._exit 1)
    else (
      Unix.sleepf 0.1;
      wait ())
  in
  let watchdog = Domain.spawn wait in
  Fun.protect f ~finally:(fun () ->
      Atomic.set finished true;
      Domain.join watchdog)

(* A watch must stop when its server goes away. It was looping for ever,
   without yielding to the scheduler. The server here accepts the watch, then
   closes the connection. Removing the watch afterwards must not fail. *)
let watch_lost_server () =
  with_watchdog "The watch keeps the scheduler busy without its server"
  @@ fun () ->
  with_watch_server "watch.sock" Conn.ok @@ fun uri ->
  let repo = Client.Repo.v (Irmin_client_unix.config uri) in
  let watch = Client.Branch.watch repo "main" ignore in
  Eio_unix.sleep 0.5;
  Client.unwatch watch;
  Client.Repo.close repo

let notify conn write =
  let open Lwt.Syntax in
  let* () =
    Conn.Response.write_header conn (Conn.Response.v_header ~status:0)
  in
  write conn

let branches repo = Client.Backend.Repo.branch_t repo
let commit name = Store.Hash.hash (fun f -> f name)

(* The watches return their removal. *)
let watch_branches repo f =
  let watch =
    Client.Backend.Branch.watch (branches repo) (fun key diff -> f (key, diff))
  in
  fun () -> Client.Backend.Branch.unwatch (branches repo) watch

let watch_main repo f =
  let watch = Client.Backend.Branch.watch_key (branches repo) "main" f in
  fun () -> Client.Backend.Branch.unwatch (branches repo) watch

(* A notification which cannot be decoded is skipped: the watch goes on.
   [skipped] is sent followed by more bytes, then [expected] is sent. The
   strings are sent as they are, without their length. *)
let watch_invalid_notification ~watch diff_t ~skipped ~expected () =
  with_watchdog "The watch keeps the scheduler busy" @@ fun () ->
  let open Lwt.Syntax in
  let encode = Irmin.Type.(unstage (to_bin_string diff_t)) in
  let send_raw s conn = Conn.write conn Irmin.Type.string s in
  let answer conn =
    let* () = Conn.ok conn in
    let* () = notify conn (send_raw "invalid") in
    let* () = notify conn (send_raw (encode skipped ^ "more")) in
    notify conn (fun conn -> Conn.write conn diff_t expected)
  in
  with_watch_server "invalid.sock" answer @@ fun uri ->
  let repo = Client.Repo.v (Irmin_client_unix.config uri) in
  let received, notified = Eio.Promise.create () in
  let unwatch = watch repo (Eio.Promise.resolve notified) in
  let diff =
    Eio.Fiber.first
      (fun () -> Eio.Promise.await received)
      (fun () ->
        Eio_unix.sleep timeout_delay;
        Alcotest.fail "The watch stops on an invalid notification")
  in
  let diff_testable =
    Alcotest.testable (Irmin.Type.pp diff_t) Irmin.Type.(unstage (equal diff_t))
  in
  Alcotest.check diff_testable "notification" expected diff;
  unwatch ();
  Client.Repo.close repo

(* The connection of a watch which cannot be set is closed. *)
let watch_refused () =
  with_watchdog "The watch keeps the scheduler busy" @@ fun () ->
  let closed, set_closed = Lwt.wait () in
  let answer (conn : Conn.t) =
    let open Lwt.Syntax in
    let* () = Conn.err conn "refused" in
    Lwt.catch
      (fun () ->
        let+ (_ : char) = Lwt_io.read_char conn.ic in
        Lwt.wakeup_later set_closed false)
      (function
        | End_of_file ->
            Lwt.wakeup_later set_closed true;
            Lwt.return_unit
        | exn -> Lwt.reraise exn)
  in
  with_watch_server "refused.sock" answer @@ fun uri ->
  let repo = Client.Repo.v (Irmin_client_unix.config uri) in
  check_raises "The watch is set"
    (function Error.Error _ -> true | _ -> false)
    (fun () -> Client.Backend.Branch.watch (branches repo) (fun _ _ -> ()));
  let closed = Lwt_eio.run_lwt @@ fun () -> timeout (fun () -> closed) in
  Alcotest.(check bool) "connection closed" true closed;
  Client.Repo.close repo

(* The answers of a server which refuses the handshake of a client, with the
   error they give to the client. *)
let refusals =
  let open Lwt.Syntax in
  let answer oc length s =
    let* () = Lwt_io.BE.write_int64 oc length in
    let* () = Lwt_io.write oc s in
    Lwt_io.flush oc
  in
  let error msg = function Error.Error m -> m = msg | _ -> false in
  [
    ( "end of connection",
      (fun fd _ ->
        (* The server still sees the client closing its side *)
        Lwt_unix.shutdown fd Unix.SHUTDOWN_SEND;
        Lwt.return_unit),
      error "invalid handshake" );
    ( "other fingerprint",
      (fun _ oc ->
        let s = "V1 another store" in
        answer oc (Int64.of_int (String.length s)) s),
      error "invalid handshake" );
    ( "oversized answer",
      (fun _ oc -> answer oc Int64.max_int ""),
      error "invalid handshake" );
    ( "no answer",
      (fun _ _ -> Lwt.return_unit),
      error "unable to connect to server" );
  ]

(* A client which cannot connect closes its connection. *)
let failed_handshake address refuse expected () =
  let sockaddr, uri = address () in
  with_handshake_server sockaddr refuse @@ fun closed ->
  check_raises "handshake accepted" expected (fun () ->
      Client.Repo.v (Irmin_client_unix.config uri));
  let closed = Lwt_eio.run_lwt @@ fun () -> timeout (fun () -> closed) in
  Alcotest.(check bool) "connection closed" true closed

let failed_handshakes =
  let unix () =
    let uri = unix_uri "handshake.sock" in
    (Unix.ADDR_UNIX (Uri.path uri), uri)
  and tcp () =
    let port = free_port () in
    ( Unix.ADDR_INET (Unix.inet_addr_loopback, port),
      Uri.of_string (Fmt.str "tcp://127.0.0.1:%d" port) )
  in
  List.concat_map
    (fun (name, refuse, expected) ->
      [
        ( "failed handshake, " ^ name,
          `Quick,
          failed_handshake unix refuse expected );
        ( "failed handshake over TCP, " ^ name,
          `Quick,
          failed_handshake tcp refuse expected );
      ])
    refusals

let misc ~uri ~clock client =
  let branch_diff_t =
    Irmin.Type.pair Store.Branch.t (Irmin.Diff.t Store.commit_key_t)
  in
  [
    ("ping", `Quick, ping client);
    ("message size", `Quick, message_size);
    ("handshake size", `Quick, handshake_size);
    ("oversized request", `Quick, oversized_request ~uri client);
    ("client message size", `Quick, client_message_size ~clock);
    ("server message size", `Quick, server_message_size ~clock);
    ("invalid message size", `Quick, invalid_message_size);
    ("watch and lost server", `Quick, watch_lost_server);
    ( "watch and invalid notification",
      `Quick,
      watch_invalid_notification ~watch:watch_branches branch_diff_t
        ~skipped:("too long", `Removed (commit "a"))
        ~expected:("main", `Removed (commit "a")) );
    ( "watch_key and invalid notification",
      `Quick,
      watch_invalid_notification ~watch:watch_main
        (Irmin.Diff.t Store.commit_key_t)
        ~skipped:(`Removed (commit "a"))
        ~expected:(`Added (commit "b")) );
    ("watch refused", `Quick, watch_refused);
  ]
  @ failed_handshakes

let misc ~uri ~clock client = [ ("misc", misc ~uri ~clock client) ]

(* Run [f uri] against a websocket server [uri] handling its connections
   with [handler]. *)
let with_websocket_server ~clock handler f =
  let port = free_port () in
  let uri = websocket_uri port in
  let stop, set_stop = Lwt.wait () in
  Eio.Switch.run @@ fun sw ->
  Eio.Switch.on_release sw (fun () -> Lwt.wakeup_later set_stop ());
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Lwt_eio.run_lwt (fun () ->
          Websocket_lwt_unix.establish_standard_server ~stop
            ~mode:(`TCP (`Port port))
            ~check_request:(fun _ -> true)
            ~on_exn:(fun exn ->
              Logs.debug (fun l -> l "Server: %a" Fmt.exn exn))
            handler);
      `Stop_daemon);
  wait_for_server ~clock uri;
  f uri;
  (* What is still running for the connection must end well *)
  Eio.Time.sleep clock 0.2

module Ws = Websocket_lwt_unix.Connected_client

let binary_frame content = Websocket.Frame.create ~opcode:Binary ~content ()

(* The handshake of a client is sent back to it. *)
let echo_handshake client =
  let open Lwt.Syntax in
  let* handshake = Ws.recv client in
  Ws.send client (binary_frame handshake.content)

(* The bytes written by [f] on a connection. *)
let written f =
  let open Lwt.Syntax in
  let ic, oc = Lwt_io.pipe () in
  let* () = f (Conn.v ic oc) in
  let* () = Lwt_io.close oc in
  let* bytes = Lwt_io.read ic in
  let+ () = Lwt_io.close ic in
  bytes

(* A watch over a websocket must stop when its server goes away. The server
   here accepts the watch, then closes the connection. *)
let watch_lost_websocket_server ~clock () =
  with_watchdog "The watch keeps the scheduler busy without its server"
  @@ fun () ->
  let ok = Lwt_eio.run_lwt (fun () -> written Conn.ok) in
  with_websocket_server ~clock (fun client ->
      let open Lwt.Syntax in
      let* () = echo_handshake client in
      (* Only the connection of the watch sends a request *)
      let* (_ : Websocket.Frame.t) = Ws.recv client in
      Ws.send client (binary_frame ok))
  @@ fun uri ->
  let repo = Client.Repo.v (Irmin_client_unix.config uri) in
  let watch = Client.Branch.watch repo "main" ignore in
  Eio_unix.sleep 0.5;
  Client.unwatch watch;
  Client.Repo.close repo

(* A frame which is not valid stops the connection of a client, not its
   program. *)
let websocket_bad_server ~clock () =
  (* A control frame cannot be this long *)
  let frame =
    Websocket.Frame.create ~opcode:Ping ~content:(String.make 200 'x') ()
  in
  with_websocket_server ~clock (fun client ->
      let open Lwt.Syntax in
      let* _ = Ws.recv client in
      Ws.send client frame)
  @@ fun uri ->
  check_raises "connected"
    (function Error.Error _ -> true | _ -> false)
    (fun () -> Client.Repo.v (Irmin_client_unix.config uri))

(* The socket of a client is closed with the client, even after a close
   frame of its server. *)
let websocket_closing_server ~clock () =
  let released = Lwt_condition.create () in
  let release () =
    Lwt_eio.run_lwt (fun () ->
        Lwt_condition.broadcast released ();
        Lwt.return_unit)
  in
  with_websocket_server ~clock (fun client ->
      let open Lwt.Syntax in
      let* () = echo_handshake client in
      let* () = Ws.send client (Websocket.Frame.close 1000) in
      (* The connection stays open: only the client can close its socket *)
      Lwt_condition.wait released)
  @@ fun uri ->
  let connect () =
    Client.Repo.close (Client.Repo.v (Irmin_client_unix.config uri))
  in
  (* The file descriptors are counted while the server keeps its side of
     the connection open. The first connection opens what is kept for the
     next ones. *)
  connect ();
  Eio.Time.sleep clock 0.2;
  let before = open_fds () in
  release ();
  connect ();
  let closed = eventually ~clock (fun () -> open_fds () = before) in
  let after = open_fds () in
  release ();
  if not closed then
    Alcotest.failf "File descriptors: %a before, %a after"
      Fmt.(option int)
      before
      Fmt.(option int)
      after

let websocket_misc ~clock =
  [
    ( "misc",
      [
        ("watch and lost server", `Quick, watch_lost_websocket_server ~clock);
        ("bad server", `Quick, websocket_bad_server ~clock);
        ("closing server", `Quick, websocket_closing_server ~clock);
      ] );
  ]

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
    ~misc:(misc ~uri ~clock client)
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
