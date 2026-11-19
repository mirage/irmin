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
let unix_watcher = fork_watcher (unix_uri "watch.sock")
let websocket_watcher = fork_watcher (websocket_uri (free_port ()))

(* The clients of servers which are only started by their tests. *)
let bad_server_connector =
  fork_connector @@ fun uri ->
  Lwt.catch
    (fun () ->
      let* repo = Client.Repo.v (Irmin_client_unix.config uri) in
      Client.Repo.close repo)
    (function
      | Irmin_client_unix.Error.Error _ -> Lwt.return_unit | exn -> Lwt.fail exn)

let closing_server_connector =
  fork_connector @@ fun uri ->
  let connect () =
    let* repo = Client.Repo.v (Irmin_client_unix.config uri) in
    Client.Repo.close repo
  in
  (* The first connection opens what is kept for the next ones *)
  let* () = connect () in
  let* () = Lwt_unix.sleep 0.2 in
  let before = open_fds () in
  let* () = connect () in
  let+ closed = eventually (fun () -> open_fds () = before) in
  if not closed then
    Fmt.failwith "File descriptors: %a before, %a after"
      Fmt.(option int)
      before
      Fmt.(option int)
      (open_fds ())

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

(* A watch must stop when its server goes away. It was looping for ever,
   without yielding to the scheduler: the watcher could not exit. Removing
   the watch afterwards must not fail. *)
let watch_lost_server { server; watcher; ready; server_gone } () =
  let* ready = timeout (fun () -> Lwt_unix.read ready (Bytes.create 1) 0 1) in
  Alcotest.(check int) "watcher ready" 1 ready;
  terminate server;
  let* (_ : Unix.process_status) = timeout (fun () -> wait_child server) in
  let* (_ : int) = Lwt_unix.write server_gone (Bytes.make 1 'g') 0 1 in
  Lwt.catch
    (fun () ->
      timeout @@ fun () ->
      let+ status = wait_child watcher in
      Alcotest.(check bool) "watcher exit" true (status = Unix.WEXITED 0))
    (function
      | Lwt_unix.Timeout ->
          terminate watcher;
          Alcotest.fail "The watch keeps the scheduler busy without its server"
      | exn -> Lwt.reraise exn)

let notify conn write =
  let* () =
    Conn.Response.write_header conn (Conn.Response.v_header ~status:0)
  in
  write conn

let commit name = Store.Hash.hash (fun f -> f name)

(* The watches return their removal. *)
let watch_branches repo f =
  let branches = Client.Backend.Repo.branch_t repo in
  let+ watch =
    Client.Backend.Branch.watch branches (fun key diff -> f (key, diff))
  in
  fun () -> Client.Backend.Branch.unwatch branches watch

let watch_main repo f =
  let branches = Client.Backend.Repo.branch_t repo in
  let+ watch = Client.Backend.Branch.watch_key branches "main" f in
  fun () -> Client.Backend.Branch.unwatch branches watch

(* A notification which cannot be decoded is skipped: the watch goes on.
   [skipped] is sent followed by more bytes, then [expected] is sent. The
   strings are sent as they are, without their length. *)
let watch_invalid_notification ~watch diff_t ~skipped ~expected () =
  let encode = Irmin.Type.(unstage (to_bin_string diff_t)) in
  let send_raw s conn = Conn.write conn Irmin.Type.string s in
  let answer conn =
    let* () = Conn.ok conn in
    let* () = notify conn (send_raw "invalid") in
    let* () = notify conn (send_raw (encode skipped ^ "more")) in
    notify conn (fun conn -> Conn.write conn diff_t expected)
  in
  with_watch_server "invalid.sock" answer @@ fun uri ->
  let* repo = Client.Repo.v (Irmin_client_unix.config uri) in
  let received, notified = Lwt.wait () in
  let* unwatch =
    watch repo (fun diff ->
        Lwt.wakeup_later notified diff;
        Lwt.return_unit)
  in
  let* diff = timeout (fun () -> received) in
  let diff_testable =
    Alcotest.testable (Irmin.Type.pp diff_t) Irmin.Type.(unstage (equal diff_t))
  in
  Alcotest.check diff_testable "notification" expected diff;
  let* () = unwatch () in
  Client.Repo.close repo

(* The connection of a watch which cannot be set is closed. *)
let watch_refused () =
  let closed, set_closed = Lwt.wait () in
  let answer (conn : Conn.t) =
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
  let* repo = Client.Repo.v (Irmin_client_unix.config uri) in
  let* () =
    fails "The watch is set"
      (function Error.Error _ -> true | _ -> false)
      (fun () -> watch_branches repo (fun _ -> Lwt.return_unit))
  in
  let* closed = timeout (fun () -> closed) in
  Alcotest.(check bool) "connection closed" true closed;
  Client.Repo.close repo

(* Run [connector] against a websocket server handling its connections
   with [handler ~stop], where [stop] is resolved once [connector] has
   exited. *)
let with_websocket_server (connector, go) handler =
  let port = free_port () in
  let stop, stopped = Lwt.wait () in
  let server =
    Websocket_lwt_unix.establish_standard_server ~stop
      ~mode:(`TCP (`Port port))
      ~check_request:(fun _ -> true)
      ~on_exn:(fun exn -> Logs.debug (fun l -> l "Server: %a" Fmt.exn exn))
      (handler ~stop)
  in
  let* status =
    Lwt.finalize
      (fun () ->
        let* () = start_connector go port in
        timeout (fun () -> wait_child connector))
      (fun () ->
        Lwt.wakeup_later stopped ();
        Lwt_unix.close go)
  in
  let+ () = server in
  Alcotest.(check bool) "client exit" true (status = Unix.WEXITED 0)

(* A frame which is not valid stops the connection of a client, not its
   process. *)
let websocket_bad_server () =
  with_websocket_server bad_server_connector @@ fun ~stop:_ client ->
  let* _ = Websocket_lwt_unix.Connected_client.recv client in
  Websocket_lwt_unix.Connected_client.send client bad_frame

(* The socket of a client is closed with the client, even after a close
   frame of its server. *)
let websocket_closing_server () =
  with_websocket_server closing_server_connector @@ fun ~stop client ->
  let module C = Websocket_lwt_unix.Connected_client in
  (* The handshake of the client is sent back to it *)
  let* handshake = C.recv client in
  let* () = C.send client (binary_frame handshake.content) in
  let* () = C.send client close_frame in
  (* The connection stays open: only the client can close its socket *)
  stop

let misc =
  let branch_diff_t =
    Irmin.Type.pair Store.Branch.t (Irmin.Diff.t Store.commit_key_t)
  in
  [
    ("ping", `Quick, ping);
    ("watch and lost server", `Quick, watch_lost_server unix_watcher);
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
    ( "watch and lost websocket server",
      `Quick,
      watch_lost_server websocket_watcher );
    ("websocket bad server", `Quick, websocket_bad_server);
    ("websocket closing server", `Quick, websocket_closing_server);
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
