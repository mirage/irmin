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

let misc client =
  let branch_diff_t =
    Irmin.Type.pair Store.Branch.t (Irmin.Diff.t Store.commit_key_t)
  in
  [
    ("ping", `Quick, ping client);
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
    ~misc:[]
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
