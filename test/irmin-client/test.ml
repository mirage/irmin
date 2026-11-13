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

(* Connections over the pipes of [Lwt_io]. *)
module Pipe_io = struct
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

module Conn = Irmin_server.Conn.Make (Pipe_io) (Irmin_server.Conn.Codec.Bin)

(* [with_watchdog f] exits if [f] does not return: a scheduler which is kept
   busy cannot report the failure itself. *)
let with_watchdog msg f =
  let finished = Atomic.make false in
  let rec wait n =
    if Atomic.get finished then ()
    else if n = 0 then (
      prerr_endline msg;
      Unix._exit 1)
    else (
      Unix.sleepf 0.1;
      wait (n - 1))
  in
  let watchdog = Domain.spawn (fun () -> wait 200) in
  Fun.protect f ~finally:(fun () ->
      Atomic.set finished true;
      Domain.join watchdog)

(* A watch must stop when its server goes away. It was looping for ever,
   without yielding to the scheduler. The server here accepts the watch, then
   closes the connection. *)
let watch_lost_server () =
  let open Lwt.Syntax in
  let path = Filename.concat (Unix.getcwd ()) "watch.sock" in
  (try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  let handler _addr (ic, oc) =
    let conn = Conn.v ic oc in
    Lwt.catch
      (fun () ->
        let* (_ : bool) = Conn.Handshake.V1.check (module Store) conn in
        let* (_ : Conn.Request.header) = Conn.Request.read_header conn in
        Conn.ok conn)
      (function End_of_file -> Lwt.return_unit | exn -> Lwt.fail exn)
  in
  let server =
    Lwt_eio.run_lwt @@ fun () ->
    Lwt_io.establish_server_with_client_address (Unix.ADDR_UNIX path) handler
  in
  with_watchdog "The watch keeps the scheduler busy without its server"
  @@ fun () ->
  let config = Irmin_client_unix.config (Uri.of_string ("unix://" ^ path)) in
  let repo = Client.Repo.v config in
  let (_ : Client.watch) = Client.Branch.watch repo "main" ignore in
  Eio_unix.sleep 0.5;
  Lwt_eio.run_lwt @@ fun () -> Lwt_io.shutdown_server server

let misc client =
  [
    ("ping", `Quick, ping client);
    ("watch and lost server", `Quick, watch_lost_server);
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
