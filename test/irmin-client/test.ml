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

(* A test waiting for a peer must fail rather than hang. *)
let timeout f = Lwt_unix.with_timeout 20. f

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
  (* The limit is the same in both directions *)
  let* () =
    with_pipe ~max_message_size:16 @@ fun conn ->
    check_too_large 17L (fun () ->
        Conn.write conn Irmin.Type.string (String.make 17 'x'))
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

(* A client speaking the protocol by hand. *)
let raw_connection uri =
  let open Lwt.Syntax in
  let* ic, oc = Lwt_io.open_connection (Unix.ADDR_UNIX (Uri.path uri)) in
  let conn = Conn.v ic oc in
  let+ ok = Conn.Handshake.V1.send (module Store) conn in
  Alcotest.(check bool) "handshake" true ok;
  conn

let send_command conn command =
  Conn.Request.write_header conn (Conn.Request.v_header ~command)

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

(* [f ()] must raise an exception accepted by [expected]. *)
let check_raises msg expected f =
  match f () with
  | _ -> Alcotest.fail msg
  | exception exn when expected exn -> ()

(* A client is closed when it cannot read an answer: what follows on the
   connection would not make sense. *)
let client_message_size ~clock () =
  Eio.Switch.run @@ fun sw ->
  let uri = Uri.of_string "ws://127.0.0.1:9298" in
  start_server ~sw ~clock ~max_message_size:4096 "Sizes" uri;
  let repo = Client.Repo.v (Irmin_client_unix.config uri) in
  let t = Client.of_branch repo "sizes" in
  let value = String.make 1000 'x' in
  let set step = Client.set_exn t ~info:Client.Info.none [ step ] value in
  set "a";
  let small =
    Client.Repo.v (Irmin_client_unix.config ~max_message_size:256 uri)
  in
  let t' = Client.of_branch small "sizes" in
  check_raises "a long answer was accepted"
    (function Irmin_server.Conn.Message_too_large _ -> true | _ -> false)
    (fun () -> Client.get t' [ "a" ]);
  check_raises "the client is still open"
    (function Irmin.Closed -> true | _ -> false)
    (fun () -> Client.ping small);
  (* The server answers with an error instead of a response which is too
     long for it, and the connection goes on. *)
  List.iter set [ "1"; "2"; "3"; "4"; "5"; "6" ];
  check_raises "a long response was sent"
    (function Error.Error _ -> true | _ -> false)
    (fun () -> Client.export repo);
  Alcotest.(check (result unit error)) "ping" (Ok ()) (Client.ping repo)

(* The dashboard sends the paths of the store as JSON strings, whatever
   they contain. *)
let dashboard_paths client () =
  let entry_t =
    Irmin.Type.(
      record "entry" (fun path kind hash -> (path, kind, hash))
      |+ field "path" string (fun (path, _, _) -> path)
      |+ field "kind" string (fun (_, kind, _) -> kind)
      |+ field "hash" string (fun (_, _, hash) -> hash)
      |> sealr)
  in
  let step = {|"<img src=x onerror=alert(1)>\|} in
  let repo = client () in
  let t = Client.of_branch repo "dashboard" in
  Client.set_exn t ~info:Client.Info.none [ step ] "v";
  let uri =
    Uri.of_string
      (Fmt.str "http://127.0.0.1:%d/?branch=dashboard" dashboard_port)
  in
  let body =
    Lwt_eio.run_lwt @@ fun () ->
    let open Lwt.Syntax in
    let* _, body = Cohttp_lwt_unix.Client.post uri in
    Cohttp_lwt.Body.to_string body
  in
  Client.Branch.remove repo "dashboard";
  match Irmin.Type.(of_json_string (list entry_t)) body with
  | Error (`Msg e) -> Alcotest.failf "invalid JSON %S: %s" body e
  | Ok entries ->
      let paths = List.map (fun (path, _, _) -> path) entries in
      Alcotest.(check (list string)) "paths" [ "/" ^ step ] paths

let misc ~uri ~clock client =
  [
    ("ping", `Quick, ping client);
    ("watch and lost server", `Quick, watch_lost_server);
    ("message size", `Quick, message_size);
    ("oversized request", `Quick, oversized_request ~uri client);
    ("client message size", `Quick, client_message_size ~clock);
    ("dashboard paths", `Quick, dashboard_paths client);
  ]

let misc ~uri ~clock client = [ ("misc", misc ~uri ~clock client) ]

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
