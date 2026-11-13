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
  val pid : int
  val uri : Uri.t
  val kind : string
end

module Make (R : R) = struct
  let () = at_exit (fun () -> try Unix.kill R.pid Sys.sigint with _ -> ())
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

let kind, pid, uri = run_server `Unix_domain

module Unix_socket = Make (struct
  let pid = pid
  let uri = uri
  let kind = kind
end)

module Tcp_socket = Make (struct
  let kind, pid, uri = run_server `Tcp
end)

module Websocket = Make (struct
  let kind, pid, uri = run_server `Websocket
end)

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

let kill_quietly pid =
  try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ()

(* A test waiting for a peer must fail rather than hang. *)
let timeout f = Lwt_unix.with_timeout 20. f

(* A process watching a branch of its own server. It says when the watch is
   set, then exits once told that the server is gone. *)
let watch_server, watcher, watcher_ready, server_gone =
  let uri = unix_uri "watch.sock" in
  let server = fork_server uri in
  let ready_r, ready_w = Unix.pipe () and gone_r, gone_w = Unix.pipe () in
  match Lwt_unix.fork () with
  | 0 ->
      in_child @@ fun () ->
      Unix.close ready_r;
      Unix.close gone_w;
      let ready = Lwt_unix.of_unix_file_descr ready_w
      and gone = Lwt_unix.of_unix_file_descr gone_r in
      Lwt_main.run
        (let* repo = Client.Repo.v (Irmin_client_unix.config uri) in
         let* (_ : Client.watch) =
           Client.Branch.watch repo "main" (fun _ -> Lwt.return_unit)
         in
         let* (_ : int) = Lwt_unix.write ready (Bytes.make 1 'r') 0 1 in
         let+ (_ : int) = Lwt_unix.read gone (Bytes.create 1) 0 1 in
         ())
  | watcher ->
      Unix.close ready_w;
      Unix.close gone_r;
      at_exit (fun () ->
          kill_quietly server;
          kill_quietly watcher);
      ( server,
        watcher,
        Lwt_unix.of_unix_file_descr ready_r,
        Lwt_unix.of_unix_file_descr gone_w )

(* A watch must stop when its server goes away. It was looping for ever,
   without yielding to the scheduler: the watcher could not exit. *)
let watch_lost_server () =
  let* ready =
    timeout (fun () -> Lwt_unix.read watcher_ready (Bytes.create 1) 0 1)
  in
  Alcotest.(check int) "watcher ready" 1 ready;
  Unix.kill watch_server Sys.sigkill;
  let* (_ : int * Unix.process_status) = Lwt_unix.waitpid [] watch_server in
  let* (_ : int) = Lwt_unix.write server_gone (Bytes.make 1 'g') 0 1 in
  Lwt.catch
    (fun () ->
      timeout @@ fun () ->
      let+ _, status = Lwt_unix.waitpid [] watcher in
      Alcotest.(check bool) "watcher exit" true (status = Unix.WEXITED 0))
    (function
      | Lwt_unix.Timeout ->
          kill_quietly watcher;
          Alcotest.fail "The watch keeps the scheduler busy without its server"
      | exn -> Lwt.fail exn)

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
let raw_connection () =
  let* ic, oc = Lwt_io.open_connection (Unix.ADDR_UNIX (Uri.path uri)) in
  let conn = Conn.v ic oc in
  let+ ok = Conn.Handshake.V1.send (module Store) conn in
  Alcotest.(check bool) "handshake" true ok;
  conn

let send_command conn command =
  Conn.Request.write_header conn (Conn.Request.v_header ~command)

(* The server closes the connection of a client announcing a message which is
   too long, and goes on serving the other clients. *)
let oversized_request () =
  let* conn = raw_connection () in
  let ic = conn.ic and oc = conn.oc in
  let* () = send_command conn "ping" in
  let* () = Lwt_io.BE.write_int64 oc 0x1_0000_0000L in
  let* () = Lwt_io.flush oc in
  let* header = timeout (fun () -> Conn.Response.read_header conn) in
  let* err = timeout (fun () -> Conn.Response.get_error conn header) in
  Alcotest.(check (option string)) "error" (Some "ERROR message too large") err;
  let* closed =
    Lwt.try_bind
      (fun () -> timeout (fun () -> Lwt_io.read_char ic))
      (fun _ -> Lwt.return_false)
      (function End_of_file -> Lwt.return_true | exn -> Lwt.fail exn)
  in
  Alcotest.(check bool) "connection closed" true closed;
  let* () = Lwt_io.close ic in
  ping ()

(* A websocket server with its own limits. *)
let small_port = 9298
let small_size = 4096
let small_uri = Uri.of_string (Fmt.str "ws://127.0.0.1:%d" small_port)

let () =
  let pid = fork_server ~max_message_size:small_size small_uri in
  at_exit (fun () -> kill_quietly pid)

let small_config = Irmin_client_unix.config small_uri

(* A client is closed when it cannot read an answer: what follows on the
   connection would not make sense. *)
let client_message_size () =
  let* repo = Client.Repo.v small_config in
  let* t = Client.of_branch repo "sizes" in
  let value = String.make 1000 'x' in
  let* () = Client.set_exn t ~info:Client.Info.none [ "a" ] value in
  let* small =
    Client.Repo.v (Irmin_client_unix.config ~max_message_size:256 small_uri)
  in
  let* t' = Client.of_branch small "sizes" in
  let* () =
    Lwt.try_bind
      (fun () -> timeout (fun () -> Client.get t' [ "a" ]))
      (fun _ -> Alcotest.fail "a long answer was accepted")
      (function
        | Irmin_server.Conn.Message_too_large _ -> Lwt.return_unit
        | exn -> Lwt.fail exn)
  in
  let* () =
    Lwt.try_bind
      (fun () -> timeout (fun () -> Client.ping small))
      (fun _ -> Alcotest.fail "the client is still open")
      (function Irmin.Closed -> Lwt.return_unit | exn -> Lwt.fail exn)
  in
  (* The server answers with an error instead of a response which is too
     long for it, and the connection goes on. *)
  let* () =
    Lwt_list.iter_s
      (fun i ->
        Client.set_exn t ~info:Client.Info.none [ string_of_int i ] value)
      [ 1; 2; 3; 4; 5; 6 ]
  in
  let* () =
    Lwt.try_bind
      (fun () -> timeout (fun () -> Client.export repo))
      (fun _ -> Alcotest.fail "a long response was sent")
      (function Error.Error _ -> Lwt.return_unit | exn -> Lwt.fail exn)
  in
  let* r = Client.ping repo in
  Alcotest.(check (result unit error)) "ping" (Ok ()) r;
  Client.Branch.remove repo "sizes"

let misc =
  [
    ("ping", `Quick, ping);
    ("watch and lost server", `Quick, watch_lost_server);
    ("message size", `Quick, message_size);
    ("oversized request", `Quick, oversized_request);
    ("client message size", `Quick, client_message_size);
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
