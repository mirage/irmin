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

let unix_server = run_server `Unix_domain
let tcp_server = run_server `Tcp
let websocket_server = run_server `Websocket
let unix_watcher = fork_watcher (unix_uri "watch.sock")

(* A server with a small limit on the size of the messages. *)
let small_size = 4096
let small_uri = unix_uri "small.sock"
let (_ : int) = fork_server ~max_message_size:small_size small_uri

module Unix_socket = Make (struct
  let kind, pid, uri = unix_server
end)

module Tcp_socket = Make (struct
  let kind, pid, uri = tcp_server
end)

module Websocket = Make (struct
  let kind, pid, uri = websocket_server
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

(* A test waiting for a peer must fail rather than hang. *)
let timeout f = Lwt_unix.with_timeout 20. f

(* A watch must stop when its server goes away. It was looping for ever,
   without yielding to the scheduler: the watcher could not exit. Removing
   the watch afterwards must not fail. *)
let watch_lost_server { server; watcher; ready; server_gone } () =
  let* ready = timeout (fun () -> Lwt_unix.read ready (Bytes.create 1) 0 1) in
  Alcotest.(check int) "watcher ready" 1 ready;
  Unix.kill server Sys.sigkill;
  let* (_ : int * Unix.process_status) = Lwt_unix.waitpid [] server in
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

let small_config = Irmin_client_unix.config small_uri

(* [f] must fail with an exception accepted by [expected]. *)
let fails msg expected f =
  Lwt.try_bind
    (fun () -> timeout f)
    (fun _ -> Alcotest.fail msg)
    (fun exn -> if expected exn then Lwt.return_unit else Lwt.fail exn)

let too_large = function
  | Irmin_server.Conn.Message_too_large _ -> true
  | _ -> false

let closed = function Irmin.Closed -> true | _ -> false

let check_ping msg repo =
  let+ r = Client.ping repo in
  Alcotest.(check (result unit error)) msg (Ok ()) r

let client_message_size () =
  let* repo = Client.Repo.v small_config in
  let* t = Client.of_branch repo "sizes" in
  let value = String.make 1000 'x' in
  let set t path = Client.set_exn t ~info:Client.Info.none path value in
  let* () = set t [ "a" ] in
  let* small =
    Client.Repo.v (Irmin_client_unix.config ~max_message_size:256 small_uri)
  in
  let* t' = Client.of_branch small "sizes" in
  (* Nothing is sent of a request which is too long: the client goes on *)
  let* () =
    fails "a long request was sent" too_large (fun () -> set t' [ "b" ])
  in
  let* () = check_ping "ping after a long request" small in
  (* A client is closed when it cannot read an answer: what follows on the
     connection would not make sense. *)
  let* () =
    fails "a long answer was accepted" too_large (fun () ->
        Client.get t' [ "a" ])
  in
  let* () =
    fails "the client is still open" closed (fun () -> Client.ping small)
  in
  (* The server answers with an error instead of a response which is too
     long for it, and the connection goes on. *)
  let* () =
    Lwt_list.iter_s (fun i -> set t [ string_of_int i ]) [ 1; 2; 3; 4; 5; 6 ]
  in
  let* () =
    fails "a long response was sent"
      (function Error.Error _ -> true | _ -> false)
      (fun () -> Client.export repo)
  in
  let* () = check_ping "ping after a long response" repo in
  Client.Branch.remove repo "sizes"

(* The server closes the connection when a request is too long for it. The
   client cannot go on with this connection: it is closed. *)
let server_message_size () =
  let* repo = Client.Repo.v small_config in
  let* t = Client.of_branch repo "sizes" in
  let any _ = true in
  let* () =
    fails "a long request was accepted" any (fun () ->
        Client.set_exn t ~info:Client.Info.none [ "a" ]
          (String.make 1_000_000 'x'))
  in
  let* () =
    fails "the connection is still open" any (fun () -> Client.ping repo)
  in
  fails "the client is still open" closed (fun () -> Client.ping repo)

(* A notification which is too long for the server is not sent. The next
   ones are still received. *)
let watch_message_size () =
  let* repo = Client.Repo.v small_config in
  let* t = Client.of_branch repo "sizes" in
  let* () = Client.set_exn t ~info:Client.Info.none [ "a" ] "v" in
  let* head = Client.Head.get t in
  (* The longest name fitting in a request. Its notification is longer. *)
  let long =
    let size name =
      String.length
        (Irmin.Type.(
           unstage (to_bin_string (pair Store.branch_t Store.commit_key_t)))
           (name, Client.Commit.key head))
    in
    let overhead = size (String.make small_size 'b') - small_size in
    String.make (small_size - overhead) 'b'
  in
  let received = ref [] in
  let got_short, short = Lwt.wait () in
  let* watch =
    Client.Branch.watch_all repo (fun branch _ ->
        received := branch :: !received;
        if branch = "short" then Lwt.wakeup_later short ();
        Lwt.return_unit)
  in
  let* () = Client.Branch.set repo long head in
  let* () = Client.Branch.set repo "short" head in
  let* () = timeout (fun () -> got_short) in
  Alcotest.(check (list string)) "notifications" [ "short" ] !received;
  let* () = Client.unwatch watch in
  Lwt_list.iter_s (Client.Branch.remove repo) [ long; "short"; "sizes" ]

(* A limit which is too short for the handshake is refused. *)
let invalid_message_size () =
  let invalid = function Invalid_argument _ -> true | _ -> false in
  let* () =
    fails "client" invalid (fun () ->
        Client.Repo.v (Irmin_client_unix.config ~max_message_size:10 small_uri))
  in
  fails "server" invalid (fun () ->
      Server.v ~max_message_size:10 ~uri:small_uri (Irmin_mem.config ()))

let misc =
  [
    ("ping", `Quick, ping);
    ("watch and lost server", `Quick, watch_lost_server unix_watcher);
    ("message size", `Quick, message_size);
    ("oversized request", `Quick, oversized_request);
    ("client message size", `Quick, client_message_size);
    ("server message size", `Quick, server_message_size);
    ("watch message size", `Quick, watch_message_size);
    ("invalid message size", `Quick, invalid_message_size);
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
