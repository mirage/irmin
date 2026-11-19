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

(* A server with a small limit on the size of the messages. *)
let small_size = 4096
let small_uri = unix_uri "small.sock"
let (_ : int) = fork_server ~max_message_size:small_size small_uri

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
  fails "a long handshake was read"
    (function Error.Error msg -> msg = "invalid handshake" | _ -> false)
    (fun () -> Conn.Handshake.V1.send (module Store) conn)

(* The server closes the connection of a client announcing a message which is
   too long, and goes on serving the other clients. *)
let oversized_request () =
  let* conn = raw_connection uri in
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

(* A notification which is too long for the client cannot be skipped, as it
   has not been read: the watch stops. *)
let client_watch_message_size () =
  let* repo = Client.Repo.v small_config in
  let* t = Client.of_branch repo "sizes" in
  let* () = Client.set_exn t ~info:Client.Info.none [ "a" ] "v" in
  let* head = Client.Head.get t in
  let* small =
    Client.Repo.v (Irmin_client_unix.config ~max_message_size:256 small_uri)
  in
  let received = ref [] in
  let got_first, first = Lwt.wait () in
  let* watch =
    Client.Branch.watch_all small (fun branch _ ->
        received := branch :: !received;
        if branch = "first" then Lwt.wakeup_later first ();
        Lwt.return_unit)
  in
  (* The watch works before the long notification *)
  let* () = Client.Branch.set repo "first" head in
  let* () = timeout (fun () -> got_first) in
  (* A watch with the default limit tells when the notifications are sent *)
  let got_short, short = Lwt.wait () in
  let* sync =
    Client.Branch.watch_all repo (fun branch _ ->
        if branch = "short" then Lwt.wakeup_later short ();
        Lwt.return_unit)
  in
  let long = String.make 300 'b' in
  let* () = Client.Branch.set repo long head in
  let* () = Client.Branch.set repo "short" head in
  let* () = timeout (fun () -> got_short) in
  let* () = Lwt_unix.sleep 0.1 in
  Alcotest.(check (list string)) "notifications" [ "first" ] !received;
  let* () = Client.unwatch watch in
  let* () = Client.unwatch sync in
  let* () = check_ping "ping after a long notification" small in
  Lwt_list.iter_s
    (Client.Branch.remove repo)
    [ "first"; long; "short"; "sizes" ]

(* A limit which is too short for the handshake, or longer than a string, is
   refused. *)
let invalid_message_size () =
  let invalid = function Invalid_argument _ -> true | _ -> false in
  let* () =
    Lwt_list.iter_s
      (fun max_message_size ->
        let* () =
          fails "client" invalid (fun () ->
              Client.Repo.v
                (Irmin_client_unix.config ~max_message_size small_uri))
        in
        fails "server" invalid (fun () ->
            Server.v ~max_message_size ~uri:small_uri (Irmin_mem.config ())))
      [ 10; Sys.max_string_length + 1 ]
  in
  fails "connection" invalid (fun () ->
      Lwt.wrap (fun () ->
          Conn.v ~max_message_size:(-1) Lwt_io.stdin Lwt_io.stdout))

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

(* The answers of a server which refuses the handshake of a client, with the
   error they give to the client. *)
let refusals =
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
  let* () =
    fails "handshake accepted" expected (fun () ->
        Client.Repo.v (Irmin_client_unix.config uri))
  in
  let+ closed = timeout (fun () -> closed) in
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

let misc =
  let branch_diff_t =
    Irmin.Type.pair Store.Branch.t (Irmin.Diff.t Store.commit_key_t)
  in
  [
    ("ping", `Quick, ping);
    ("message size", `Quick, message_size);
    ("handshake size", `Quick, handshake_size);
    ("oversized request", `Quick, oversized_request);
    ("client message size", `Quick, client_message_size);
    ("server message size", `Quick, server_message_size);
    ("watch message size", `Quick, watch_message_size);
    ("client watch message size", `Quick, client_watch_message_size);
    ("invalid message size", `Quick, invalid_message_size);
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
  ]
  @ failed_handshakes

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
