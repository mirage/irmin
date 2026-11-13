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

let misc =
  [
    ("ping", `Quick, ping);
    ("watch and lost server", `Quick, watch_lost_server unix_watcher);
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
