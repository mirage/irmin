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

let unix_uri name =
  Uri.of_string ("unix://" ^ Filename.concat (Unix.getcwd ()) name)

(* A forked process must never come back to the code of the tests, nor run
   their [at_exit] functions. *)
let in_child f =
  let code =
    match f () with
    | () -> 0
    | exception exn ->
        Fmt.epr "Child process: %a@." Fmt.exn exn;
        1
  in
  Unix._exit code

let kill_quietly pid =
  try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ()

(* Wait until a server accepts the connections. *)
let wait_for_server uri =
  let addr =
    match (Uri.scheme uri, Uri.port uri) with
    | Some "unix", _ -> Unix.ADDR_UNIX (Uri.path uri)
    | _, Some port -> Unix.ADDR_INET (Unix.inet_addr_loopback, port)
    | _, None -> Fmt.failwith "No port in %a" Uri.pp uri
  in
  let connect () =
    let socket =
      Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0
    in
    Fun.protect
      ~finally:(fun () -> Unix.close socket)
      (fun () ->
        match Unix.connect socket addr with
        | () -> true
        | exception Unix.Unix_error ((Unix.ECONNREFUSED | Unix.ENOENT), _, _) ->
            false)
  in
  let rec wait tries =
    if not (connect ()) then
      if tries = 0 then Fmt.failwith "No server on %a" Uri.pp uri
      else (
        Unix.sleepf 0.05;
        wait (tries - 1))
  in
  wait 400

(* Fork a server and wait until it is listening. The processes are forked
   before the first client is created: a child must not inherit the
   connections of the tests. *)
let fork_server ?dashboard ?max_message_size uri =
  match Lwt_unix.fork () with
  | 0 ->
      in_child @@ fun () ->
      let () = Irmin.Backend.Watch.set_listen_dir_hook Irmin_watcher.hook in
      let conf = Irmin_mem.config () in
      Lwt_main.run
        (Server.v ?dashboard ?max_message_size ~uri conf >>= Server.serve)
  | pid ->
      at_exit (fun () -> kill_quietly pid);
      wait_for_server uri;
      pid

type watcher = {
  server : int;
  watcher : int;
  ready : Lwt_unix.file_descr;
  server_gone : Lwt_unix.file_descr;
}

(* A process watching a branch of its own server. It says when the watch is
   set. Once told that the server is gone, it removes the watch and exits. *)
let fork_watcher uri =
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
        ( Client.Repo.v (Irmin_client_unix.config uri) >>= fun repo ->
          Client.Branch.watch repo "main" (fun _ -> Lwt.return_unit)
          >>= fun watch ->
          Lwt_unix.write ready (Bytes.make 1 'r') 0 1 >>= fun (_ : int) ->
          Lwt_unix.read gone (Bytes.create 1) 0 1 >>= fun (_ : int) ->
          Client.unwatch watch )
  | watcher ->
      Unix.close ready_w;
      Unix.close gone_r;
      at_exit (fun () -> kill_quietly watcher);
      {
        server;
        watcher;
        ready = Lwt_unix.of_unix_file_descr ready_r;
        server_gone = Lwt_unix.of_unix_file_descr gone_w;
      }

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

let dashboard_port = free_port ()

let run_server s =
  let uri scheme =
    Uri.of_string (Fmt.str "%s://localhost:%d" scheme (free_port ()))
  in
  let kind, uri =
    match s with
    | `Websocket -> ("Websocket", uri "ws")
    | `Unix_domain -> ("Unix_domain", unix_uri "test.sock")
    | `Tcp -> ("Tcp", uri "tcp")
  in
  let dashboard =
    match s with
    | `Unix_domain -> Some (`TCP (`Port dashboard_port))
    | `Tcp | `Websocket -> None
  in
  (kind, fork_server ?dashboard uri, uri)

let suite client all =
  List.map
    (fun (name, speed, f) ->
      Alcotest_lwt.test_case name speed (test name f client))
    all
