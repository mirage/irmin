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

(* Fork a server and wait until it should be listening. *)
let fork_server ?dashboard ?max_message_size uri =
  match Lwt_unix.fork () with
  | 0 ->
      in_child @@ fun () ->
      let () = Irmin.Backend.Watch.set_listen_dir_hook Irmin_watcher.hook in
      let conf = Irmin_mem.config () in
      Lwt_main.run
        (Server.v ?dashboard ?max_message_size ~uri conf >>= Server.serve)
  | pid ->
      Unix.sleep 3;
      pid

let dashboard_port = 9299

let run_server s =
  let kind, uri =
    match s with
    | `Websocket -> ("Websocket", Uri.of_string "ws://localhost:90991")
    | `Unix_domain -> ("Unix_domain", unix_uri "test.sock")
    | `Tcp -> ("Tcp", Uri.of_string "tcp://localhost:90992")
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
