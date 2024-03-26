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
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_unix.Make (Store)
module Server = Irmin_server_unix.Make (Store)

let test name f client _switch () =
  Logs.debug (fun l -> l "Running: %s" name);
  f client

let run_server ~sw s =
  let kind, uri =
    match s with
    | `Websocket -> ("Websocket", Uri.of_string "ws://localhost:90991")
    | `Unix_domain ->
        let dir = Unix.getcwd () in
        let sock = Filename.concat dir "test.sock" in
        ("Unix_domain", Uri.of_string ("unix://" ^ sock))
    | `Tcp -> ("Tcp", Uri.of_string "tcp://localhost:90992")
  in
  match Lwt_unix.fork () with
  | 0 ->
      let () = Irmin.Backend.Watch.set_listen_dir_hook Irmin_watcher.hook in
      let conf = Irmin_mem.config () in
      Lwt_eio.run_lwt (fun () -> Server.v ~sw ~uri conf >>= Server.serve);
      (kind, 0, uri)
  | n ->
      Unix.sleep 3;
      (kind, n, uri)

let suite client all =
  List.map
    (fun (name, speed, f) ->
      Alcotest_lwt.test_case name speed (test name f client))
    all
