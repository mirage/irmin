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

let run_server ~sw ~clock s =
  let kind, uri =
    match s with
    | `Websocket -> ("Websocket", Uri.of_string "ws://localhost:90991")
    | `Unix_domain ->
        let dir = Unix.getcwd () in
        let sock = Filename.concat dir "test.sock" in
        ("Unix_domain", Uri.of_string ("unix://" ^ sock))
    | `Tcp -> ("Tcp", Uri.of_string "tcp://localhost:90992")
  in
  let stop, set_stop = Lwt.wait () in
  Eio.Switch.on_release sw (fun () -> Lwt.wakeup_later set_stop ());
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let () = Irmin.Backend.Watch.set_listen_dir_hook Irmin_watcher.hook in
      let spec = Irmin.Backend.Conf.Spec.v kind in
      let key = Irmin.Backend.Conf.root spec in
      let conf = Irmin.Backend.Conf.singleton spec key kind in
      Lwt_eio.run_lwt (fun () -> Server.v ~uri conf >>= Server.serve ~stop);
      `Stop_daemon);
  Eio.Time.sleep clock 0.1;
  (kind, uri)
