(*
 * Copyright (c) 2026 Tarides
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

(** End-to-end smoke test for irmin-lwt-client.

    Spawns an [Irmin_server] in an Eio fiber listening on a Unix-domain socket,
    then uses the Lwt-typed [Irmin_lwt_client] to connect and do basic set/get
    operations over the wire. *)

let () = Mirage_crypto_rng_unix.use_default ()

(* The server uses an in-memory Irmin store; the client mirrors its Schema
   via [Irmin_lwt_client.Make (Irmin_lwt.Contents.String)]. *)
module Server_store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Server = Irmin_server_unix.Make (Server_store)
module Client = Irmin_lwt_client.Make (Irmin_lwt.Contents.String)
open Lwt.Syntax

let info ?(author = "test") msg =
  Client.Info.v ~author ~message:msg (Int64.of_float (Unix.gettimeofday ()))

let with_server ~sw ~clock f =
  let dir = Unix.getcwd () in
  let sock = Filename.concat dir "test-lwt-client.sock" in
  (try Unix.unlink sock with _ -> ());
  let uri = Uri.of_string ("unix://" ^ sock) in
  let stop, set_stop = Lwt.wait () in
  Eio.Switch.on_release sw (fun () -> Lwt.wakeup_later set_stop ());
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let spec = Irmin.Backend.Conf.Spec.v "Unix_domain" in
      let key = Irmin.Backend.Conf.root spec in
      let conf = Irmin.Backend.Conf.singleton spec key "Unix_domain" in
      Lwt_eio.run_lwt (fun () ->
          let open Lwt.Infix in
          Server.v ~uri conf >>= Server.serve ~stop);
      `Stop_daemon);
  Eio.Time.sleep clock 0.2;
  f uri

let test_set_get uri =
  let* repo = Client.connect uri in
  let* t = Client.main repo in
  let* () = Client.set_exn t [ "k" ] "v" ~info:(fun () -> info "k") in
  let* v = Client.get t [ "k" ] in
  assert (v = "v");
  let* found = Client.find t [ "k" ] in
  assert (found = Some "v");
  Client.close repo

let run name f =
  Printf.printf "%-30s " name;
  flush stdout;
  Lwt_eio.Promise.await_lwt (f ());
  print_endline "ok"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#clock in
  Lwt_eio.with_event_loop ~clock @@ fun _ ->
  with_server ~sw ~clock @@ fun uri ->
  run "set/get over Unix socket" (fun () -> test_set_get uri);
  print_endline "irmin-lwt-client: smoke test passed"
