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

(** End-to-end smoke test for irmin-lwt-chunk.

    Combines [Irmin_lwt_chunk.Content_addressable] (chunking layer) with
    [Irmin_lwt_mem]'s Append_only and Atomic_write makers to build a Lwt-typed
    chunked KV store. The chunking is exercised by writing a value larger than
    the default chunk size threshold. *)

module CA = Irmin_lwt_chunk.Content_addressable (Irmin_lwt_mem.Append_only)
module Chunk_maker = Irmin_lwt.Maker (CA) (Irmin_lwt_mem.Atomic_write)
module S = Irmin_lwt.KV_maker (CA) (Irmin_lwt_mem.Atomic_write)
module Store = S.Make (Irmin_lwt.Contents.String)
module Irmin_test = Irmin_lwt_test.Irmin_test
open Lwt.Syntax

let info ?(author = "test") msg =
  Store.Info.v ~author ~message:msg (Int64.of_float (Unix.gettimeofday ()))

let test_basic () =
  let config =
    Irmin_lwt_chunk.config ~size:512 ~min_size:131 (Irmin_lwt_mem.config ())
  in
  let* repo = Store.Repo.v config in
  let* t = Store.main repo in
  let small = "abc" in
  let big = String.make 5000 'x' in
  let* () = Store.set_exn t [ "small" ] small ~info:(fun () -> info "small") in
  let* () = Store.set_exn t [ "big" ] big ~info:(fun () -> info "big") in
  let* small' = Store.get t [ "small" ] in
  let* big' = Store.get t [ "big" ] in
  assert (small = small');
  assert (big = big');
  Store.Repo.close repo

let run name f =
  Printf.printf "%-30s " name;
  flush stdout;
  Lwt_eio.Promise.await_lwt (f ());
  print_endline "ok"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Irmin.Backend.Watch.set_watch_switch sw;
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  run "chunked set/get small+big" test_basic;
  print_endline "--- running irmin-lwt-test harness ---";
  let suite =
    let store =
      Irmin_test.store (module Chunk_maker) (module Irmin_lwt.Metadata.None)
    in
    let init ~config:_ = Lwt.return_unit in
    let config =
      Irmin_lwt_chunk.config ~size:512 ~min_size:131 (Irmin_lwt_mem.config ())
    in
    Irmin_test.Suite.create ~name:"CHUNK" ~init ~store ~config ()
  in
  Lwt_eio.Promise.await_lwt
    (Irmin_test.Store.run "irmin-lwt-chunk" ~slow:false ~misc:[]
       ~sleep:Lwt_unix.sleep
       [ (`Quick, suite) ]);
  print_endline "irmin-lwt-chunk: all tests passed"
