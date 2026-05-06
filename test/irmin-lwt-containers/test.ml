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

(** End-to-end smoke test for irmin-lwt-containers.

    Exercises Counter, Lww_register and Blob_log on top of [Irmin_lwt_mem]. *)

open Lwt.Syntax

let path = [ "k" ]

let test_counter () =
  let module C = Irmin_lwt_containers.Counter.Mem in
  let* repo = C.Store.Repo.v (Irmin_lwt_mem.config ()) in
  let* t = C.Store.main repo in
  let* () = C.inc ~by:5L ~path t in
  let* () = C.inc ~by:3L ~path t in
  let* () = C.dec ~by:1L ~path t in
  let* v = C.read ~path t in
  assert (v = 7L);
  C.Store.Repo.close repo

let test_lww_register () =
  let module L = Irmin_lwt_containers.Lww_register.Mem (struct
    type t = string [@@deriving irmin]
  end) in
  let* repo = L.Store.Repo.v (Irmin_lwt_mem.config ()) in
  let* t = L.Store.main repo in
  let* () = L.write ~path t "first" in
  let* () = L.write ~path t "second" in
  let* v = L.read ~path t in
  assert (v = Some "second");
  L.Store.Repo.close repo

let test_blob_log () =
  let module B = Irmin_lwt_containers.Blob_log.Mem (struct
    type t = string [@@deriving irmin]
  end) in
  let* repo = B.Store.Repo.v (Irmin_lwt_mem.config ()) in
  let* t = B.Store.main repo in
  let* () = B.append ~path t "a" in
  let* () = B.append ~path t "b" in
  let* () = B.append ~path t "c" in
  let* l = B.read_all ~path t in
  (* entries are stored newest-first *)
  assert (l = [ "c"; "b"; "a" ]);
  B.Store.Repo.close repo

let run name f =
  Printf.printf "%-30s " name;
  flush stdout;
  Lwt_eio.Promise.await_lwt (f ());
  print_endline "ok"

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  run "Counter" test_counter;
  run "Lww_register" test_lww_register;
  run "Blob_log" test_blob_log;
  print_endline "irmin-lwt-containers: all smoke tests passed"
