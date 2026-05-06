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

(** End-to-end smoke test for irmin-lwt-tezos. Validates that the
    Tezos-flavoured Schema (BLAKE2B + Base58 hash, V1 pre-hashed Node / Commit /
    Contents) round-trips through a Lwt-typed pack store on a temporary
    directory. *)

module S = Irmin_lwt_tezos
open Lwt.Syntax

let info ?(author = "test") msg =
  S.Info.v ~author ~message:msg (Int64.of_float (Unix.gettimeofday ()))

let test_basic config =
  let* repo = S.Repo.v config in
  let* t = S.main repo in
  let* () =
    S.set_exn t [ "k" ] (Bytes.of_string "v") ~info:(fun () -> info "k")
  in
  let* v = S.get t [ "k" ] in
  assert (Bytes.to_string v = "v");
  S.Repo.close repo

let run name f =
  Printf.printf "%-30s " name;
  flush stdout;
  Lwt_eio.Promise.await_lwt (f ());
  print_endline "ok"

let with_tmp_dir env f =
  let fs = Eio.Stdenv.fs env in
  let base = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  let name =
    Printf.sprintf "irmin-lwt-tezos-test-%d-%d" (Unix.getpid ())
      (Random.bits ())
  in
  let path = Filename.concat base name in
  Eio.Switch.run @@ fun sw ->
  let cleanup () =
    try
      let cmd = Printf.sprintf "rm -rf %s" (Filename.quote path) in
      ignore (Sys.command cmd)
    with _ -> ()
  in
  Fun.protect ~finally:cleanup (fun () -> f ~sw ~fs ~path)

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  with_tmp_dir env @@ fun ~sw ~fs ~path ->
  let config = Irmin_pack.Conf.init ~sw ~fs ~fresh:true Eio.Path.(fs / path) in
  run "tezos schema basic" (fun () -> test_basic config);
  print_endline "irmin-lwt-tezos: smoke test passed"
