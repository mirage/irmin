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

(** End-to-end smoke test for the Lwt-over-Eio shim, pack backend.

    Exercises user code (Lwt) -> Irmin_lwt -> Irmin_lwt_pack -> Irmin_pack_unix
    (Eio): a small set of writes / reads / commits / branches on a pack store
    backed by a temporary on-disk directory. *)

module Conf = struct
  let entries = 32
  let stable_hash = 256
  let contents_length_header = Some `Varint
  let inode_child_order = `Seeded_hash
  let forbid_empty_dir_persistence = false
end

module Maker = Irmin_lwt_pack.Maker (Conf)
module S = Maker.Make (Irmin_lwt.Schema.KV (Irmin_lwt.Contents.String))
open Lwt.Syntax

let info ?(author = "test") msg =
  S.Info.v ~author ~message:msg (Int64.of_float (Unix.gettimeofday ()))

let test_basic_set_get config =
  let* repo = S.Repo.v config in
  let* t = S.main repo in
  let* () = S.set_exn t [ "a"; "b" ] "1" ~info:(fun () -> info "set a/b") in
  let* v = S.get t [ "a"; "b" ] in
  assert (v = "1");
  let* found = S.find t [ "a"; "b" ] in
  assert (found = Some "1");
  let* missing = S.find t [ "x" ] in
  assert (missing = None);
  S.Repo.close repo

let test_branch_and_commit config =
  let* repo = S.Repo.v config in
  let* t = S.main repo in
  let* () = S.set_exn t [ "k" ] "v0" ~info:(fun () -> info "init") in
  let* head0 = S.Head.get t in
  let* () = S.set_exn t [ "k" ] "v1" ~info:(fun () -> info "update") in
  let* head1 = S.Head.get t in
  let hash_eq = Irmin.Type.(unstage (equal S.Hash.t)) in
  assert (not (hash_eq (S.Commit.hash head0) (S.Commit.hash head1)));
  let* dev = S.of_branch repo "dev" in
  let* () = S.set_exn dev [ "k" ] "branch" ~info:(fun () -> info "branch") in
  let* main_v = S.get t [ "k" ] in
  let* dev_v = S.get dev [ "k" ] in
  assert (main_v = "v1");
  assert (dev_v = "branch");
  S.Repo.close repo

let test_persistence config_fresh config_reopen =
  let* repo = S.Repo.v config_fresh in
  let* t = S.main repo in
  let* () = S.set_exn t [ "p" ] "persisted" ~info:(fun () -> info "p") in
  let* () = S.Repo.close repo in
  let* repo = S.Repo.v config_reopen in
  let* t = S.main repo in
  let* v = S.get t [ "p" ] in
  assert (v = "persisted");
  S.Repo.close repo

(* Smoke test the irmin-pack-unix advanced surface bridged to Lwt:
   integrity check, flush, GC predicates, stats. We do not exercise a
   full GC run (requires a domain manager and full repo state) -- that
   is a separate, heavier test. *)
let test_pack_advanced config =
  let* repo = S.Repo.v config in
  let* t = S.main repo in
  let* () = S.set_exn t [ "k" ] "v" ~info:(fun () -> info "k") in
  let* commit = S.Head.get t in
  let* () = S.flush repo in
  let* result = S.integrity_check ~auto_repair:false repo in
  (match result with
  | Ok (`Fixed _ | `No_error) -> ()
  | Error _ -> assert false);
  let* split_ok = S.is_split_allowed repo in
  let* gc_ok = S.Gc.is_allowed repo in
  let* gc_finished = S.Gc.is_finished repo in
  assert gc_finished;
  ignore split_ok;
  ignore gc_ok;
  let* () = S.stats ~dump_blob_paths_to:None ~commit repo in
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
    Printf.sprintf "irmin-lwt-pack-test-%d-%d" (Unix.getpid ()) (Random.bits ())
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
  let config_fresh =
    Irmin_pack.Conf.init ~sw ~fs ~fresh:true Eio.Path.(fs / path)
  in
  let config_reopen =
    Irmin_pack.Conf.init ~sw ~fs ~fresh:false Eio.Path.(fs / path)
  in
  let path2 = path ^ "-2" in
  let config_fresh2 () =
    Irmin_pack.Conf.init ~sw ~fs ~fresh:true Eio.Path.(fs / path2)
  in
  run "basic set/get" (fun () -> test_basic_set_get (config_fresh2 ()));
  run "branch and commit" (fun () -> test_branch_and_commit (config_fresh2 ()));
  run "persistence across reopen" (fun () ->
      test_persistence config_fresh config_reopen);
  run "pack advanced surface" (fun () -> test_pack_advanced (config_fresh2 ()));
  (try
     let cmd = Printf.sprintf "rm -rf %s" (Filename.quote path2) in
     ignore (Sys.command cmd)
   with _ -> ());
  print_endline "irmin-lwt-pack: all smoke tests passed"
