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

(** End-to-end smoke test for the Lwt-over-Eio shim, fs (Unix) backend.

    Exercises the chain user code (Lwt) -> Irmin_lwt -> Irmin_lwt_fs ->
    Irmin_fs_unix (Eio): writes / reads / commits / branches / persistence on a
    filesystem-backed store rooted in a temporary directory. *)

module S = Irmin_lwt_fs.KV.Make (Irmin_lwt.Contents.String)
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

let test_persistence config =
  let* repo = S.Repo.v config in
  let* t = S.main repo in
  let* () = S.set_exn t [ "p" ] "persisted" ~info:(fun () -> info "p") in
  let* () = S.Repo.close repo in
  let* repo = S.Repo.v config in
  let* t = S.main repo in
  let* v = S.get t [ "p" ] in
  assert (v = "persisted");
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
    Printf.sprintf "irmin-lwt-fs-test-%d-%d" (Unix.getpid ()) (Random.bits ())
  in
  let path = Filename.concat base name in
  let cleanup () =
    try
      let cmd = Printf.sprintf "rm -rf %s" (Filename.quote path) in
      ignore (Sys.command cmd)
    with _ -> ()
  in
  Fun.protect ~finally:cleanup (fun () -> f ~fs ~path)

module Irmin_test = Irmin_lwt_test.Irmin_test

(* Bridge [Irmin_watcher.hook] (Lwt-typed) to [Irmin.Backend.Watch.hook]
   (Eio direct-style). Required by the harness Watch test on filesystem
   backends. *)
let bridged_listen_dir_hook : Irmin.Backend.Watch.hook =
 fun id path f ->
  let f_lwt s = Lwt_eio.run_eio (fun () -> f s) in
  let unhook_lwt =
    Lwt_eio.Promise.await_lwt (Irmin_watcher.hook id path f_lwt)
  in
  fun () -> Lwt_eio.Promise.await_lwt (unhook_lwt ())

let suite_for config =
  let store =
    Irmin_test.store (module Irmin_lwt_fs) (module Irmin_lwt.Metadata.None)
  in
  let init ~config:_ = Lwt.return_unit in
  Irmin_test.Suite.create ~name:"FS" ~init ~store ~config ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Irmin.Backend.Watch.set_watch_switch sw;
  Irmin.Backend.Watch.set_listen_dir_hook bridged_listen_dir_hook;
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  with_tmp_dir env @@ fun ~fs ~path ->
  let config_at sub =
    Irmin_lwt_fs.config
      ~root:Eio.Path.(fs / Filename.concat path sub)
      ~clock:env#clock
  in
  run "basic set/get" (fun () -> test_basic_set_get (config_at "a"));
  run "branch and commit" (fun () -> test_branch_and_commit (config_at "b"));
  run "persistence across reopen" (fun () -> test_persistence (config_at "c"));
  print_endline "--- running irmin-lwt-test harness ---";
  Lwt_eio.Promise.await_lwt
    (Irmin_test.Store.run "irmin-lwt-fs" ~slow:false ~misc:[]
       ~sleep:Lwt_unix.sleep
       [ (`Quick, suite_for (config_at "harness")) ]);
  print_endline "irmin-lwt-fs: all smoke tests + harness passed"
