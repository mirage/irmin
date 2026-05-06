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

(** End-to-end smoke test for irmin-lwt-git on the in-memory variant.

    Exercises the chain user code (Lwt) -> Irmin_lwt -> Irmin_lwt_git ->
    Irmin_git (Eio) on top of the in-memory Git store [Irmin_git.Mem]. *)

module S = Irmin_lwt_git.Mem.KV (Irmin_lwt.Contents.String)
module Irmin_test = Irmin_lwt_test.Irmin_test
open Lwt.Syntax

let info ?(author = "test") msg =
  S.Info.v ~author ~message:msg (Int64.of_float (Unix.gettimeofday ()))

let test_basic_set_get () =
  let* repo = S.Repo.v (Irmin_lwt_git.config "_build/test-lwt-git-mem") in
  let* t = S.main repo in
  let* () = S.set_exn t [ "a"; "b" ] "1" ~info:(fun () -> info "set a/b") in
  let* v = S.get t [ "a"; "b" ] in
  assert (v = "1");
  let* found = S.find t [ "a"; "b" ] in
  assert (found = Some "1");
  let* missing = S.find t [ "x" ] in
  assert (missing = None);
  S.Repo.close repo

let test_branch_and_commit () =
  let* repo = S.Repo.v (Irmin_lwt_git.config "_build/test-lwt-git-mem-2") in
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

let test_git_commit_passthrough () =
  let* repo = S.Repo.v (Irmin_lwt_git.config "_build/test-lwt-git-mem-3") in
  let* t = S.main repo in
  let* () = S.set_exn t [ "k" ] "v" ~info:(fun () -> info "k") in
  let* head = S.Head.get t in
  let* git_commit = S.git_commit repo head in
  (* The git commit object must be retrievable. *)
  assert (Option.is_some git_commit);
  S.Repo.close repo

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
  run "basic set/get" test_basic_set_get;
  run "branch and commit" test_branch_and_commit;
  run "git_commit passthrough" test_git_commit_passthrough;
  print_endline "--- running irmin-lwt-test harness ---";
  let suite =
    let store = (module S : Irmin_test.Generic_key) in
    let init ~config:_ = Lwt.return_unit in
    Irmin_test.Suite.create_generic_key ~name:"GIT" ~init ~store
      ~config:(Irmin_lwt_git.config "_build/test-lwt-git-harness")
      ()
  in
  Lwt_eio.Promise.await_lwt
    (Irmin_test.Store.run "irmin-lwt-git" ~slow:false ~misc:[]
       ~sleep:Lwt_unix.sleep
       [ (`Quick, suite) ]);
  print_endline "irmin-lwt-git: all smoke tests + harness passed"
