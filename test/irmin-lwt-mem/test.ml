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

(** End-to-end smoke test for the Lwt-over-Eio shim.

    Exercises the chain user code (Lwt) -> Irmin_lwt -> Irmin_lwt_mem ->
    Irmin_mem (Eio): a small set of writes / reads / commits / lookups on a KV
    store backed by Irmin 4's in-memory backend. *)

module S = Irmin_lwt_mem.KV.Make (Irmin_lwt.Contents.String)
open Lwt.Syntax

let info ?(author = "test") msg =
  S.Info.v ~author ~message:msg (Int64.of_float (Unix.gettimeofday ()))

let test_basic_set_get () =
  let* repo = S.Repo.v (Irmin_lwt_mem.config ()) in
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
  let* repo = S.Repo.v (Irmin_lwt_mem.config ()) in
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

(* Sync.fetch / push between two in-memory repos: validates that
   [S.Backend.Slice], [S.Backend.Remote] and [S.Repo.export]/[import]
   round-trip correctly through Wrap_store's Lwt <-> Eio bridges. *)
module Sync = Irmin_lwt.Sync.Make (S)

let test_sync_between_repos () =
  let* src_repo = S.Repo.v (Irmin_lwt_mem.config ()) in
  let* dst_repo = S.Repo.v (Irmin_lwt_mem.config ()) in
  let* src = S.main src_repo in
  let* () = S.set_exn src [ "x" ] "from-src" ~info:(fun () -> info "x") in
  let* () = S.set_exn src [ "y" ] "also" ~info:(fun () -> info "y") in
  let* dst = S.main dst_repo in
  let remote = Irmin_lwt.remote_store (module S) src in
  let* status = Sync.pull_exn dst remote `Set in
  (match status with `Empty -> assert false | `Head _ -> ());
  let* x = S.get dst [ "x" ] in
  let* y = S.get dst [ "y" ] in
  assert (x = "from-src");
  assert (y = "also");
  let* () = S.Repo.close src_repo in
  S.Repo.close dst_repo

(* Dot.output_buffer: validates that the dot graph generation round-trips
   [S.Backend.Slice.iter] and [S.Backend.Branch.list] via Wrap_store. *)
module Dot = Irmin_lwt.Dot (S)

(* [S.E] is the per-backend [Remote.t] constructor: [Wrap_store.Make]
   forwards [E = Inner.E] so user code can construct [S.E e] with the
   backend's endpoint type. We don't call Sync on it (irmin-mem has no
   real remote and would fail with "fetch operation is not available")
   — the goal here is just to type-check that [S.E ()] is a value of
   [Irmin_lwt.remote] (= [Irmin.remote], the same extensible variant). *)
let test_remote_e_constructor () =
  let _ : Irmin_lwt.remote = S.E () in
  let _ : Irmin.remote = S.E () in
  Lwt.return_unit

let test_dot_output () =
  let* repo = S.Repo.v (Irmin_lwt_mem.config ()) in
  let* t = S.main repo in
  let* () = S.set_exn t [ "a" ] "1" ~info:(fun () -> info "a") in
  let* () = S.set_exn t [ "b" ] "2" ~info:(fun () -> info "b") in
  let buf = Buffer.create 256 in
  let* () = Dot.output_buffer t ~date:Int64.to_string buf in
  let s = Buffer.contents buf in
  assert (String.length s > 0);
  (* Astring is not in scope here; do a substring search by hand. *)
  let contains needle =
    let nlen = String.length needle and slen = String.length s in
    let rec loop i =
      if i + nlen > slen then false
      else if String.sub s i nlen = needle then true
      else loop (i + 1)
    in
    loop 0
  in
  assert (contains "digraph");
  S.Repo.close repo

let run name f =
  Printf.printf "%-30s " name;
  flush stdout;
  Lwt_eio.Promise.await_lwt (f ());
  print_endline "ok"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Watch.scheduler needs an Eio switch to fork its background fiber; the
     hook is currently never set by Irmin itself. *)
  Irmin.Backend.Watch.set_watch_switch sw;
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  run "basic set/get" test_basic_set_get;
  run "branch and commit" test_branch_and_commit;
  run "sync between repos" test_sync_between_repos;
  run "remote E constructor" test_remote_e_constructor;
  run "dot output" test_dot_output;
  print_endline "--- running irmin-lwt-test harness ---";
  Lwt_eio.Promise.await_lwt
    (Irmin_lwt_test.Irmin_test.Store.run "irmin-lwt-mem" ~slow:false ~misc:[]
       ~sleep:Lwt_unix.sleep
       [ (`Quick, Test_mem.suite) ])
