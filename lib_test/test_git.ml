(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Test_common

let test_db = "test_db_git"

let init_disk () =
  if Filename.basename (Sys.getcwd ()) <> "lib_test" then
    failwith "The Git test should be run in the lib_test/ directory."
  else if Sys.file_exists test_db then
    Git_unix.FS.create ~root:test_db () >>= fun t ->
    Git_unix.FS.remove t
  else
    Lwt.return_unit

let init () =
  init_disk () >|= fun () ->
  Irmin_unix.set_listen_dir_hook ()

let clean () =
  Irmin.Private.Watch.(set_listen_dir_hook none);
  Lwt.return_unit

let suite k =
  let module S = (val git_store k) in
  {
    name   = "GIT" ^ string_of_contents k;
    cont   = k;
    kind   = `Git;
    init; clean;
    store  = (module S: Test_S);
    config =
      let head = Git.Reference.of_raw "refs/heads/test" in
      Irmin_git.config ~root:test_db ~head ~bare:true ()
  }

let test_non_bare () =
  let open Irmin_unix in
  init_disk () >>= fun () ->
  let module Store =
    Irmin_git.FS(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)
  in
  let config = Irmin_git.config ~root:test_db ~bare:false () in
  Store.Repo.create config >>= Store.master task >>= fun t ->
  Store.update (t "fst one") ["fst"] "ok" >>= fun () ->
  Store.update (t "snd one") ["fst"; "snd"] "maybe?" >>= fun () ->
  Store.update (t "fst one") ["fst"] "hoho"

let test_sort_order () =
  let module Memory =
    Irmin_unix.Irmin_git.Memory
      (Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)
  in
  Memory.Repo.create (Irmin_git.config ()) >>= fun repo ->
  let commit_t = Memory.Private.Repo.commit_t repo in
  let node_t = Memory.Private.Repo.node_t repo in
  let head_tree_id branch =
    Memory.head_exn branch >>= fun head ->
    Memory.Private.Commit.read_exn commit_t head >|= fun commit ->
    Memory.Private.Commit.Val.node commit
  in
  let ls branch =
    head_tree_id branch >>= fun tree_id ->
    Memory.Private.Node.read_exn node_t tree_id >|= fun tree ->
    Memory.Private.Node.Val.alist tree |> List.map fst
  in
  Memory.master (fun () -> Irmin.Task.empty) repo >>= fun master ->
  let master = master () in
  Memory.update master ["foo.c"] "foo.c" >>= fun () ->
  Memory.update master ["foo1"] "foo1" >>= fun () ->
  Memory.update master ["foo"; "foo.o"] "foo.o" >>= fun () ->
  ls master >>= fun items ->
  Alcotest.(check (list string)) "Sort order" ["foo.c"; "foo"; "foo1"] items;
  head_tree_id master >>= fun tree_id ->
  Alcotest.(check string) "Sort hash" "00c5f5e40e37fde61911f71373813c0b6cad1477"
    (Memory.Private.Node.Key.to_hum tree_id);
  (* Convert dir to file; changes order in listing *)
  Memory.update master ["foo"] "foo" >>= fun () ->
  ls master >>= fun items ->
  Alcotest.(check (list string)) "Sort order" ["foo"; "foo.c"; "foo1"] items;
  Lwt.return ()

let run f () = Lwt_main.run (f ())

let misc =
  "GIT.misc", [
    "Testing git non-bare repostiories", `Quick, run test_non_bare;
    "Testing sort order", `Quick, run test_sort_order;
  ]
