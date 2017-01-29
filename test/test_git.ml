(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let config =
  let head = Git.Reference.of_raw "refs/heads/test" in
  Irmin_git.config ~root:test_db ~head ~bare:true ()

module Memory = Irmin_git.Memory (Git_unix.Sync.IO) (Git_unix.Zlib)
    (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)

let store = (module Memory: Test_S)
let stats = None
let init () = Memory.Git_mem.clear_all (); Lwt.return_unit
let clean () = Lwt.return_unit
let suite = { name = "GIT"; kind = `Git; clean; init; store; stats; config }

let get = function
  | Some x -> x
  | None   -> Alcotest.fail "get"

let test_sort_order (module S: Test_S) =
  S.Repo.v (Irmin_git.config ()) >>= fun repo ->
  let commit_t = S.Private.Repo.commit_t repo in
  let node_t = S.Private.Repo.node_t repo in
  let head_tree_id branch =
    S.Head.get branch >>= fun head ->
    S.Private.Commit.find commit_t head >|= fun commit ->
    S.Private.Commit.Val.node (get commit)
  in
  let ls branch =
    head_tree_id branch >>= fun tree_id ->
    S.Private.Node.find node_t tree_id >|= fun tree ->
    S.Private.Node.Val.list (get tree) |> List.map fst
  in
  let nope = Irmin.Task.empty in
  S.master repo >>= fun master ->
  S.set master nope ["foo.c"] "foo.c" >>= fun () ->
  S.set master nope ["foo1"] "foo1" >>= fun () ->
  S.set master nope ["foo"; "foo.o"] "foo.o" >>= fun () ->
  ls master >>= fun items ->
  Alcotest.(check (list string)) "Sort order" ["foo.c"; "foo"; "foo1"] items;
  head_tree_id master >>= fun tree_id ->
  Alcotest.(check string) "Sort hash" "00c5f5e40e37fde61911f71373813c0b6cad1477"
    (Fmt.to_to_string S.Private.Node.Key.pp tree_id);
  (* Convert dir to file; changes order in listing *)
  S.set master nope ["foo"] "foo" >>= fun () ->
  ls master >>= fun items ->
  Alcotest.(check (list string)) "Sort order" ["foo"; "foo.c"; "foo1"] items;
  Lwt.return ()

let test_sort_order store () = Lwt_main.run (test_sort_order store)
