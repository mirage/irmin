(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

open! Import
open Common

let root = Filename.concat "_build" "test-instances"
let src = Logs.Src.create "tests.instances" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

let index_log_size = Some 1_000

module Conf = struct
  let entries = 32
  let stable_hash = 256
end

module Hash = Irmin.Hash.SHA1

module S = struct
  module P = Irmin.Path.String_list
  module M = Irmin.Metadata.None
  module XNode = Irmin.Private.Node.Make
  module XCommit = Irmin.Private.Commit

  module Maker =
    Irmin_pack.Maker_ext (Irmin_pack.Version.V2) (Conf) (XNode) (XCommit)

  include
    Maker.Make (M) (Irmin.Contents.String) (P) (Irmin.Branch.String) (Hash)
end

let config ?(readonly = false) ?(fresh = true) root =
  Irmin_pack.config ~readonly ?index_log_size ~fresh root

let info () = S.Info.empty

let open_ro_after_rw_closed () =
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* t = S.master rw in
  let* tree = S.Tree.add S.Tree.empty [ "a" ] "x" in
  S.set_tree_exn ~parents:[] ~info t [] tree >>= fun () ->
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  S.Repo.close rw >>= fun () ->
  let* t = S.master ro in
  let* c = S.Head.get t in
  S.Commit.of_hash ro (S.Commit.hash c) >>= function
  | None -> Alcotest.fail "no hash"
  | Some commit ->
      let tree = S.Commit.tree commit in
      let* x = S.Tree.find tree [ "a" ] in
      Alcotest.(check (option string)) "RO find" (Some "x") x;
      S.Repo.close ro

let check_commit_absent repo commit =
  S.Commit.of_hash repo (S.Commit.hash commit) >|= function
  | None -> ()
  | Some _ -> Alcotest.fail "should not find hash"

let check_binding ?msg repo commit key value =
  let msg =
    match msg with
    | Some m -> m
    | None ->
        Fmt.str "Expected binding [%a ↦ %s]" Fmt.(Dump.list string) key value
  in
  S.Commit.of_hash repo (S.Commit.hash commit) >>= function
  | None -> Alcotest.failf "commit not found"
  | Some commit ->
      let tree = S.Commit.tree commit in
      let+ x = S.Tree.find tree key in
      Alcotest.(check (option string)) msg (Some value) x

let ro_sync_after_add () =
  let check ro c k v =
    S.Commit.of_hash ro (S.Commit.hash c) >>= function
    | None -> Alcotest.failf "commit not found"
    | Some commit ->
        let tree = S.Commit.tree commit in
        let+ x = S.Tree.find tree [ k ] in
        Alcotest.(check (option string)) "RO find" (Some v) x
  in
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  let* tree = S.Tree.add S.Tree.empty [ "a" ] "x" in
  let* c1 = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  S.sync ro;
  check ro c1 "a" "x" >>= fun () ->
  let* tree = S.Tree.add S.Tree.empty [ "a" ] "y" in
  let* c2 = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  check ro c1 "a" "x" >>= fun () ->
  let* () =
    S.Commit.of_hash ro (S.Commit.hash c2) >|= function
    | None -> ()
    | Some _ -> Alcotest.failf "should not find branch by"
  in
  S.sync ro;
  check ro c2 "a" "y" >>= fun () ->
  S.Repo.close ro >>= fun () -> S.Repo.close rw

let ro_sync_after_close () =
  let binding f = f [ "a" ] "x" in
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  let* tree = binding (S.Tree.add S.Tree.empty ?metadata:None) in
  let* c1 = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  S.Repo.close rw >>= fun () ->
  S.sync ro;
  binding (check_binding ro c1) >>= fun () -> S.Repo.close ro

module P = S.Private

let clear_all repo =
  Log.debug (fun l -> l "clear repo");
  Lwt.join
    [
      P.Contents.clear (P.Repo.contents_t repo);
      P.Branch.clear (P.Repo.branch_t repo);
      P.Commit.clear (P.Repo.commit_t repo);
      P.Node.clear (P.Repo.node_t repo);
    ]

(** Open RO after RW was cleared. *)
let clear_rw_open_ro () =
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* tree = S.Tree.add S.Tree.empty [ "a" ] "x" in
  let* c = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  clear_all rw >>= fun () ->
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  check_commit_absent ro c >>= fun () ->
  S.Repo.close rw >>= fun () -> S.Repo.close ro

(** RO looks for values before and after sync but after RW was cleared. *)
let clear_rw_find_ro () =
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  let* tree = S.Tree.add S.Tree.empty [ "a" ] "x" in
  let* c1 = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  S.sync ro;
  check_binding ro c1 ~msg:"RO finds value" [ "a" ] "x" >>= fun () ->
  clear_all rw >>= fun () ->
  let* tree = S.Tree.add S.Tree.empty [ "b" ] "y" in
  let* c2 = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  let* () =
    check_binding ro c1 ~msg:"RO finds value after clear but before sync"
      [ "a" ] "x"
  in
  S.sync ro;
  let* () =
    check_binding ro c2 ~msg:"RO finds value added after clear" [ "b" ] "y"
  in
  check_commit_absent ro c1 >>= fun () ->
  S.Repo.close rw >>= fun () -> S.Repo.close ro

let clear_rw_twice () =
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* t = S.master rw in
  let check_empty () =
    S.Head.find t >|= function
    | None -> ()
    | Some _ -> Alcotest.fail "should be empty"
  in
  let add () =
    check_empty () >>= fun () ->
    let* tree = S.Tree.add S.Tree.empty [ "a" ] "x" in
    S.set_tree_exn ~parents:[] ~info t [] tree
  in
  let add_after_clear () =
    add () >>= fun () ->
    let* c = S.Head.get t in
    check_binding rw c ~msg:"RW finds value added after clear" [ "a" ] "x"
  in
  add () >>= fun () ->
  clear_all rw >>= fun () ->
  add_after_clear () >>= fun () ->
  clear_all rw >>= fun () ->
  check_empty () >>= fun () -> S.Repo.close rw

let dict_sync_after_clear_same_offset () =
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  let find_key hash key value =
    S.Commit.of_hash ro hash >>= function
    | None -> Alcotest.fail "no hash"
    | Some commit ->
        let tree = S.Commit.tree commit in
        let+ x = S.Tree.find tree [ key ] in
        Alcotest.(check (option string)) "RO find" (Some value) x
  in
  let long_string = random_string 200 in
  let* tree = S.Tree.add S.Tree.empty [ long_string ] "x" in
  let* c = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  let h = S.Commit.hash c in
  S.sync ro;
  find_key h long_string "x" >>= fun () ->
  clear_all rw >>= fun () ->
  let long_string = random_string 200 in
  let* tree = S.Tree.add S.Tree.empty [ long_string ] "y" in
  let* c = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  let h = S.Commit.hash c in
  S.sync ro;
  find_key h long_string "y" >>= fun () ->
  S.Repo.close rw >>= fun () -> S.Repo.close ro

let tests =
  let tc name test =
    Alcotest.test_case name `Quick (fun () -> Lwt_main.run (test ()))
  in
  [
    tc "Test open ro after rw closed" open_ro_after_rw_closed;
    tc "Open ro after rw cleared" clear_rw_open_ro;
    tc "Clear rw twice" clear_rw_twice;
    tc "Find in ro after rw cleared" clear_rw_find_ro;
    tc "Test ro sync after add" ro_sync_after_add;
    tc "Test ro sync after close" ro_sync_after_close;
    tc "RO sync dict after clear, same offset" dict_sync_after_clear_same_offset;
  ]
