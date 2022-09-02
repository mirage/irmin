(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

let root = Filename.concat "_build" "test-readonly"
let src = Logs.Src.create "tests.readonly" ~doc:"Tests read-only stores"

module Log = (val Logs.src_log src : Logs.LOG)

let index_log_size = Some 1_000

module S = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)
end

let config ?(readonly = false) ?(fresh = true) root =
  Irmin_pack.config ~readonly ?index_log_size ~fresh root

let info () = S.Info.empty

let open_ro_after_rw_closed () =
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* t = S.main rw in
  let tree = S.Tree.singleton [ "a" ] "x" in
  S.set_tree_exn ~parents:[] ~info t [] tree >>= fun () ->
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  S.Repo.close rw >>= fun () ->
  let* t = S.main ro in
  let* c = S.Head.get t in
  S.Commit.of_hash ro (S.Commit.hash c) >>= function
  | None -> Alcotest.fail "no hash"
  | Some commit ->
      let tree = S.Commit.tree commit in
      let* x = S.Tree.find tree [ "a" ] in
      Alcotest.(check (option string)) "RO find" (Some "x") x;
      S.Repo.close ro

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

let ro_reload_after_add () =
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
  let tree = S.Tree.singleton [ "a" ] "x" in
  let* c1 = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  S.reload ro;
  check ro c1 "a" "x" >>= fun () ->
  let tree = S.Tree.singleton [ "a" ] "y" in
  let* c2 = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  check ro c1 "a" "x" >>= fun () ->
  let* () =
    S.Commit.of_hash ro (S.Commit.hash c2) >|= function
    | None -> ()
    | Some _ -> Alcotest.failf "should not find branch by"
  in
  S.reload ro;
  check ro c2 "a" "y" >>= fun () ->
  S.Repo.close ro >>= fun () -> S.Repo.close rw

let ro_reload_after_close () =
  let binding f = f [ "a" ] "x" in
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  let tree = binding (S.Tree.singleton ?metadata:None) in
  let* c1 = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  S.Repo.close rw >>= fun () ->
  S.reload ro;
  binding (check_binding ro c1) >>= fun () -> S.Repo.close ro

let ro_batch () =
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  Alcotest.check_raises_lwt "Read-only store throws RO_not_allowed exception"
    Irmin_pack_unix.Errors.RO_not_allowed (fun () ->
      S.Backend.Repo.batch ro (fun _ _ _ -> Lwt.return_unit))
  >>= fun () ->
  S.Repo.close ro >>= fun () -> S.Repo.close rw

let tests =
  let tc name test = Alcotest_lwt.test_case name `Quick (fun _switch -> test) in
  [
    tc "Test open ro after rw closed" open_ro_after_rw_closed;
    tc "Test ro reload after add" ro_reload_after_add;
    tc "Test ro reload after close" ro_reload_after_close;
    tc "Test ro batch" ro_batch;
  ]
