(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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

let src = Logs.Src.create "tests.gc" ~doc:"Test gc"

module Log = (val Logs.src_log src : Logs.LOG)

let test_dir = "_build"

let fresh_name =
  let c = ref 0 in
  fun () ->
    incr c;
    let name = Filename.concat test_dir ("test-gc" ^ string_of_int !c) in
    name

include struct
  module S = struct
    module Maker = Irmin_pack_unix.Maker (Conf)
    include Maker.Make (Schema)
  end

  type t = {
    root : string;
    repo : S.Repo.t;
    parents : S.Commit.t list;
    tree : S.tree;
  }

  let config ~readonly ~fresh root =
    Irmin_pack.config ~readonly
      ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal ~lru_size:0 ~fresh
      root

  let info = S.Info.empty

  let gc ?unlink t commit =
    let commit_key = S.Commit.key commit in
    S.gc ?unlink t.repo commit_key

  let wait_for_gc _ =
    (* TODO for testing purposes its convenient to have this function. *)
    Lwt.return_unit

  let commit t =
    let parents = List.map S.Commit.key t.parents in
    let+ h = S.Commit.v t.repo ~info ~parents t.tree in
    S.Tree.clear t.tree;
    h

  let set t key data =
    let* tree = S.Tree.add t.tree key data in
    Lwt.return { t with tree }

  let del t key =
    let* tree = S.Tree.remove t.tree key in
    Lwt.return { t with tree }

  let checkout t key =
    let* c = S.Commit.of_hash t.repo (S.Commit.hash key) in
    match c with
    | None -> Lwt.return_none
    | Some commit ->
        let tree = S.Commit.tree commit in
        Lwt.return_some { t with tree; parents = [ commit ] }

  let checkout_exn t key =
    let* o = checkout t key in
    match o with None -> Lwt.fail Not_found | Some p -> Lwt.return p

  let init ?(readonly = false) ?(fresh = true) ?root () =
    (* start with a clean dir if fresh *)
    let root = Option.value root ~default:(fresh_name ()) in
    if fresh then rm_dir root;
    let+ repo = S.Repo.v (config ~readonly ~fresh root) in
    let tree = S.Tree.empty () in
    { root; repo; tree; parents = [] }
end

(** Predefined commits. *)
let commit_1 t =
  let* t = set t [ "a"; "b" ] "Novembre" in
  let* t = set t [ "a"; "c" ] "Juin" in
  let+ h = commit t in
  (t, h)

let commit_2 t =
  let* t = set t [ "a"; "d" ] "Mars" in
  let+ h = commit t in
  (t, h)

let commit_3 t =
  let* t = set t [ "a"; "f" ] "Fevrier" in
  let+ h = commit t in
  (t, h)

let commit_4 t =
  let* t = set t [ "a"; "e" ] "Mars" in
  let+ h = commit t in
  (t, h)

let commit_5 t =
  let* t = set t [ "e"; "a" ] "Avril" in
  let+ h = commit t in
  (t, h)

let commit_del t =
  let* t = del t [ "a"; "c" ] in
  let+ h = commit t in
  (t, h)

(** Wrappers for testing. *)
let check_blob tree key expected =
  let+ got = S.Tree.find tree key in
  Alcotest.(check (option string)) "find blob" (Some expected) got

let check_none tree key =
  let+ got = S.Tree.find tree key in
  Alcotest.(check (option string)) "blob not found" None got

let check_tree_1 tree =
  let* () = check_blob tree [ "a"; "b" ] "Novembre" in
  check_blob tree [ "a"; "c" ] "Juin"

let check_1 t c =
  S.Commit.of_key t.repo (S.Commit.key c) >>= function
  | None -> Alcotest.fail "no hash found in repo"
  | Some commit ->
      let tree = S.Commit.tree commit in
      check_tree_1 tree

let check_2 t c =
  S.Commit.of_key t.repo (S.Commit.key c) >>= function
  | None -> Alcotest.fail "no hash found in repo"
  | Some commit ->
      let tree = S.Commit.tree commit in
      let* () = check_blob tree [ "a"; "d" ] "Mars" in
      (* c2 always contains c1 tree in tests *)
      check_tree_1 tree

let check_3 t c =
  S.Commit.of_key t.repo (S.Commit.key c) >>= function
  | None -> Alcotest.fail "no hash found in repo"
  | Some commit ->
      let tree = S.Commit.tree commit in
      check_blob tree [ "a"; "f" ] "Fevrier"

let check_4 t c =
  S.Commit.of_key t.repo (S.Commit.key c) >>= function
  | None -> Alcotest.fail "no hash found in repo"
  | Some commit ->
      let tree = S.Commit.tree commit in
      check_blob tree [ "a"; "e" ] "Mars"

let check_5 t c =
  S.Commit.of_key t.repo (S.Commit.key c) >>= function
  | None -> Alcotest.fail "no hash found in repo"
  | Some commit ->
      let tree = S.Commit.tree commit in
      let* () = check_blob tree [ "e"; "a" ] "Avril" in
      (* c5 always contains c1 and c4 trees in tests *)
      let* () = check_tree_1 tree in
      check_blob tree [ "a"; "e" ] "Mars"

let check_del_1 t c =
  S.Commit.of_key t.repo (S.Commit.key c) >>= function
  | None -> Alcotest.fail "no hash found in repo"
  | Some commit ->
      let tree = S.Commit.tree commit in
      check_none tree [ "a"; "c" ]

let check_not_found t key msg =
  let* c = S.Commit.of_hash t.repo (S.Commit.hash key) in
  match c with
  | None -> Lwt.return_unit
  | Some _ -> Alcotest.failf "should not find %s" msg

module Blocking_gc = struct
  (** Check that gc preserves and deletes commits accordingly. *)
  let one_gc () =
    (* c1 - c2            *)
    (*   \---- c3         *)
    (*            gc(c3)  *)
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let* t = checkout_exn t c1 in
    let* t, c3 = commit_3 t in
    [%log.debug "Gc c1, c2, keep c3"];
    let () = gc t c3 in
    let* () = wait_for_gc t in
    (* TODO: parent of commit is not gced. *)
    (* let* () = check_not_found t c1 "removed c1" in *)
    let* () = check_not_found t c2 "removed c2" in
    let* () = check_3 t c3 in
    S.Repo.close t.repo

  (** Check that calling gc twice works. *)
  let two_gc () =
    (*                gc(c4)      gc(c5) *)
    (* c1 - c2 --- c4 -------- c5        *)
    (*   \---- c3                        *)
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let* t = checkout_exn t c1 in
    let* t, c3 = commit_3 t in
    let* t = checkout_exn t c2 in
    let* t, c4 = commit_4 t in
    [%log.debug "Gc c1, c2, c3, keep c4"];
    let () = gc t c4 in
    let* () = wait_for_gc t in
    let* t = checkout_exn t c4 in
    let* t, c5 = commit_5 t in
    let* () = check_5 t c5 in
    [%log.debug "Gc c4, keep c5"];
    let () = gc t c5 in
    let* () = wait_for_gc t in
    let* () = check_5 t c5 in
    let* () = check_not_found t c1 "removed c1" in
    let* () = check_not_found t c2 "removed c2" in
    let* () = check_not_found t c3 "removed c3" in
    (* TODO: parent of commit is not gced. *)
    (* let* () = check_not_found t c2a "removed c2a" in *)
    S.Repo.close t.repo

  (** Check that calling gc on first commit of chain keeps everything. *)
  let gc_keeps_all () =
    (* c1 - c2 - c3        *)
    (*              gc(c1) *)
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let* t = checkout_exn t c2 in
    let* t, c3 = commit_3 t in
    [%log.debug "Keep c1, c2, c3"];
    let () = gc t c1 in
    let* () = wait_for_gc t in
    let* () = check_1 t c1 in
    let* () = check_2 t c2 in
    let* () = check_3 t c3 in
    S.Repo.close t.repo

  (** Check that adding back gced commits works. *)
  let gc_add_back () =
    (* c1 - c_del - c3 ------ c1 - c2 ------- c3 *)
    (*                 gc(c3)         gc(c1)     *)
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c_del = commit_del t in
    let* t = checkout_exn t c_del in
    let* t, c3 = commit_3 t in
    [%log.debug "Gc c1, c_del, keep c3"];
    let () = gc t c3 in
    let* () = wait_for_gc t in
    let* () = check_not_found t c1 "removed c1" in
    (* TODO: parent of commit is not gced. *)
    (* let* () = check_not_found t c_del "removed c_del" in *)
    let* () = check_3 t c3 in
    let* () = check_del_1 t c3 in
    [%log.debug "Add back c1"];
    let* t = checkout_exn t c3 in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* () = check_1 t c1 in
    let* t, c2 = commit_2 t in
    let* () = check_2 t c2 in
    [%log.debug "Gc c3, keep c1, c2"];
    let () = gc t c1 in
    let* () = wait_for_gc t in
    (* TODO: parent of commit is not gced. *)
    (* let* () = check_not_found t c3 "removed c3" in *)
    let* () = check_2 t c2 in
    [%log.debug "Add back c3"];
    let* t = checkout_exn t c2 in
    let* t, c3 = commit_3 t in
    let* () = check_3 t c2 in
    let* () = check_3 t c3 in
    S.Repo.close t.repo

  (** Check that gc and close work together. *)
  let close () =
    (* c1 ------ c2                        *)
    (*    gc(c1)               gc(c2)      *)
    (*             close close       close *)
    let* t = init () in
    let store_name = t.root in
    let* t, c1 = commit_1 t in
    let () = gc t c1 in
    let* () = wait_for_gc t.repo in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let* () = S.Repo.close t.repo in
    let* t = init ~readonly:false ~fresh:false ~root:store_name () in
    let* () = check_1 t c1 in
    let* () = check_2 t c2 in
    let* () = S.Repo.close t.repo in
    let* t = init ~readonly:false ~fresh:false ~root:store_name () in
    [%log.debug "Gc c1, keep c2"];
    let () = gc t c2 in
    let* () = S.Repo.close t.repo in
    let* t = init ~readonly:false ~fresh:false ~root:store_name () in
    (* TODO: parent of commit is not gced. *)
    (* let* () = check_not_found t c1 "removed c1" in *)
    let* () = check_2 t c2 in
    S.Repo.close t.repo

  (** Check that gc works on a commit with two parents. *)
  let gc_commit_with_two_parents () =
    (*         gc(c3) *)
    (* c1 - c3        *)
    (* c2 -/          *)
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let t = { t with parents = [ c1; c2 ] } in
    let* t, c3 = commit_3 t in
    let () = gc t c3 in
    let* () = wait_for_gc t in
    (* TODO: parent of commit is not gced. *)
    (* let* () = check_not_found t c1 "removed c1" in *)
    (* let* () = check_not_found t c2a "removed c2a" in *)
    let* () = check_3 t c3 in
    S.Repo.close t.repo

  (** Check that gc preserves and deletes commits from RO. *)
  let gc_ro () =
    (* c1 ---- c3 ------------------- c4 - c5                     *)
    (*   \- c2                                                    *)
    (*                  gc(c3)                      gc(c4)        *)
    (*           reload       reload         reload        reload *)
    let* t = init () in
    let* ro_t = init ~readonly:true ~fresh:false ~root:t.root () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let* t = checkout_exn t c1 in
    let* t, c3 = commit_3 t in
    S.reload ro_t.repo;
    [%log.debug "Gc c1, c2, keeps c3"];
    let () = gc t c3 in
    let* () = wait_for_gc t.repo in
    [%log.debug "RO finds everything before reload"];
    let* () = check_1 ro_t c1 in
    let* () = check_2 ro_t c2 in
    let* () = check_3 ro_t c3 in
    S.reload ro_t.repo;
    [%log.debug "RO does not find gced commits after reload"];
    let* () = check_3 ro_t c3 in
    (* TODO: parent of commit is not gced. *)
    (* check_not_found ro_t c1 "c1" >>= fun () -> *)
    let* () = check_not_found ro_t c2 "c2" in
    let* t = checkout_exn t c3 in
    let* t, c4 = commit_4 t in
    let* t = checkout_exn t c4 in
    let* t, c5 = commit_5 t in
    S.reload ro_t.repo;
    [%log.debug "Gc c3, keep c4, c5"];
    let () = gc t c4 in
    let* () = wait_for_gc t.repo in
    [%log.debug "RO finds c3, c4, c5 before reload"];
    let* () = check_3 ro_t c3 in
    let* () = check_4 ro_t c4 in
    let* () = check_5 ro_t c5 in
    S.reload ro_t.repo;
    [%log.debug "RO finds c4, c5 but not c3 after reload"];
    let* () = check_4 ro_t c4 in
    let* () = check_5 ro_t c5 in
    (* TODO: parent of commit is not gced. *)
    (* check_not_found ro_t c3 "c3" >>= fun () -> *)
    let* () = S.Repo.close t.repo in
    S.Repo.close ro_t.repo

  (** Check that RO works if reload is called after two gcs. *)
  let ro_after_two_gc () =
    (* c1 ------- c2               *)
    (*    gc(c1)     gc(c2)        *)
    (*                      reload *)
    let* t = init () in
    let* ro_t = init ~readonly:true ~fresh:false ~root:t.root () in
    let* t, c1 = commit_1 t in
    S.reload ro_t.repo;
    let () = gc t c1 in
    let* () = wait_for_gc t.repo in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let () = gc t c2 in
    let* () = wait_for_gc t.repo in
    [%log.debug "RO finds c1, but not c2 before reload"];
    let* () = check_1 ro_t c1 in
    let* () = check_not_found ro_t c2 "c2" in
    [%log.debug "RO finds c2, but not c1 after reload"];
    S.reload ro_t.repo;
    let* () = check_2 ro_t c2 in
    (* TODO: parent of commit is not gced. *)
    (* check_not_found ro_t c1 "c1" >>= fun () -> *)
    let* () = S.Repo.close t.repo in
    S.Repo.close ro_t.repo

  (** Check that gc and close and ro work together. *)
  let ro_close () =
    let* t = init () in
    let* ro_t = init ~readonly:true ~fresh:false ~root:t.root () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let* () = S.Repo.close ro_t.repo in
    let () = gc t c2 in
    let* () = wait_for_gc t.repo in
    [%log.debug "RO reopens is similar to a reload"];
    let* ro_t = init ~readonly:true ~fresh:false ~root:t.root () in
    let* () = check_2 ro_t c2 in
    (* TODO: parent of commit is not gced. *)
    (* check_not_found ro_t c1 "removed c1" >>= fun () -> *)
    let* t = checkout_exn t c2 in
    let* t, c3 = commit_3 t in
    S.reload ro_t.repo;
    let* () = check_3 t c3 in
    let* () = check_3 ro_t c3 in
    (* TODO: parent of commit is not gced. *)
    (* check_not_found ro_t c1 "removed c1" >>= fun () -> *)
    let* () = S.Repo.close t.repo in
    S.Repo.close ro_t.repo

  (** Check opening RO store and calling reload right after. *)
  let ro_reload_after_v () =
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* ro_t = init ~readonly:true ~fresh:false ~root:t.root () in
    S.reload ro_t.repo;
    let* () = check_1 ro_t c1 in
    let* () = S.Repo.close t.repo in
    S.Repo.close ro_t.repo

  let tests =
    let tc name f =
      Alcotest.test_case name `Quick (fun () -> Lwt_main.run (f ()))
    in

    [
      tc "Test one gc" one_gc;
      tc "Test twice gc" two_gc;
      tc "Test gc keeps commits" gc_keeps_all;
      tc "Test adding back commits" gc_add_back;
      tc "Test close" close;
      tc "Test gc commit with two parents" gc_commit_with_two_parents;
      tc "Test gc ro" gc_ro;
      tc "Test reload after two gc" ro_after_two_gc;
      tc "Test ro close" ro_close;
      tc "Test ro reload after open" ro_reload_after_v;
    ]
end

module Concurrent_gc = struct
  let tests = []
end
