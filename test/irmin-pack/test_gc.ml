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

let tc name f = Alcotest_lwt.test_case name `Quick (fun _switch () -> f ())

module Store = struct
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

  let config ~lru_size ~readonly ~fresh root =
    Irmin_pack.config ~readonly
      ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal ~fresh ~lru_size
      root

  let info = S.Info.empty

  let start_gc ?(unlink = false) t commit =
    let commit_key = S.Commit.key commit in
    let* _launched = S.Gc.start_exn ~unlink t.repo commit_key in
    Lwt.return_unit

  let finalise_gc t =
    let* result = S.Gc.finalise_exn ~wait:true t.repo in
    match result with
    | `Running -> Alcotest.fail "expected finalised gc"
    (* consider `Idle as success because gc can finalise during commit as well *)
    | `Idle | `Finalised _ -> Lwt.return_unit

  let commit ?(info = info) t =
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

  let init ?(lru_size = 0) ?(readonly = false) ?(fresh = true) ?root () =
    (* start with a clean dir if fresh *)
    let root = Option.value root ~default:(fresh_name ()) in
    if fresh then rm_dir root;
    let+ repo = S.Repo.v (config ~readonly ~fresh ~lru_size root) in
    let tree = S.Tree.empty () in
    { root; repo; tree; parents = [] }

  let config root = config ~lru_size:0 ~readonly:false ~fresh:true root

  let init_with_config config =
    let+ repo = S.Repo.v config in
    let root = Irmin_pack.Conf.root config in
    let tree = S.Tree.empty () in
    { root; repo; tree; parents = [] }

  let close t = S.Repo.close t.repo

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

  let commit_1_different_author t =
    let info = S.Info.v ~author:"someone" Int64.zero in
    let* t = set t [ "a"; "b" ] "Novembre" in
    let* t = set t [ "a"; "c" ] "Juin" in
    let+ h = commit ~info t in
    (t, h)
end

include Store

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
  | None -> Alcotest.fail "no hash found in repo for check_1"
  | Some commit ->
      let tree = S.Commit.tree commit in
      check_tree_1 tree

let check_2 t c =
  S.Commit.of_key t.repo (S.Commit.key c) >>= function
  | None -> Alcotest.fail "no hash found in repo for check_2"
  | Some commit ->
      let tree = S.Commit.tree commit in
      let* () = check_blob tree [ "a"; "d" ] "Mars" in
      (* c2 always contains c1 tree in tests *)
      check_tree_1 tree

let check_3 t c =
  S.Commit.of_key t.repo (S.Commit.key c) >>= function
  | None -> Alcotest.fail "no hash found in repo for check_3"
  | Some commit ->
      let tree = S.Commit.tree commit in
      check_blob tree [ "a"; "f" ] "Fevrier"

let check_4 t c =
  S.Commit.of_key t.repo (S.Commit.key c) >>= function
  | None -> Alcotest.fail "no hash found in repo for check_4"
  | Some commit ->
      let tree = S.Commit.tree commit in
      check_blob tree [ "a"; "e" ] "Mars"

let check_5 t c =
  S.Commit.of_key t.repo (S.Commit.key c) >>= function
  | None -> Alcotest.fail "no hash found in repo for check_5"
  | Some commit ->
      let tree = S.Commit.tree commit in
      let* () = check_blob tree [ "e"; "a" ] "Avril" in
      (* c5 always contains c1 and c4 trees in tests *)
      let* () = check_tree_1 tree in
      check_blob tree [ "a"; "e" ] "Mars"

let check_del_1 t c =
  S.Commit.of_key t.repo (S.Commit.key c) >>= function
  | None -> Alcotest.fail "no hash found in repo for check_del_1"
  | Some commit ->
      let tree = S.Commit.tree commit in
      check_none tree [ "a"; "c" ]

let check_not_found t key msg =
  let* c = S.Commit.of_hash t.repo (S.Commit.hash key) in
  match c with
  | None -> Lwt.return_unit
  | Some _ -> Alcotest.failf "should not find %s" msg

module Gc = struct
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
    let* () = start_gc t c3 in
    let* () = finalise_gc t in
    let* () = check_not_found t c1 "removed c1" in
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
    let* () = start_gc t c4 in
    let* () = finalise_gc t in
    let* t = checkout_exn t c4 in
    let* t, c5 = commit_5 t in
    let* () = check_5 t c5 in
    [%log.debug "Gc c4, keep c5"];
    let* () = start_gc t c5 in
    let* () = finalise_gc t in
    let* () = check_5 t c5 in
    let* () = check_not_found t c1 "removed c1" in
    let* () = check_not_found t c2 "removed c2" in
    let* () = check_not_found t c3 "removed c3" in
    let* () = check_not_found t c4 "removed c4" in
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
    let* () = start_gc t c1 in
    let* () = finalise_gc t in
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
    let* () = start_gc t c3 in
    let* () = finalise_gc t in
    let* () = check_not_found t c1 "removed c1" in
    let* () = check_not_found t c_del "removed c_del" in
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
    let* () = start_gc t c1 in
    let* () = finalise_gc t in
    let* () = check_not_found t c3 "removed c3" in
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
    let* () = start_gc ~unlink:false t c1 in
    let* () = finalise_gc t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let* () = S.Repo.close t.repo in
    Alcotest.(check bool)
      "unlink:false" true
      (Sys.file_exists (Filename.concat store_name "store.0.suffix"));
    let* t = init ~readonly:true ~fresh:false ~root:store_name () in
    let* () = S.Repo.close t.repo in
    Alcotest.(check bool)
      "RO no clean up" true
      (Sys.file_exists (Filename.concat store_name "store.0.suffix"));
    let* t = init ~readonly:false ~fresh:false ~root:store_name () in
    let* () = S.Repo.close t.repo in
    Alcotest.(check bool)
      "RW cleaned up" false
      (Sys.file_exists (Filename.concat store_name "store.0.suffix"));
    let* t = init ~readonly:false ~fresh:false ~root:store_name () in
    let* () = check_1 t c1 in
    let* () = check_2 t c2 in
    let* () = S.Repo.close t.repo in
    let* t = init ~readonly:false ~fresh:false ~root:store_name () in
    [%log.debug "Gc c1, keep c2"];
    let* () = start_gc ~unlink:true t c2 in
    let* () = finalise_gc t in
    let* () = S.Repo.close t.repo in
    Alcotest.(check bool)
      "unlink:true" false
      (Sys.file_exists (Filename.concat store_name "store.1.suffix"));
    let* t = init ~readonly:false ~fresh:false ~root:store_name () in
    let* () = check_not_found t c1 "removed c1" in
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
    let* () = start_gc t c3 in
    let* () = finalise_gc t in
    let* () = check_not_found t c1 "removed c1" in
    let* () = check_not_found t c2 "removed c2" in
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
    let* () = start_gc t c3 in
    let* () = finalise_gc t in
    [%log.debug "RO finds everything before reload"];
    let* () = check_1 ro_t c1 in
    let* () = check_2 ro_t c2 in
    let* () = check_3 ro_t c3 in
    S.reload ro_t.repo;
    [%log.debug "RO does not find gced commits after reload"];
    let* () = check_3 ro_t c3 in
    let* () = check_not_found ro_t c1 "c1" in
    let* () = check_not_found ro_t c2 "c2" in
    let* t = checkout_exn t c3 in
    let* t, c4 = commit_4 t in
    let* t = checkout_exn t c4 in
    let* t, c5 = commit_5 t in
    S.reload ro_t.repo;
    [%log.debug "Gc c3, keep c4, c5"];
    let* () = start_gc t c4 in
    let* () = finalise_gc t in
    [%log.debug "RO finds c3, c4, c5 before reload"];
    let* () = check_3 ro_t c3 in
    let* () = check_4 ro_t c4 in
    let* () = check_5 ro_t c5 in
    S.reload ro_t.repo;
    [%log.debug "RO finds c4, c5 but not c3 after reload"];
    let* () = check_4 ro_t c4 in
    let* () = check_5 ro_t c5 in
    let* () = check_not_found ro_t c3 "c3" in
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
    let* () = start_gc t c1 in
    let* () = finalise_gc t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let* () = start_gc t c2 in
    let* () = finalise_gc t in
    [%log.debug "RO finds c1, but not c2 before reload"];
    let* () = check_1 ro_t c1 in
    let* () = check_not_found ro_t c2 "c2" in
    [%log.debug "RO finds c2, but not c1 after reload"];
    S.reload ro_t.repo;
    let* () = check_2 ro_t c2 in
    let* () = check_not_found ro_t c1 "c1" in
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
    let* () = start_gc t c2 in
    let* () = finalise_gc t in
    [%log.debug "RO reopens is similar to a reload"];
    let* ro_t = init ~readonly:true ~fresh:false ~root:t.root () in
    let* () = check_2 ro_t c2 in
    let* () = check_not_found ro_t c1 "removed c1" in
    let* t = checkout_exn t c2 in
    let* t, c3 = commit_3 t in
    S.reload ro_t.repo;
    let* () = check_3 t c3 in
    let* () = check_3 ro_t c3 in
    let* () = check_not_found ro_t c1 "removed c1" in
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

  (** Check that gc works when the lru caches some objects that are delete by
      consequent commits. See https://github.com/mirage/irmin/issues/1920. *)
  let gc_lru () =
    let check t c =
      S.Commit.of_key t.repo (S.Commit.key c) >>= function
      | None -> Alcotest.fail "no hash found in repo"
      | Some commit ->
          let tree = S.Commit.tree commit in
          check_blob tree [ "a"; "b"; "c" ] "b"
    in
    let* t = init ~lru_size:100 () in
    let* t = set t [ "a"; "b"; "c" ] "b" in
    let* c1 = commit t in
    let* t = checkout_exn t c1 in
    let* t = set t [ "a"; "d"; "c" ] "b" in
    let* c2 = commit t in
    let* t = checkout_exn t c2 in
    let* t = del t [ "a"; "d"; "c" ] in
    let* c3 = commit t in
    let* t = checkout_exn t c3 in
    let* t = set t [ "a"; "b"; "e" ] "a" in
    let* c4 = commit t in
    let* () = start_gc t c3 in
    let* () = finalise_gc t in
    let* () = check t c4 in
    S.Repo.close t.repo

  (** Check that calling gc during a batch raises an error. *)
  let gc_during_batch () =
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* _ =
      Alcotest.check_raises_lwt "Should not call gc in batch"
        (Irmin_pack_unix.Errors.Pack_error `Gc_forbidden_during_batch)
        (fun () ->
          S.Backend.Repo.batch t.repo (fun _ _ _ ->
              let* () = start_gc t c1 in
              finalise_gc t))
    in
    S.Repo.close t.repo

  (** Add back commits after they were gced. *)
  let add_back_gced_commit () =
    (* c1 - c2 - c3                *)
    (*              gc(c3)         *)
    (*                     c1 - c2 *)
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let* t = checkout_exn t c2 in
    let* t, c3 = commit_3 t in
    [%log.debug "Keep c3 remove c1 c2"];
    let* () = start_gc t c3 in
    let* () = finalise_gc t in
    let* () = check_not_found t c1 "removed c1" in
    let* () = check_not_found t c2 "removed c2" in
    let* t, c1_again =
      commit_1 { t with tree = S.Tree.empty (); parents = [] }
    in
    Alcotest.check_repr S.Hash.t "added commit has the same hash as gced one"
      (S.Commit.hash c1_again) (S.Commit.hash c1);
    let* () = check_1 t c1_again in
    let* t = checkout_exn t c1_again in
    let* t, c2_again = commit_2 t in
    Alcotest.check_repr S.Hash.t "added commit has the same hash as gced one"
      (S.Commit.hash c2_again) (S.Commit.hash c2);
    let* () = check_2 t c2_again in
    let* () = check_3 t c3 in
    S.Repo.close t.repo

  let gc_similar_commits () =
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* () = start_gc t c1 in
    let* () = finalise_gc t in
    let* t = checkout_exn t c1 in
    let* t, c1_again = commit_1_different_author t in
    let* () = start_gc t c1_again in
    let* () = finalise_gc t in
    let* () = check_1 t c1_again in
    S.Repo.close t.repo

  let tests =
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
      tc "Test lru" gc_lru;
      tc "Test gc during batch" gc_during_batch;
      tc "Test add back gced commit" add_back_gced_commit;
      tc "Test gc on similar commits" gc_similar_commits;
    ]
end

module Concurrent_gc = struct
  (** Check that finding old objects during a gc works. *)
  let find_running_gc ~lru_size () =
    let* t = init ~lru_size () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    [%log.debug "Gc c1 keep c2"];
    let* () = start_gc t c2 in
    let* () = check_1 t c1 in
    let* () = check_2 t c2 in
    let* () = finalise_gc t in
    let* () = check_not_found t c1 "removed c1" in
    let* () = check_2 t c2 in
    S.Repo.close t.repo

  (** Check adding new objects during a gc and finding them after the gc. *)
  let add_running_gc ~lru_size () =
    let* t = init ~lru_size () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    [%log.debug "Gc c1 keep c2"];
    let* () = start_gc t c2 in
    let* t = checkout_exn t c2 in
    let* t, c3 = commit_3 t in
    let* () = finalise_gc t in
    let* () = check_not_found t c1 "removed c1" in
    let* () = check_2 t c2 in
    let* () = check_3 t c3 in
    S.Repo.close t.repo

  (** Check adding new objects during a gc and finding them after the gc. *)
  let several_gc ~lru_size () =
    let* t = init ~lru_size () in
    let* t, c1 = commit_1 t in
    let* () = start_gc t c1 in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let* () = finalise_gc t in
    let* () = start_gc t c2 in
    let* t = checkout_exn t c2 in
    let* t, c3 = commit_3 t in
    let* () = finalise_gc t in
    let* () = start_gc t c3 in
    let* t = checkout_exn t c3 in
    let* t, c4 = commit_4 t in
    let* () = finalise_gc t in
    let* () = start_gc t c4 in
    let* t = checkout_exn t c4 in
    let* t, c5 = commit_5 t in
    let* () = finalise_gc t in
    let* () = check_not_found t c1 "removed c1" in
    let* () = check_not_found t c2 "removed c2" in
    let* () = check_not_found t c3 "removed c3" in
    let* () = check_4 t c4 in
    let* () = check_5 t c5 in
    S.Repo.close t.repo

  let find_running_gc_with_lru = find_running_gc ~lru_size:100
  let add_running_gc_with_lru = add_running_gc ~lru_size:100
  let several_gc_with_lru = several_gc ~lru_size:100
  let find_running_gc = find_running_gc ~lru_size:0
  let add_running_gc = add_running_gc ~lru_size:0
  let several_gc = several_gc ~lru_size:0

  (** Check that RO can find old objects during gc. Also that RO can still find
      removed objects before a call to [reload]. *)
  let ro_find_running_gc () =
    let* t = init () in
    let* ro_t = init ~readonly:true ~fresh:false ~root:t.root () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    [%log.debug "Gc c1 keep c2"];
    let* () = start_gc t c2 in
    S.reload ro_t.repo;
    let* () = check_1 ro_t c1 in
    S.reload ro_t.repo;
    let* () = check_2 ro_t c2 in
    let* () = finalise_gc t in
    let* () = check_1 ro_t c1 in
    let* () = check_2 ro_t c2 in
    S.reload ro_t.repo;
    let* () = check_not_found ro_t c1 "removed c1" in
    let* () = check_2 t c2 in
    let* () = S.Repo.close t.repo in
    S.Repo.close ro_t.repo

  (** Check that RO can find objects added during gc, but only after a call to
      [reload]. *)
  let ro_add_running_gc () =
    let* t = init () in
    let* ro_t = init ~readonly:true ~fresh:false ~root:t.root () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    [%log.debug "Gc c1 keep c2"];
    let* () = start_gc t c2 in
    S.reload ro_t.repo;
    let* t = checkout_exn t c2 in
    let* t, c3 = commit_3 t in
    S.reload ro_t.repo;
    let* t = checkout_exn t c2 in
    let* t, c4 = commit_4 t in
    let* () = finalise_gc t in
    let* () = check_not_found ro_t c4 "not yet loaded c4" in
    let* () = check_1 ro_t c1 in
    let* () = check_2 ro_t c2 in
    let* () = check_3 ro_t c3 in
    S.reload ro_t.repo;
    let* () = check_not_found ro_t c1 "removed c1" in
    let* () = check_4 ro_t c4 in
    let* () = S.Repo.close t.repo in
    S.Repo.close ro_t.repo

  (** Check that RO can call [reload] during a second gc, even after no reloads
      occured during the first gc. *)
  let ro_reload_after_second_gc () =
    let* t = init () in
    let* ro_t = init ~readonly:true ~fresh:false ~root:t.root () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    [%log.debug "Gc c1 keep c2"];
    let* () = start_gc t c2 in
    let* () = finalise_gc t in
    let* t = checkout_exn t c2 in
    let* t, c3 = commit_3 t in
    [%log.debug "Gc c2 keep c3"];
    let* () = start_gc t c3 in
    let* () = finalise_gc t in
    S.reload ro_t.repo;
    let* () = check_not_found ro_t c1 "removed c1" in
    let* () = check_not_found ro_t c2 "removed c2" in
    let* () = check_3 t c3 in
    let* () = S.Repo.close t.repo in
    S.Repo.close ro_t.repo

  (** Check that calling close during a gc kills the gc without finalising it.
      On reopening the store, the following gc works fine. *)
  let close_running_gc () =
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* () = start_gc t c1 in
    let* () = S.Repo.close t.repo in
    let* t = init ~readonly:false ~fresh:false ~root:t.root () in
    let* () = check_1 t c1 in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let* () = start_gc t c2 in
    let* () = finalise_gc t in
    let* t = checkout_exn t c2 in
    S.Repo.close t.repo

  (** Check starting a gc before a previous is finalised. *)
  let test_skip () =
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* t = checkout_exn t c1 in
    let* t, c2 = commit_2 t in
    let* () = start_gc t c2 in
    let* t = checkout_exn t c2 in
    let* t, c3 = commit_3 t in
    let* () = start_gc t c3 in
    let* () = finalise_gc t in
    let* () = check_not_found t c1 "removed c1" in
    let* () = check_2 t c2 in
    let* () = check_3 t c3 in
    S.Repo.close t.repo

  let kill_gc t =
    let repo : S.Repo.t = t.repo in
    match (repo.running_gc : S.X.Repo.running_gc option) with
    | None -> Alcotest.failf "running_gc missing after call to start"
    | Some { gc; _ } -> (
        try S.X.Gc.cancel gc
        with Unix.Unix_error (Unix.ESRCH, "kill", _) -> false)

  let test_kill_gc_and_finalise () =
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* () = start_gc t c1 in
    let killed = kill_gc t in
    let* () =
      if killed then
        Alcotest.check_raises_lwt "Gc process killed"
          (Irmin_pack_unix.Errors.Pack_error
             (`Gc_process_died_without_result_file
               "cancelled \"No_such_file_or_directory\""))
          (fun () -> finalise_gc t)
      else Lwt.return_unit
    in
    S.Repo.close t.repo

  let test_kill_gc_and_close () =
    let* t = init () in
    let* t, c1 = commit_1 t in
    let* () = start_gc t c1 in
    let _killed = kill_gc t in
    S.Repo.close t.repo

  let tests =
    [
      tc "Test find_running_gc" find_running_gc;
      tc "Test add_running_gc" add_running_gc;
      tc "Test several_gc" several_gc;
      tc "Test find_running_gc_with_lru" find_running_gc_with_lru;
      tc "Test add_running_gc_with_lru" add_running_gc_with_lru;
      tc "Test several_gc_with_lru" several_gc_with_lru;
      tc "Test ro_find_running_gc" ro_find_running_gc;
      tc "Test ro_add_running_gc" ro_add_running_gc;
      tc "Test ro_reload_after_second_gc" ro_reload_after_second_gc;
      tc "Test close_running_gc" close_running_gc;
      tc "Test skip gc" test_skip;
      tc "Test kill gc and finalise" test_kill_gc_and_finalise;
      tc "Test kill gc and close" test_kill_gc_and_close;
    ]
end
