(* Level-1 smoke test for irmin-lwt: exercise the minimal Repo/Store
   lifecycle through the Lwt-wrapped API against the in-memory backend. *)

module Backend = Irmin_mem.KV.Make (Irmin.Contents.String)
module Store = Irmin_lwt.Make (Backend)

let info message () = Backend.Info.v ~author:"irmin-lwt-test" ~message 0L
let contents = Alcotest.option Alcotest.string

let test_set_then_find _switch () =
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* t = Store.main repo in
  let* () = Store.set_exn t ~info:(info "set foo") [ "foo" ] "bar" in
  let* v = Store.find t [ "foo" ] in
  Alcotest.check contents "foo -> bar" (Some "bar") v;
  let* () = Store.Repo.close repo in
  Lwt.return_unit

let test_remove _switch () =
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* t = Store.main repo in
  let* () = Store.set_exn t ~info:(info "set foo") [ "foo" ] "bar" in
  let* () = Store.remove_exn t ~info:(info "remove foo") [ "foo" ] in
  let* v = Store.find t [ "foo" ] in
  Alcotest.check contents "foo is gone" None v;
  let* () = Store.Repo.close repo in
  Lwt.return_unit

let test_missing_path _switch () =
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* t = Store.main repo in
  let* v = Store.find t [ "unset" ] in
  Alcotest.check contents "missing path" None v;
  let* () = Store.Repo.close repo in
  Lwt.return_unit

(* Level-2: a realistic workflow test that exercises branching, merging,
   and history introspection — the idioms a typical Irmin 3 consumer
   (e.g. Tezos' context) relies on — through the Lwt-wrapped API. *)

let test_branch_merge_workflow _switch () =
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* main = Store.main repo in
  (* Seed the main branch. *)
  let* () = Store.set_exn main ~info:(info "seed a") [ "a" ] "1" in
  let* () = Store.set_exn main ~info:(info "seed b") [ "b" ] "2" in
  (* Fork a feature branch from main and add a third entry. *)
  let* () =
    Store.set_exn main ~info:(info "fork point") [ "feature-flag" ] "yes"
  in
  let* feature = Store.of_branch repo "feature" in
  let* () = Store.set_exn feature ~info:(info "feature: add c") [ "c" ] "3" in
  (* Merge the feature branch back into main. *)
  let* result =
    Store.merge_into ~into:main ~info:(info "merge feature") feature
  in
  Alcotest.check
    (Alcotest.result Alcotest.unit Alcotest.reject)
    "merge succeeds" (Ok ()) result;
  (* Main should now see all three entries. *)
  let* a = Store.find main [ "a" ] in
  let* b = Store.find main [ "b" ] in
  let* c = Store.find main [ "c" ] in
  Alcotest.check contents "a survived" (Some "1") a;
  Alcotest.check contents "b survived" (Some "2") b;
  Alcotest.check contents "c merged in" (Some "3") c;
  (* [last_modified c] should return at least one commit (the one adding c). *)
  let* history = Store.last_modified main [ "c" ] in
  Alcotest.(check bool) "c has history" true (history <> []);
  let* () = Store.Repo.close repo in
  Lwt.return_unit

(* Level-3: interactions between the Lwt monad and the lwt_eio bridge.
   These are the subtle cases that can break real applications if the
   wrapper does not forward Lwt's scheduling semantics correctly. *)

let test_exception_caught_by_lwt _switch () =
  (* [Store.get] raises [Invalid_argument] on a missing path. The
     exception must propagate as a failed Lwt promise so that [Lwt.catch]
     can handle it. *)
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* t = Store.main repo in
  let* caught =
    Lwt.catch
      (fun () ->
        let* _ = Store.get t [ "nope" ] in
        Lwt.return_false)
      (fun _exn -> Lwt.return_true)
  in
  Alcotest.(check bool) "Lwt.catch caught the exception" true caught;
  let* () = Store.Repo.close repo in
  Lwt.return_unit

let test_pause_interleaved _switch () =
  (* [Lwt.pause] between two irmin-lwt calls must not break anything: the
     scheduler ceding control and resuming should leave the store in the
     expected state. *)
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* t = Store.main repo in
  let* () = Store.set_exn t ~info:(info "a") [ "x" ] "first" in
  let* () = Lwt.pause () in
  let* () = Store.set_exn t ~info:(info "b") [ "x" ] "second" in
  let* v = Store.find t [ "x" ] in
  Alcotest.check contents "last write wins" (Some "second") v;
  let* () = Store.Repo.close repo in
  Lwt.return_unit

let test_many_concurrent_reads _switch () =
  (* Dispatch several reads in parallel via [Lwt.all] and check they all
     complete with the expected value. This exercises the lwt_eio bridge
     under concurrent pressure from the Lwt side. *)
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* t = Store.main repo in
  let* () = Store.set_exn t ~info:(info "seed") [ "k" ] "v" in
  let n = 50 in
  let* results = Lwt.all (List.init n (fun _ -> Store.find t [ "k" ])) in
  Alcotest.(check int) "all reads completed" n (List.length results);
  Alcotest.(check bool)
    "all reads returned the same value" true
    (List.for_all (( = ) (Some "v")) results);
  let* () = Store.Repo.close repo in
  Lwt.return_unit

(* Submodule tests: exercise the Tree, Commit, Branch and Head wrappers
   that the MVP did not cover. These are the submodules a typical Irmin 3
   consumer (e.g. Tezos' context) uses heavily. *)

let test_tree_build_and_read _switch () =
  let open Lwt.Syntax in
  let empty = Store.Tree.empty () in
  let* tree = Store.Tree.add empty [ "a" ] "1" in
  let* tree = Store.Tree.add tree [ "b"; "c" ] "2" in
  let* v1 = Store.Tree.find tree [ "a" ] in
  let* v2 = Store.Tree.find tree [ "b"; "c" ] in
  let* missing = Store.Tree.find tree [ "nope" ] in
  Alcotest.check contents "tree a" (Some "1") v1;
  Alcotest.check contents "tree b/c" (Some "2") v2;
  Alcotest.check contents "tree missing" None missing;
  Alcotest.(check bool) "non-empty" false (Store.Tree.is_empty tree);
  Lwt.return_unit

let test_tree_fold _switch () =
  (* Traverse a small tree with a Lwt-returning contents folder and
     collect the encountered contents into a list. *)
  let open Lwt.Syntax in
  let empty = Store.Tree.empty () in
  let* tree = Store.Tree.add empty [ "a" ] "1" in
  let* tree = Store.Tree.add tree [ "b" ] "2" in
  let* tree = Store.Tree.add tree [ "c" ] "3" in
  let collect _path c acc = Lwt.return (c :: acc) in
  let* seen = Store.Tree.fold ~contents:collect tree [] in
  let sorted = List.sort compare seen in
  Alcotest.(check (list string))
    "fold collected all contents" [ "1"; "2"; "3" ] sorted;
  Lwt.return_unit

let hash_to_string h = Irmin.Type.to_string Backend.Hash.t h

let test_commit_and_branch _switch () =
  (* Build a tree, commit it explicitly through [Commit.v], set a branch
     to it through [Branch.set], then read it back via [Branch.find]. *)
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let tree = Store.Tree.empty () in
  let* tree = Store.Tree.add tree [ "k" ] "v" in
  let* c =
    Store.Commit.v repo ~info:(info "explicit commit" ()) ~parents:[] tree
  in
  let* () = Store.Branch.set repo "topic" c in
  let* c' = Store.Branch.find repo "topic" in
  let* () =
    match c' with
    | None -> Alcotest.fail "branch lookup returned None"
    | Some c' ->
        Alcotest.(check string)
          "same commit hash"
          (hash_to_string (Store.Commit.hash c))
          (hash_to_string (Store.Commit.hash c'));
        Lwt.return_unit
  in
  let* () = Store.Repo.close repo in
  Lwt.return_unit

let test_head_follows_writes _switch () =
  (* After a write, [Head.find] should see a commit, and the returned
     commit's tree should contain the new entry. Use a unique branch
     name so other tests don't pollute the in-memory backend's shared
     state. *)
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* t = Store.of_branch repo "head-follows-writes" in
  let* head0 = Store.Head.find t in
  Alcotest.(check bool) "empty head initially" true (Option.is_none head0);
  let* () = Store.set_exn t ~info:(info "create head") [ "k" ] "v" in
  let* head1 = Store.Head.find t in
  let* () =
    match head1 with
    | None -> Alcotest.fail "expected a head after a write"
    | Some c ->
        let* v = Store.Tree.find (Store.Commit.tree c) [ "k" ] in
        Alcotest.check contents "head tree contains write" (Some "v") v;
        Lwt.return_unit
  in
  let* () = Store.Repo.close repo in
  Lwt.return_unit

let () =
  Irmin_lwt.run @@ fun () ->
  Alcotest_lwt.run "irmin-lwt"
    [
      ( "smoke",
        [
          Alcotest_lwt.test_case "set then find" `Quick test_set_then_find;
          Alcotest_lwt.test_case "remove" `Quick test_remove;
          Alcotest_lwt.test_case "missing path" `Quick test_missing_path;
        ] );
      ( "workflow",
        [
          Alcotest_lwt.test_case "branch + merge + history" `Quick
            test_branch_merge_workflow;
        ] );
      ( "lwt-interaction",
        [
          Alcotest_lwt.test_case "Lwt.catch catches an Irmin exception" `Quick
            test_exception_caught_by_lwt;
          Alcotest_lwt.test_case "Lwt.pause interleaves with Irmin ops" `Quick
            test_pause_interleaved;
          Alcotest_lwt.test_case "many concurrent reads via Lwt.all" `Quick
            test_many_concurrent_reads;
        ] );
      ( "tree",
        [
          Alcotest_lwt.test_case "build and read" `Quick
            test_tree_build_and_read;
          Alcotest_lwt.test_case "fold with Lwt callback" `Quick test_tree_fold;
        ] );
      ( "commit-branch-head",
        [
          Alcotest_lwt.test_case "commit + branch round-trip" `Quick
            test_commit_and_branch;
          Alcotest_lwt.test_case "head follows writes" `Quick
            test_head_follows_writes;
        ] );
    ]
