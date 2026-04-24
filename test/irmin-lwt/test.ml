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
    ]
