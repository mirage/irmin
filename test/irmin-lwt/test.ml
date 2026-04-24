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
    ]
