open Lwt.Infix
open Common

let ( let* ) x f = Lwt.bind x f

let root = Filename.concat "_build" "test-instances"

let src = Logs.Src.create "tests.instances" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

let index_log_size = Some 1_000

module Conf = struct
  let entries = 32

  let stable_hash = 256
end

module Hash = Irmin.Hash.SHA1
module S =
  Irmin_pack.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

let config ?(readonly = false) ?(fresh = true) root =
  Irmin_pack.config ~readonly ?index_log_size ~fresh root

let info () = Irmin.Info.empty

let open_ro_after_rw_closed () =
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* t = S.master rw in
  let* tree = S.Tree.add S.Tree.empty [ "a" ] "x" in
  let* () = S.set_tree_exn ~parents:[] ~info t [] tree in
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  let* () = S.Repo.close rw in
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
      S.Tree.find tree key >|= fun x ->
      Alcotest.(check (option string)) msg (Some value) x

let ro_sync_after_add () =
  let check ro c k v =
    S.Commit.of_hash ro (S.Commit.hash c) >>= function
    | None -> Alcotest.failf "commit not found"
    | Some commit ->
        let tree = S.Commit.tree commit in
        S.Tree.find tree [ k ] >|= fun x ->
        Alcotest.(check (option string)) "RO find" (Some v) x
  in
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  let* tree = S.Tree.add S.Tree.empty [ "a" ] "x" in
  let* c1 = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  S.sync ro;
  let* () = check ro c1 "a" "x" in
  let* tree = S.Tree.add S.Tree.empty [ "a" ] "y" in
  let* c2 = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  let* () = check ro c1 "a" "x" in
  let* () =
    S.Commit.of_hash ro (S.Commit.hash c2) >|= function
    | None -> ()
    | Some _ -> Alcotest.failf "should not find branch by"
  in
  S.sync ro;
  let* () = check ro c2 "a" "y" in
  let* () = S.Repo.close ro in
  S.Repo.close rw

let ro_sync_after_close () =
  let binding f = f [ "a" ] "x" in
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  let* tree = binding (S.Tree.add S.Tree.empty ?metadata:None) in
  let* c1 = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  let* () = S.Repo.close rw in
  S.sync ro;
  let* () = binding (check_binding ro c1) in
  S.Repo.close ro

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
  let* () = clear_all rw in
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  let* () = check_commit_absent ro c in
  let* () = S.Repo.close rw in
  S.Repo.close ro

(** RO looks for values before and after sync but after RW was cleared. *)
let clear_rw_find_ro () =
  rm_dir root;
  let* rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let* ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  let* tree = S.Tree.add S.Tree.empty [ "a" ] "x" in
  let* c1 = S.Commit.v rw ~parents:[] ~info:(info ()) tree in
  S.sync ro;
  let* () = check_binding ro c1 ~msg:"RO finds value" [ "a" ] "x" in
  let* () = clear_all rw in
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
  let* () = check_commit_absent ro c1 in
  let* () = S.Repo.close rw in
  S.Repo.close ro

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
    let* () = check_empty () in
    let* tree = S.Tree.add S.Tree.empty [ "a" ] "x" in
    S.set_tree_exn ~parents:[] ~info t [] tree
  in
  let add_after_clear () =
    let* () = add () in
    let* c = S.Head.get t in
    check_binding rw c ~msg:"RW finds value added after clear" [ "a" ] "x"
  in
  let* () = add () in
  let* () = clear_all rw in
  let* () = add_after_clear () in
  let* () = clear_all rw in
  let* () = check_empty () in
  S.Repo.close rw

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
  ]
