open Lwt.Infix
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
module S =
  Irmin_pack.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

let config ?(readonly = false) ?(fresh = true) root =
  Irmin_pack.config ~readonly ?index_log_size ~fresh root

let info () = Irmin.Info.empty

let check repo commit msg k v =
  S.Commit.of_hash repo (S.Commit.hash commit) >>= function
  | None -> Alcotest.fail "no hash"
  | Some commit ->
      let tree = S.Commit.tree commit in
      S.Tree.find tree [ k ] >|= fun x ->
      Alcotest.(check (option string)) msg (Some v) x

let check_not_found repo commit =
  S.Commit.of_hash repo (S.Commit.hash commit) >|= function
  | None -> ()
  | Some _ -> Alcotest.fail "should not find hash"

let open_ro_after_rw_closed () =
  rm_dir root;
  S.Repo.v (config ~readonly:false ~fresh:true root) >>= fun rw ->
  S.master rw >>= fun t ->
  S.Tree.add S.Tree.empty [ "a" ] "x" >>= fun tree ->
  S.set_tree_exn ~parents:[] ~info t [] tree >>= fun () ->
  S.Repo.v (config ~readonly:true ~fresh:false root) >>= fun ro ->
  S.Repo.close rw >>= fun () ->
  S.master ro >>= fun t ->
  S.Head.get t >>= fun c ->
  check ro c "RO find" "a" "x" >>= fun () -> S.Repo.close ro

let ro_sync_after_add () =
  rm_dir root;
  S.Repo.v (config ~readonly:false ~fresh:true root) >>= fun rw ->
  S.Repo.v (config ~readonly:true ~fresh:false root) >>= fun ro ->
  S.Tree.add S.Tree.empty [ "a" ] "x" >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >>= fun c1 ->
  S.sync ro;
  check ro c1 "RO find after add" "a" "x" >>= fun () ->
  S.Tree.add S.Tree.empty [ "a" ] "y" >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >>= fun c2 ->
  check ro c1 "RO finds old values" "a" "x" >>= fun () ->
  check_not_found ro c2 >>= fun () ->
  S.sync ro;
  check ro c2 "RO finds new values after sync" "a" "y" >>= fun () ->
  S.Repo.close ro >>= fun () -> S.Repo.close rw

let ro_sync_after_close () =
  rm_dir root;
  S.Repo.v (config ~readonly:false ~fresh:true root) >>= fun rw ->
  S.Repo.v (config ~readonly:true ~fresh:false root) >>= fun ro ->
  S.Tree.add S.Tree.empty [ "a" ] "x" >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >>= fun c1 ->
  S.Repo.close rw >>= fun () ->
  S.sync ro;
  check ro c1 "RO find after RW close" "a" "x" >>= fun () -> S.Repo.close ro

module P = S.Private

let clear repo =
  Log.debug (fun l -> l "clear repo");
  let t = P.Repo.contents_t repo in
  let b = P.Repo.branch_t repo in
  let c = P.Repo.commit_t repo in
  let n = P.Repo.node_t repo in
  P.Contents.clear t >>= fun () ->
  P.Branch.clear b >>= fun () ->
  P.Commit.clear c >>= fun () -> P.Node.clear n

(** Open RO after RW was cleared. *)
let clear_rw_open_ro () =
  rm_dir root;
  S.Repo.v (config ~readonly:false ~fresh:true root) >>= fun rw ->
  S.Tree.add S.Tree.empty [ "a" ] "x" >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >>= fun c ->
  clear rw >>= fun () ->
  S.Repo.v (config ~readonly:true ~fresh:false root) >>= fun ro ->
  check_not_found ro c >>= fun () ->
  S.Repo.close rw >>= fun () -> S.Repo.close ro

(** RO looks for values before and after sync but after RW was cleared. *)
let clear_rw_find_ro () =
  rm_dir root;
  S.Repo.v (config ~readonly:false ~fresh:true root) >>= fun rw ->
  S.Repo.v (config ~readonly:true ~fresh:false root) >>= fun ro ->
  S.Tree.add S.Tree.empty [ "a" ] "x" >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >>= fun c1 ->
  S.sync ro;
  check ro c1 "RO finds value" "a" "x" >>= fun () ->
  clear rw >>= fun () ->
  S.Tree.add S.Tree.empty [ "b" ] "y" >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >>= fun c2 ->
  check ro c1 "RO finds value after clear but before sync" "a" "x" >>= fun () ->
  S.sync ro;
  check ro c2 "RO finds value added after clear" "b" "y" >>= fun () ->
  check_not_found ro c1 >>= fun () ->
  S.Repo.close rw >>= fun () -> S.Repo.close ro

let clear_rw_twice () =
  rm_dir root;
  S.Repo.v (config ~readonly:false ~fresh:true root) >>= fun rw ->
  S.master rw >>= fun t ->
  let check_empty () =
    S.Head.find t >|= function
    | None -> ()
    | Some _ -> Alcotest.fail "should be empty"
  in
  let add () =
    check_empty () >>= fun () ->
    S.Tree.add S.Tree.empty [ "a" ] "x" >>= fun tree ->
    S.set_tree_exn ~parents:[] ~info t [] tree
  in
  let add_after_clear () =
    add () >>= fun () ->
    S.Head.get t >>= fun c ->
    check rw c "RW finds value added after clear" "a" "x"
  in
  add () >>= fun () ->
  clear rw >>= fun () ->
  add_after_clear () >>= fun () ->
  clear rw >>= fun () ->
  check_empty () >>= fun () -> S.Repo.close rw

let tests =
  [
    Alcotest.test_case "Test open ro after rw closed" `Quick (fun () ->
        Lwt_main.run (open_ro_after_rw_closed ()));
    Alcotest.test_case "Open ro after rw cleared" `Quick (fun () ->
        Lwt_main.run (clear_rw_open_ro ()));
    Alcotest.test_case "Clear rw twice" `Quick (fun () ->
        Lwt_main.run (clear_rw_twice ()));
    Alcotest.test_case "Find in ro after rw cleared" `Quick (fun () ->
        Lwt_main.run (clear_rw_find_ro ()));
    Alcotest.test_case "Test ro sync after add" `Quick (fun () ->
        Lwt_main.run (ro_sync_after_add ()));
    Alcotest.test_case "Test ro sync after close" `Quick (fun () ->
        Lwt_main.run (ro_sync_after_close ()));
  ]
