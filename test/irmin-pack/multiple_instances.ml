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
  S.Commit.of_hash ro (S.Commit.hash c) >>= function
  | None -> Alcotest.fail "no hash"
  | Some commit ->
      let tree = S.Commit.tree commit in
      S.Tree.find tree [ "a" ] >>= fun x ->
      Alcotest.(check (option string)) "RO find" (Some "x") x;
      S.Repo.close ro

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
  S.Repo.v (config ~readonly:false ~fresh:true root) >>= fun rw ->
  S.Repo.v (config ~readonly:true ~fresh:false root) >>= fun ro ->
  S.Tree.add S.Tree.empty [ "a" ] "x" >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >>= fun c1 ->
  S.sync ro;
  check ro c1 "a" "x" >>= fun () ->
  S.Tree.add S.Tree.empty [ "a" ] "y" >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >>= fun c2 ->
  check ro c1 "a" "x" >>= fun () ->
  (S.Commit.of_hash ro (S.Commit.hash c2) >|= function
   | None -> ()
   | Some _ -> Alcotest.failf "should not find branch by")
  >>= fun () ->
  S.sync ro;
  check ro c2 "a" "y" >>= fun () ->
  S.Repo.close ro >>= fun () -> S.Repo.close rw

let ro_sync_after_close () =
  let check ro c k v =
    S.Commit.of_hash ro (S.Commit.hash c) >>= function
    | None -> Alcotest.failf "commit not found"
    | Some commit ->
        let tree = S.Commit.tree commit in
        S.Tree.find tree [ k ] >|= fun x ->
        Alcotest.(check (option string)) "RO find" (Some v) x
  in
  rm_dir root;
  S.Repo.v (config ~readonly:false ~fresh:true root) >>= fun rw ->
  S.Repo.v (config ~readonly:true ~fresh:false root) >>= fun ro ->
  S.Tree.add S.Tree.empty [ "a" ] "x" >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >>= fun c1 ->
  S.Repo.close rw >>= fun () ->
  S.sync ro;
  check ro c1 "a" "x" >>= fun () -> S.Repo.close ro

let tests =
  [
    Alcotest.test_case "Test open ro after rw closed" `Quick (fun () ->
        Lwt_main.run (open_ro_after_rw_closed ()));
    Alcotest.test_case "Test ro sync after add" `Quick (fun () ->
        Lwt_main.run (ro_sync_after_add ()));
    Alcotest.test_case "Test ro sync after close" `Quick (fun () ->
        Lwt_main.run (ro_sync_after_close ()));
  ]
