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

module P = S.Private

let clear repo =
  let t = P.Repo.contents_t repo in
  let b = P.Repo.branch_t repo in
  let c = P.Repo.commit_t repo in
  let n = P.Repo.node_t repo in
  P.Contents.clear t >>= fun () ->
  P.Branch.clear b >>= fun () ->
  P.Commit.clear c >>= fun () -> P.Node.clear n

let clear_rw_open_ro () =
  rm_dir root;
  S.Repo.v (config ~readonly:false ~fresh:true root) >>= fun rw ->
  S.Tree.add S.Tree.empty [ "a" ] "x" >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >>= fun c ->
  let h = S.Commit.hash c in
  clear rw >>= fun () ->
  S.Repo.v (config ~readonly:true ~fresh:false root) >>= fun ro ->
  (S.Commit.of_hash ro h >|= function
   | None -> ()
   | Some _ -> Alcotest.fail "should not find commit in index")
  >>= fun () ->
  S.Repo.close rw >>= fun () -> S.Repo.close ro

let clear_rw_find_ro () =
  rm_dir root;
  S.Repo.v (config ~readonly:false ~fresh:true root) >>= fun rw ->
  S.Repo.v (config ~readonly:true ~fresh:false root) >>= fun ro ->
  S.Tree.add S.Tree.empty [ "a" ] "x" >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >>= fun c ->
  let h = S.Commit.hash c in
  (S.Commit.of_hash ro h >|= function
   | None -> Alcotest.fail "no hash"
   | Some _ -> ())
  >>= fun () ->
  clear rw >>= fun () ->
  S.Tree.add S.Tree.empty [ "b" ] "y" >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >>= fun c ->
  let h1 = S.Commit.hash c in
  (S.Commit.of_hash ro h1 >>= function
   | None -> Alcotest.fail "no hash"
   | Some commit ->
       let tree = S.Commit.tree commit in
       S.Tree.find tree [ "b" ] >|= fun y ->
       Alcotest.(check (option string)) "RO find" (Some "y") y)
  >>= fun () ->
  (S.Commit.of_hash ro h >|= function
   | None -> ()
   | Some _ -> Alcotest.fail "should not find commit in index")
  >>= fun () ->
  S.Repo.close rw >>= fun () -> S.Repo.close ro

let tests =
  [
    Alcotest.test_case "Test open ro after rw closed" `Quick (fun () ->
        Lwt_main.run (open_ro_after_rw_closed ()));
    Alcotest.test_case "Open ro after rw cleared" `Quick (fun () ->
        Lwt_main.run (clear_rw_open_ro ()));
    Alcotest.test_case "Find in ro after rw cleared" `Quick (fun () ->
        Lwt_main.run (clear_rw_find_ro ()));
  ]
