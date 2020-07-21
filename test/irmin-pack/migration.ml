open Lwt.Infix
open Common

let src = Logs.Src.create "tests.migration" ~doc:"Test migrations"

module Log = (val Logs.src_log src : Logs.LOG)

module Hash = Irmin.Hash.SHA1
module S =
  Irmin_pack.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

let config ?(readonly = false) ?(fresh = true) root =
  Irmin_pack.config ~readonly ?index_log_size ~fresh root

let info () = Irmin.Info.empty

let rootV1 = Filename.concat "_build" "testV1"

(** testV1 is a store in V1 used to test the migration to V2 *)
let setup_test_env () =
  let rootV2 = Filename.concat "_build" "_v2" in
  let root_testV1 = "testV1" in
  rm_dir rootV1;
  rm_dir rootV2;
  let cmd = Printf.sprintf "cp -r %s %s" root_testV1 rootV1 in
  Fmt.epr "exec: %s\n%!" cmd;
  let _ = Sys.command cmd in
  ()

(** Opening a store in V1 automatically migrates it to V2. Old values are
    readable and new values can be added. Reopening the store in RO mode is also
    tested. *)
let test () =
  setup_test_env ();
  let check repo commit msg k v =
    S.Commit.of_hash repo (S.Commit.hash commit) >>= function
    | None -> Alcotest.fail "no hash"
    | Some commit ->
        let tree = S.Commit.tree commit in
        S.Tree.find tree k >|= fun x ->
        Alcotest.(check (option string)) msg (Some v) x
  in
  let check_old_values r =
    (S.Branch.find r "bar" >>= function
     | None -> Alcotest.failf "branch bar not found"
     | Some commit ->
         check r commit "check old values a" [ "a"; "d" ] "x" >>= fun () ->
         check r commit "check old values a/b/c/" [ "a"; "b"; "c" ] "z")
    >>= fun () ->
    S.Branch.find r "foo" >>= function
    | None -> Alcotest.failf "branch foo not found"
    | Some commit -> check r commit "check old values b" [ "b" ] "y"
  in
  S.Repo.v (config ~readonly:false ~fresh:false rootV1) >>= fun r ->
  check_old_values r >>= fun () ->
  S.Tree.add S.Tree.empty [ "c" ] "x" >>= fun tree ->
  S.Commit.v r ~parents:[] ~info:(info ()) tree >>= fun c ->
  check r c "check new values" [ "c" ] "x" >>= fun () ->
  S.Repo.close r >>= fun () ->
  S.Repo.v (config ~readonly:false ~fresh:false rootV1) >>= fun r ->
  S.Repo.v (config ~readonly:true ~fresh:false rootV1) >>= fun ro ->
  check_old_values ro >>= fun () ->
  check ro c "check new values after reopening" [ "c" ] "x" >>= fun () ->
  S.Repo.close r >>= fun () -> S.Repo.close ro

let tests =
  [
    Alcotest.test_case "Test migration V1 to V2" `Quick (fun () ->
        Lwt_main.run (test ()));
  ]
