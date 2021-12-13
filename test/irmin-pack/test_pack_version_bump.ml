(** These tests for issue #1658, which follows PR #1655 which
   introduces a store version bump from V1 to V2. 

There are 3 tests: 

- test_RO_no_version_bump: open a V1 store RO mode, the version should
  remain V1

- test_RW_no_version_bump: open a V1 store RW mode, but don't attempt
  to mutate, version should remain V1

- test_RW_version_bump: open a V1 store RW mode, change something,
  version should bump to V2

*)


open! Import
open Common

let src = Logs.Src.create "tests.version_bump" ~doc:"Test pack version bump"

module Log = (val Logs.src_log src : Logs.LOG)

module Util = struct

  type mode = [`RW | `RO ]

  (** Following are generic utils *)

  let tmp_dir () = Filename.temp_file "test_pack_version_bump_" ""

  (** Make a directory; dir is assumed to not exist; parent is assumed to exist *)
  let mkdir p = 
    assert(not (Sys.file_exists p));
    Filename.quote_command "mkdir" [p] |> fun cmd -> 
    [%log.info "exec: %s\n%!" cmd];
    Sys.command cmd |> function
    | 0 -> ()
    | n -> 
      Fmt.failwith "Command `%s' exited with non-zero code %d\n" cmd n
    

  (** Copy src to dst; dst is assumed to not exist *)
  let copy_dir src dst = 
    assert(not (Sys.file_exists dst));
    (* don't check if it is empty; perhaps we should *)
    Filename.quote_command "cp" ["-R";src;dst] |> fun cmd -> 
    [%log.info "exec: %s\n%!" cmd];
    Sys.command cmd |> function
    | 0 -> ()
    | n -> 
      Fmt.failwith "Failed to set up the test env; command `%s' exited \
                    with non-zero code %d\n" cmd n      

  (** More specific utils from here *)

  let v1_store_archive_dir = "test/irmin-pack/data/version_1"

  (** Check we are in the location we expect, the project root. See
     test_existing_stores.goto_project_root for alternative. *)
  let assert_project_root () = 
    match Sys.file_exists v1_store_archive_dir with
    | true -> ()
    | false -> 
      Fmt.failwith "Couldn't find dir %S from cwd %S\n%!" 
        v1_store_archive_dir 
        Filename.current_dir_name

  let _ = assert_project_root ()

  module Unix_ = Irmin_pack.IO.Unix

  
  (** Get the version of the underlying file; file is assumed to
     exist; file is assumed to be an Irmin_pack.IO.Unix file *)
  let io_get_version ~fn : [`V1 | `V2] = 
    assert(Sys.file_exists fn);
    Unix_.v ~version:None ~fresh:false ~readonly:true fn |> fun t ->
    let r = Unix_.version t in
    Unix_.close t;
    r
    
end
open Util


(** The test_RW_version_bump requires the full Irmin_pack.KV interface *)
module Store_ = struct
  (* Craig slack 2021-12-13@11:02

module Store = Irmin_pack.KV (Common.Conf)

let test () =
  let config = Irmin_pack.config "./data/version_1" in
  let* store = Store.Repo.v config >>= Store.main in
  Store.set_tree_exn store [] (Store.Tree.empty ())
  *)
  
  (* taken from common.ml *)
  module Conf = Irmin_tezos.Conf 

  module Store1 = Irmin_pack.KV(Common.Conf)
  module Store2 = Store1.Make(Irmin.Contents.String)
  module Store = Store2


  let open_v1_store ~mode ~dir = 
    let config : Irmin.config = Irmin_pack.config ~readonly:(mode=`RO) dir in
    let* repo : Store.repo = Store.Repo.v config in
    Lwt.return repo

  let close_store repo = Store.Repo.close repo

  let add_tree repo : unit Lwt.t = 
    let* main = Store.main repo in
    let info () = Store.Info.v (Int64.of_int 0) in
    Store.set_tree_exn ~info main [] (Store.Tree.empty ())

  let _ = add_tree

  let force_version_bump = add_tree
 
end
open Store_


let test_RO_no_version_bump ~tmp_dir : unit Lwt.t = 
  [%log.info "Executing test_RO_no_version_bump in dir %s\n%!" tmp_dir];
  let mode = `RO in
  rm_dir tmp_dir;
  copy_dir v1_store_archive_dir tmp_dir;
  (* .../index/log seems to be in an incorrect format? *)
  Sys.remove (tmp_dir ^"/index/log");
  assert(io_get_version ~fn:(tmp_dir ^"/store.pack") = `V1);
  let* store = open_v1_store ~mode ~dir:tmp_dir in
  let* () = close_store store in
  (* maybe the version bump is only visible after closing the
     store... so check again *)
  assert(io_get_version ~fn:(tmp_dir ^"/store.pack") = `V1);
  Lwt.return ()

let test_RW_no_version_bump ~tmp_dir : unit Lwt.t = 
  [%log.info "Executing test_RW_no_version_bump in dir %s\n%!" tmp_dir];
  let mode = `RW in
  rm_dir tmp_dir;
  copy_dir v1_store_archive_dir tmp_dir;
  (* .../index/log seems to be in an incorrect format? *)
  Sys.remove (tmp_dir ^"/index/log");
  assert(io_get_version ~fn:(tmp_dir ^"/store.pack") = `V1);
  let* store = open_v1_store ~mode ~dir:tmp_dir in
  let* () = close_store store in
  (* maybe the version bump is only visible after closing the
     store... so check again *)
  assert(io_get_version ~fn:(tmp_dir ^"/store.pack") = `V1);
  Lwt.return ()

let test_RW_version_bump ~tmp_dir = 
  [%log.info "Executing test_RW_version_bump in dir %s\n%!" tmp_dir];
  let mode = `RW in
  rm_dir tmp_dir;
  copy_dir v1_store_archive_dir tmp_dir;
  (* .../index/log seems to be in an incorrect format? *)
  Sys.remove (tmp_dir ^"/index/log");
  assert(io_get_version ~fn:(tmp_dir ^"/store.pack") = `V1);
  let* store = open_v1_store ~mode ~dir:tmp_dir in
  let* () = force_version_bump store in
  let* () = close_store store in
  (* maybe the version bump is only visible after closing the
     store... so check again *)
  assert(io_get_version ~fn:(tmp_dir ^"/store.pack") = `V2);
  Lwt.return ()


let tests = 
  (* f applies g to a new tmp_dir *)
  let f g = fun () -> 
    Lwt_main.run @@ g ~tmp_dir:(tmp_dir()) 
  in
  Alcotest.[
    test_case "test_RO_no_version_bump" `Quick (f test_RO_no_version_bump);
    test_case "test_RW_no_version_bump" `Quick (f test_RW_no_version_bump);
    test_case "test_RW_version_bump" `Quick (f test_RW_version_bump);     
  ]
  
