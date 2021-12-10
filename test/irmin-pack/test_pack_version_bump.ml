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

  type mode = [`RW | `RO ]

  let log_size = 500_000    

  let index ~mode ~dir = Common.Index.v ~readonly:(mode=`RO) ~log_size dir

  type 'a pack = 'a Common.Pack.t

  let open_v1_store ~(mode:mode) ~dir : _ pack Lwt.t = 
    let index = index ~mode ~dir in
    (* NOTE Common.Pack.v returns in lwt *)
    let* pack = Common.Pack.v ~readonly:(mode=`RO) ~index dir in
    Lwt.return pack

  let close_store (pack:_ pack) : unit Lwt.t = Common.Pack.close pack
    
  let version (pack:_ pack) : [`V1 | `V2 ] = 
    (* FIXME no version info in this intf *)
    Common.Pack.debug_block pack |> fun io -> 
    Irmin_pack.IO.Unix.version io |> fun ver -> 
    ver

  let value_not_in_pack = "a value, hopefully_not_present_in_pack"

  (* FIXME not clear how to make changes to the pack that would trip
     the version upgrade; Pack.unsafe_append? *)
  let make_changes (pack:_ pack) : unit Lwt.t =
    Common.Pack.add pack value_not_in_pack >>= fun key -> 
    (* FIXME we need the value_version of v to be v2, but if it is just contents... *)
    Common.Pack.unsafe_append ~ensure_unique:false ~overcommit:false pack key value_not_in_pack |> fun _key' -> 
    (* Common.Pack.flush ~index:true ~index_merge:true pack; *)
    Lwt.return ()

end
open Util


let test_RO_no_version_bump ~tmp_dir : unit Lwt.t = 
  [%log.info "Executing test_RO_no_version_bump in dir %s\n%!" tmp_dir];
  let mode = `RO in
  rm_dir tmp_dir;
  copy_dir v1_store_archive_dir tmp_dir;
  let* store = open_v1_store ~mode ~dir:tmp_dir in
  assert(version store = `V1);
  let* () = close_store store in
  (* maybe the version bump is only visible after closing the
     store... so check again *)
  let* store = open_v1_store ~mode ~dir:tmp_dir in
  assert(version store = `V1);
  let* () = close_store store in
  Lwt.return ()

let test_RW_no_version_bump ~tmp_dir : unit Lwt.t = 
  [%log.info "Executing test_RW_no_version_bump in dir %s\n%!" tmp_dir];
  let mode = `RW in
  rm_dir tmp_dir;
  copy_dir v1_store_archive_dir tmp_dir;
  let* store = open_v1_store ~mode ~dir:tmp_dir in
  assert(version store = `V1);
  let* () = close_store store in
  (* maybe the version bump is only visible after closing the
     store... so check again *)
  let* store = open_v1_store ~mode ~dir:tmp_dir in
  assert(version store = `V1);
  let* () = close_store store in
  Lwt.return ()

let test_RW_version_bump ~tmp_dir = 
  [%log.info "Executing test_RW_version_bump in dir %s\n%!" tmp_dir];
  let mode = `RW in
  rm_dir tmp_dir;
  copy_dir v1_store_archive_dir tmp_dir;
  let* store = open_v1_store ~mode ~dir:tmp_dir in
  assert(version store = `V1);
  (* make some changes to the store to elicit the expected version
     bump *)
  let* () = make_changes store in
  assert(version store = `V2);
  let* () = close_store store in
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
  
