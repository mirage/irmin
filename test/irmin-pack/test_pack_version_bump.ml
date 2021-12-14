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


(** This sets up infrastructure to open the existing "version_1" store *)
module With_existing_store() = struct

  let tmp_dir = tmp_dir ()

  let _ = [%log.info "Using temporary directory %s\n%!" tmp_dir]

  (* Make a copy of the v1_store_archive_dir in tmp_dir *)
  let _ =
    assert_project_root();
    rm_dir tmp_dir;
    copy_dir v1_store_archive_dir tmp_dir;
    ()  
  
  (** Set up modules to allow access to "version_1" store *)
  module Store_ = struct
    (* In test_existing_stores.ml, we have Test_reconstruct, which
       uses S.traverse_pack_file; so S is likely the correct config
       for the pack file. S = V2(), V2() = V2_maker.Make(Schema);
       V2_maker = Irmin_pack.Maker(Conf); Conf is from Tezos_irmin;
       Schema is from common *)
    module Conf = Irmin_tezos.Conf
                    
    module Schema = Common.Schema 
    (* same as Irmin_tezos.Schema? no; tezos uses their own hash;
       common has Hash.SHA1 *)

    (* from test_existing_stores.ml; FIXME why V2? *)
    module V2_maker = Irmin_pack.Maker (Conf)
    module V2 = V2_maker.Make (Schema)
  end

  (* [S] and [config] are the main things we use from this module *)
  module S = Store_.V2
      
  (* Code copied and modified from test_existing_stores.ml; FIXME is
     this the config of index, or pack? or both? *)
  let config ~readonly : Irmin.config = 
    Irmin_pack.config ~readonly ~index_log_size:1000 ~fresh:false tmp_dir
end

let test_RO_no_version_bump () : unit Lwt.t = 
  [%log.info "Executing test_RO_no_version_bump in dir\n%!"];
  let open With_existing_store() in
  assert(io_get_version ~fn:(tmp_dir ^"/store.pack") = `V1);
  S.Repo.v (config ~readonly:true) >>= fun repo -> 
  S.Repo.close repo >>= fun () ->
  (* maybe the version bump is only visible after closing the
     store... so check again *)
  assert(io_get_version ~fn:(tmp_dir ^"/store.pack") = `V1);
  Lwt.return ()

let test_RW_no_version_bump () : unit Lwt.t = 
  [%log.info "Executing test_RW_no_version_bump in dir \n%!"];
  let open With_existing_store() in
  assert(io_get_version ~fn:(tmp_dir ^"/store.pack") = `V1);
  S.Repo.v (config ~readonly:false (* !changed! *)) >>= fun repo -> 
  S.Repo.close repo >>= fun () ->
  (* maybe the version bump is only visible after closing the
     store... so check again *)
  assert(io_get_version ~fn:(tmp_dir ^"/store.pack") = `V1);
  Lwt.return ()

let test_RW_version_bump () : unit Lwt.t = 
  [%log.info "Executing test_RW_version_bump\n%!"];
  let open With_existing_store() in
  assert(io_get_version ~fn:(tmp_dir ^"/store.pack") = `V1);
  S.Repo.v (config ~readonly:false (* !changed! *)) >>= fun repo -> 
  (* force version bump by writing to the store *)
  let* main = S.main repo in
  let info () = S.Info.v (Int64.of_int 0) in
  S.set_tree_exn ~info main [] (S.Tree.empty ()) >>= fun () -> 
  (* close *)
  S.Repo.close repo >>= fun () ->
  assert(io_get_version ~fn:(tmp_dir ^"/store.pack") = `V2);
  Lwt.return ()

let tests = 
  (* following applies lwt-function-g to unit *)
  let f g = fun () -> Lwt_main.run @@ g () in
  Alcotest.[
    test_case "test_RO_no_version_bump" `Quick (f test_RO_no_version_bump);
    test_case "test_RW_no_version_bump" `Quick (f test_RW_no_version_bump);
    test_case "test_RW_version_bump"    `Quick (f test_RW_version_bump);     
  ]
