(** These tests for issue #1658, which follows PR #1655 which introduces a store
    version bump from V1 to V2 when a writer instance adds a V2-only pack entry
    (including nodes and commits, but not contents). *)

open! Import
open Common

(** {2 Preamble} *)

let src = Logs.Src.create "tests.version_bump" ~doc:"Test pack version bump"

module Log = (val Logs.src_log src : Logs.LOG)

module Util = struct
  (** Following are generic utils *)

  let exec_cmd = Common.exec_cmd
  let tmp_dir () = Filename.temp_file "test_pack_version_bump_" ""

  (** Copy src to dst; dst is assumed to not exist *)
  let copy_dir src dst =
    assert (not (Sys.file_exists dst));
    (* don't check if it is empty; perhaps we should *)
    Filename.quote_command "cp" [ "-R"; src; dst ] |> fun cmd ->
    exec_cmd cmd |> function
    | Ok () -> ()
    | Error n ->
        Fmt.failwith
          "Failed to set up test env; command `%s' exited with non-zero code %d\n"
          cmd n

  (** Identify the root directory, by comparing st_dev,st_ino *)
  let is_root =
    let open Unix in
    let root_stat = stat "/" in
    fun s ->
      let stat = Unix.stat s in
      (stat.st_dev, stat.st_ino) = (root_stat.st_dev, root_stat.st_ino)

  (** Starting from ".", try to find a parent directory that has a given
      property. *)
  let find_parent_matching test =
    let rec go path =
      match test path with
      | true -> Ok path
      | false -> (
          match is_root path with true -> Error () | false -> go (path ^ "/.."))
    in
    go "."

  (** More specific utils from here *)

  let v1_store_archive_dir = "test/irmin-pack/data/version_1"

  (** Find the project root, that contains the v1_store_archive_dir *)
  let project_root =
    find_parent_matching (fun d ->
        Sys.file_exists (d ^ "/" ^ v1_store_archive_dir))
    |> function
    | Ok s -> s
    | Error () ->
        Fmt.failwith
          "Couldn't find project root containing path to %s, after examining \
           current directory %s and ancestors"
          v1_store_archive_dir (Sys.getcwd ())

  (* Given the above, the following should always succeed *)
  let () = assert (Sys.file_exists (project_root ^ "/" ^ v1_store_archive_dir))

  module Unix_ = Irmin_pack.IO.Unix

  (** Get the version of the underlying file; file is assumed to exist; file is
      assumed to be an Irmin_pack.IO.Unix file *)
  let io_get_version ~fn : [ `V1 | `V2 ] =
    assert (Sys.file_exists fn);
    let t = Unix_.v ~version:None ~fresh:false ~readonly:true fn in
    let r = Unix_.version t in
    Unix_.close t;
    r

  let alco_check_version ~pos ~expected ~actual =
    Alcotest.check_repr ~pos Irmin_pack.Version.t "" expected actual
end

open Util

(** This sets up infrastructure to open the existing "version_1" store *)
module With_existing_store () = struct
  let tmp_dir = tmp_dir ()
  let () = [%log.info "Using temporary directory %s" tmp_dir]

  (* Make a copy of the v1_store_archive_dir in tmp_dir *)
  let () =
    rm_dir tmp_dir;
    copy_dir (project_root ^ "/" ^ v1_store_archive_dir) tmp_dir;
    ()

  (** Set up modules to allow access to "version_1" store *)
  module Private = struct
    (* In test_existing_stores.ml, we have Test_reconstruct, which
       uses S.traverse_pack_file; so S is likely the correct config
       for the pack file. S = V2(), V2() = V2_maker.Make(Schema);
       V2_maker = Irmin_pack.Maker(Conf); Conf is from Tezos_irmin;
       Schema is from common. *)
    (* Additional comment from @CraigFe: ``The specific Conf module
       used here doesn't impact the behaviour of the test (we have two
       flags that control the branching factor of inodes, and one that
       opts-in to more efficient reads of contents values).'' This
       implies that the exact Conf module used shouldn't affect the
       tests. *)
    module Conf = Irmin_tezos.Conf
    module Schema = Common.Schema
    (* Note this Schema is not the same as Irmin_tezos.Schema;
       Irmin_tezos uses their own hash; Common has Hash.SHA1 *)

    (* from test_existing_stores.ml; the V2 is because
       test_existing_stores defines two different configs *)
    module V2_maker = Irmin_pack.Maker (Conf)
    module V2 = V2_maker.Make (Schema)
  end

  (* [S] is the functionality we use from Private, together with an
     appropriate config *)
  module S = Private.V2

  (* Code copied and modified from test_existing_stores.ml; this is
     the config for index and pack *)
  let config ~readonly : Irmin.config =
    Irmin_pack.config ~readonly ~index_log_size:1000 ~fresh:false tmp_dir
end

(** {2 The tests} *)

(** test_RO_no_version_bump: open a V1 store RO mode, the version should remain
    V1 *)
let test_RO_no_version_bump () : unit Lwt.t =
  [%log.info "Executing test_RO_no_version_bump"];
  let open With_existing_store () in
  assert (io_get_version ~fn:(tmp_dir ^ "/store.pack") = `V1);
  let* repo = S.Repo.v (config ~readonly:true) in
  let* () = S.Repo.close repo in
  (* maybe the version bump is only visible after closing the
     store... so check again *)
  alco_check_version ~pos:__POS__ ~expected:`V1
    ~actual:(io_get_version ~fn:(tmp_dir ^ "/store.pack"));
  Lwt.return ()

(** test_RW_no_version_bump: open a V1 store RW mode, but don't attempt to
    mutate, version should remain V1 *)
let test_RW_no_version_bump () : unit Lwt.t =
  [%log.info "Executing test_RW_no_version_bump"];
  let open With_existing_store () in
  assert (io_get_version ~fn:(tmp_dir ^ "/store.pack") = `V1);
  let* repo = S.Repo.v (config ~readonly:false (* was RO before, now RW *)) in
  let* () = S.Repo.close repo in
  (* maybe the version bump is only visible after closing the
     store... so check again *)
  alco_check_version ~pos:__POS__ ~expected:`V1
    ~actual:(io_get_version ~fn:(tmp_dir ^ "/store.pack"));
  Lwt.return ()

(** test_RW_version_bump: open a V1 store RW mode, change something, version
    should bump to V2 *)
let test_RW_version_bump () : unit Lwt.t =
  [%log.info "Executing test_RW_version_bump"];
  let open With_existing_store () in
  assert (io_get_version ~fn:(tmp_dir ^ "/store.pack") = `V1);
  let* repo = S.Repo.v (config ~readonly:false) in
  (* force version bump by writing to the store *)
  let* main = S.main repo in
  let info () = S.Info.v (Int64.of_int 0) in
  let* () = S.set_tree_exn ~info main [] (S.Tree.empty ()) in
  (* close *)
  let* () = S.Repo.close repo in
  alco_check_version ~pos:__POS__ ~expected:`V2
    ~actual:(io_get_version ~fn:(tmp_dir ^ "/store.pack"));
  Lwt.return ()

let tests =
  (* following applies lwt-function-g to unit *)
  let f g () = Lwt_main.run @@ g () in
  Alcotest.
    [
      test_case "test_RO_no_version_bump" `Quick (f test_RO_no_version_bump);
      test_case "test_RW_no_version_bump" `Quick (f test_RW_no_version_bump);
      test_case "test_RW_version_bump" `Quick (f test_RW_version_bump);
    ]
