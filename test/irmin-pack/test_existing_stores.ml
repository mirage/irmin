(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import
open Common

let src = Logs.Src.create "tests.integrity_checks" ~doc:"Test integrity checks"

module Log = (val Logs.src_log src : Logs.LOG)

let config ?(readonly = false) ?(fresh = true)
    ?(indexing_strategy = Irmin_pack.Indexing_strategy.always) root =
  Irmin_pack.config ~readonly ~index_log_size:1000 ~indexing_strategy ~fresh
    root

let archive =
  [
    ("bar", [ ([ "a"; "d" ], "x"); ([ "a"; "b"; "c" ], "z") ]);
    ("foo", [ ([ "b" ], "y") ]);
  ]

let root_v1_archive ~fs =
  Eio.Path.(fs / "test" / "irmin-pack" / "data" / "version_1")

let root_v1 ~fs = Eio.Path.(fs / "_build" / "test_pack_version_1")
let tmp ~fs = Eio.Path.(fs / "_build" / "test_index_reconstruct")

module Test (S : Irmin.Generic_key.KV with type Schema.Contents.t = string) =
struct
  let check_commit repo commit bindings =
    match commit |> S.Commit.key |> S.Commit.of_key repo with
    | None ->
        Alcotest.failf "Commit `%a' is dangling in repo" S.Commit.pp_hash commit
    | Some commit ->
        let tree = S.Commit.tree commit in
        bindings
        |> List.iter (fun (key, value) ->
            S.Tree.find tree key
            |> Alcotest.(check (option string))
                 (Fmt.str "Expected binding [%a ↦ %s]"
                    Fmt.(Dump.list string)
                    key value)
                 (Some value))

  let check_repo repo structure =
    structure
    |> List.iter @@ fun (branch, bindings) ->
       match S.Branch.find repo branch with
       | None -> Alcotest.failf "Couldn't find expected branch `%s'" branch
       | Some commit -> check_commit repo commit bindings

  let commit_of_string repo c =
    match Irmin.Type.of_string S.Hash.t c with
    | Ok x -> (
        let commit = S.Commit.of_hash repo x in
        match commit with
        | None -> Alcotest.fail "could not find commit in store"
        | Some x -> x)
    | _ -> Alcotest.fail "could not read hash"

  let bin_string_of_string c =
    let s = ref "" in
    let f x = s := !s ^ x in
    let () =
      match Irmin.Type.of_string S.Hash.t c with
      | Ok x -> Irmin.Type.(unstage (encode_bin S.Hash.t)) x f
      | _ -> Alcotest.fail "could not read hash"
    in
    !s
end

module Small_conf = struct
  let entries = 2
  let stable_hash = 3
  let contents_length_header = Some `Varint
  let inode_child_order = `Hash_bits
  let forbid_empty_dir_persistence = false
end

module V1_maker = Irmin_pack_unix.Maker (Small_conf)
module V2_maker = Irmin_pack_unix.Maker (Conf)

module Schema_v2 = struct
  open Irmin
  module Metadata = Metadata.None
  module Contents = Contents.String_v2
  module Path = Path.String_list
  module Branch = Branch.String
  module Hash = Hash.SHA1
  module Node = Node.Generic_key.Make_v2 (Hash) (Path) (Metadata)
  module Commit = Commit.Generic_key.Make_v2 (Hash)
  module Info = Info.Default
end

module V1 () = V1_maker.Make (Schema_v2)
module V2 () = V2_maker.Make (Schema_v2)

module Test_store = struct
  module S = V2 ()
  include Test (S)
end

module Test_reconstruct = struct
  module S = V2 ()
  include Test (S)

  let setup_test_env ~fs () =
    setup_test_env ~root_archive:(root_v1_archive ~fs)
      ~root_local_build:(root_v1 ~fs);
    setup_test_env ~root_archive:(root_v1_archive ~fs)
      ~root_local_build:(tmp ~fs)

  let test_reconstruct ~fs () =
    let module Kind = Irmin_pack.Pack_value.Kind in
    setup_test_env ~fs ();
    (* Open store in RW to migrate it to V3. *)
    Eio.Switch.run @@ fun sw ->
    let conf = config ~sw ~fs ~readonly:false ~fresh:false (root_v1 ~fs) in
    let repo = S.Repo.v conf in
    let () = S.Repo.close repo in
    (* Test on a V3 store. *)
    S.test_traverse_pack_file (`Reconstruct_index `In_place) conf;
    let index_old =
      Index.v_exn ~fresh:false ~readonly:false ~log_size:500_000
        (Eio.Path.native_exn @@ tmp ~fs)
    in
    let index_new =
      Index.v_exn ~fresh:false ~readonly:false ~log_size:500_000
        (Eio.Path.native_exn @@ root_v1 ~fs)
    in
    Index.iter
      (fun k (offset, length, kind) ->
        [%log.debug
          "index find k = %a (off, len, kind) = (%a, %d, %a)"
            (Irmin.Type.pp S.Hash.t) k Int63.pp offset length Kind.pp kind];
        match Index.find index_new k with
        | Some (offset', length', kind') ->
            Alcotest.(check int63) "check offset" offset offset';
            Alcotest.(check int) "check length" length length';
            Alcotest.(check_repr Kind.t) "check kind" kind kind'
        | None ->
            Alcotest.failf "expected to find hash %a" (Irmin.Type.pp S.Hash.t) k)
      index_old;
    Index.close_exn index_old;
    Index.close_exn index_new;
    [%log.app
      "Checking old bindings are still reachable post index reconstruction)"];
    let r = S.Repo.v conf in
    check_repo r archive;
    S.Repo.close r

  let test_gc_allowed ~fs () =
    setup_test_env ~fs ();
    Eio.Switch.run @@ fun sw ->
    let conf = config ~sw ~fs ~readonly:false ~fresh:false (root_v1 ~fs) in
    let repo = S.Repo.v conf in
    let allowed = S.Gc.is_allowed repo in
    Alcotest.(check bool)
      "deleting gc not allowed on stores with V1 objects" allowed false;
    S.Repo.close repo
end

module Test_corrupted_stores = struct
  let root_archive ~fs =
    Eio.Path.(fs / "test" / "irmin-pack" / "data" / "corrupted")

  let root ~fs = Eio.Path.(fs / "_build" / "test_integrity")

  let setup_env ~fs () =
    setup_test_env ~root_archive:(root_archive ~fs) ~root_local_build:(root ~fs)

  module S = V2 ()
  include Test (S)

  let test ~fs () =
    setup_env ~fs ();
    Eio.Switch.run @@ fun sw ->
    let rw = S.Repo.v (config ~sw ~fs ~fresh:false (root ~fs)) in
    [%log.app
      "integrity check on a store where 3 entries are missing from pack"];
    let result = S.integrity_check ~auto_repair:false rw in
    (match result with
    | Ok `No_error -> Alcotest.fail "Store is corrupted, the check should fail"
    | Error (`Corrupted 3) -> ()
    | _ -> Alcotest.fail "With auto_repair:false should not match");
    let result = S.integrity_check ~auto_repair:true rw in
    (match result with
    | Ok (`Fixed 3) -> ()
    | _ -> Alcotest.fail "Integrity check should repair the store");
    let result = S.integrity_check ~auto_repair:false rw in
    (match result with
    | Ok `No_error -> ()
    | _ -> Alcotest.fail "Store is repaired, should return Ok");
    S.Repo.close rw

  let root_archive ~fs =
    Eio.Path.(fs / "test" / "irmin-pack" / "data" / "version_3_minimal")

  let root_local_build ~fs = Eio.Path.(fs / "_build" / "test_corrupt_minimal")

  let setup_env ~fs () =
    setup_test_env ~root_archive:(root_archive ~fs)
      ~root_local_build:(root_local_build ~fs)

  module IO = Irmin_pack_unix.Io.Unix

  let write_corrupted_data_to_suffix ~fs () =
    Eio.Switch.run @@ fun sw ->
    let path = Eio.Path.(root_local_build ~fs / "store.0.suffix") in
    let io = IO.open_ ~sw ~path ~readonly:false |> Result.get_ok in
    let corrupted_node_hash =
      (* the correct hash starts with '9', modified it to have an incorrect hash
         on disk. *)
      "1b120e5019dcc6cd90b4d9c9826c9ebbebdc0023"
    in
    let s = bin_string_of_string corrupted_node_hash in
    let len = String.length s in
    assert (len = 20);
    IO.write_exn io ~off:(Int63.of_int 54) ~len s;
    IO.close io |> Result.get_ok

  let test_minimal ~fs () =
    setup_env ~fs ();
    [%log.app "integrity check on a good minimal store"];
    Eio.Switch.run @@ fun sw ->
    let config =
      config ~sw ~fs ~fresh:false
        ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal
        (root_local_build ~fs)
    in
    let rw = S.Repo.v config in

    let commit =
      commit_of_string rw "22e159de13b427226e5901defd17f0c14e744205"
    in
    let result = S.integrity_check ~heads:[ commit ] ~auto_repair:false rw in
    let () =
      match result with
      | Ok `No_error -> ()
      | Error (`Cannot_fix err) -> Alcotest.failf "Store is corrupted %s" err
      | _ -> Alcotest.fail "Unexpected result of integrity_check"
    in
    let () = S.Repo.close rw in
    [%log.app "integrity check on a corrupted minimal store"];
    write_corrupted_data_to_suffix ~fs ();
    let rw = S.Repo.v config in
    let result = S.integrity_check ~heads:[ commit ] ~auto_repair:false rw in
    let () =
      match result with
      | Ok `No_error -> Alcotest.fail "Store is corrupted, check should fail"
      | Error (`Cannot_fix err) ->
          let err = String.sub err 0 33 in
          Alcotest.(check string)
            "corrupted store" "Inconsistencies found: Wrong_hash" err
      | _ -> Alcotest.fail "Unexpected result of integrity_check"
    in

    S.Repo.close rw
end

module Test_corrupted_inode = struct
  let root_archive ~fs =
    Eio.Path.(fs / "test" / "irmin-pack" / "data" / "corrupted_inode")

  let root ~fs = Eio.Path.(fs / "_build" / "test_integrity_inode")

  let setup_test_env ~fs () =
    setup_test_env ~root_archive:(root_archive ~fs) ~root_local_build:(root ~fs)

  module S = V1 ()
  include Test (S)

  let test ~fs () =
    setup_test_env ~fs ();
    Eio.Switch.run @@ fun sw ->
    let rw = S.Repo.v (config ~sw ~fs ~fresh:false (root ~fs)) in
    [%log.app "integrity check of inodes on a store with one corrupted inode"];
    let c2 = "8d89b97726d9fb650d088cb7e21b78d84d132c6e" in
    let c2 = commit_of_string rw c2 in
    let result = S.integrity_check_inodes ~heads:[ c2 ] rw in
    (match result with
    | Ok _ ->
        Alcotest.failf
          "Store is corrupted for second commit, the check should fail"
    | Error _ -> ());
    let c1 = "1b1e259ca4e7bb8dc32c73ade93d8181c29cebe6" in
    let c1 = commit_of_string rw c1 in
    let result = S.integrity_check_inodes ~heads:[ c1 ] rw in
    (match result with
    | Error _ ->
        Alcotest.fail
          "Store is not corrupted for first commit, the check should not fail."
    | Ok _ -> ());
    S.Repo.close rw
end

module Test_traverse_gced = struct
  let root_archive ~fs =
    Eio.Path.(fs / "test" / "irmin-pack" / "data" / "version_3_minimal")

  let root_local_build ~fs = Eio.Path.(fs / "_build" / "test_reconstruct")

  let setup_test_env ~fs () =
    setup_test_env ~root_archive:(root_archive ~fs)
      ~root_local_build:(root_local_build ~fs)

  module S = V2 ()
  include Test (S)

  let commit_and_gc ~fs ~domain_mgr conf =
    Eio.Switch.run @@ fun sw ->
    let conf = conf ~sw ~fs in
    let repo = S.Repo.v conf in
    let commit =
      commit_of_string repo "22e159de13b427226e5901defd17f0c14e744205"
    in
    let tree = S.Commit.tree commit in
    let tree = S.Tree.add tree [ "abba"; "baba" ] "x" in
    let commit = S.Commit.v repo ~info:S.Info.empty ~parents:[] tree in
    let commit_key = S.Commit.key commit in
    let _ = S.Gc.start_exn ~domain_mgr ~unlink:false repo commit_key in
    let result = S.Gc.finalise_exn ~wait:true repo in
    let () =
      match result with
      | `Running -> Alcotest.fail "expected finalised gc"
      (* consider `Idle as success because gc can finalise during commit as well *)
      | `Idle | `Finalised _ -> ()
    in
    S.Repo.close repo

  let test_traverse_pack ~fs ~domain_mgr () =
    Eio.Switch.run @@ fun sw ->
    let module Kind = Irmin_pack.Pack_value.Kind in
    setup_test_env ~fs ();
    let conf =
      config ~readonly:false ~fresh:false
        ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal
        (root_local_build ~fs)
    in
    let () = commit_and_gc ~fs ~domain_mgr conf in
    S.test_traverse_pack_file `Check_index (conf ~sw ~fs)
end

module Test_symlinks = struct
  module S = V2 ()
  include Test (S)

  let root_links ~fs = Eio.Path.(fs / "_build" / "test_pack_symlinks")
  let root_link ~fs = Eio.Path.(fs / "_build" / "test_pack_root_symlink")

  (* [mirror ~src ~dst] creates [dst] with the directories of [src], and a
     symbolic link to each of its files, as the dune sandbox does. *)
  let rec mirror ~src ~dst =
    Unix.mkdir dst 0o755;
    Sys.readdir src
    |> Array.iter @@ fun name ->
       let src = Filename.concat src name and dst = Filename.concat dst name in
       if Sys.is_directory src then mirror ~src ~dst else Unix.symlink src dst

  (* [check ~readonly ~fs root] opens the store at [root] and checks its
     contents. *)
  let check ~readonly ~fs root =
    Eio.Switch.run @@ fun sw ->
    let repo = S.Repo.v (config ~sw ~fs ~readonly ~fresh:false root) in
    Fun.protect
      ~finally:(fun () -> S.Repo.close repo)
      (fun () -> check_repo repo archive)

  let test ~fs () =
    setup_test_env ~root_archive:(root_v1_archive ~fs)
      ~root_local_build:(root_v1 ~fs);
    (* Open the store in RW to migrate it to V3. *)
    Eio.Switch.run (fun sw ->
        S.Repo.close
          (S.Repo.v (config ~sw ~fs ~readonly:false ~fresh:false (root_v1 ~fs))));
    let store =
      Filename.concat (Sys.getcwd ()) (Eio.Path.native_exn (root_v1 ~fs))
    in
    let mirror_store () =
      rm_dir (root_links ~fs);
      mirror ~src:store ~dst:(Eio.Path.native_exn (root_links ~fs))
    in
    mirror_store ();
    check ~readonly:true ~fs (root_links ~fs);
    (* Recreating the store, or its index, replaces the links and leaves their
       targets untouched. *)
    let index_dir = Eio.Path.(root_v1 ~fs / "index") in
    (* The modification times as well, since a reconstructed index has the
       same contents. *)
    let index_files () =
      Eio.Path.read_dir index_dir
      |> List.map (fun name ->
          let file = Eio.Path.(index_dir / name) in
          ( name,
            ( Eio.Path.load file,
              (Eio.Path.stat ~follow:true file).Eio.File.Stat.mtime ) ))
    in
    let index_before = index_files () in
    let check_target msg =
      Alcotest.(check (list (pair string (pair string (float 0.)))))
        (msg ^ ": index untouched")
        index_before (index_files ());
      check ~readonly:true ~fs (root_v1 ~fs)
    in
    let open_fresh () =
      Eio.Switch.run @@ fun sw ->
      S.Repo.close
        (S.Repo.v (config ~sw ~fs ~readonly:false ~fresh:true (root_links ~fs)))
    in
    open_fresh ();
    check_target "fresh store";
    mirror_store ();
    let index_link = Eio.Path.(root_links ~fs / "index") in
    rm_dir index_link;
    Unix.symlink
      (Filename.concat store "index")
      (Eio.Path.native_exn index_link);
    open_fresh ();
    check_target "fresh store, index directory as a link";
    mirror_store ();
    Eio.Switch.run (fun sw ->
        S.test_traverse_pack_file (`Reconstruct_index `In_place)
          (config ~sw ~fs ~readonly:false ~fresh:false (root_links ~fs)));
    check_target "index reconstruction";
    (* A read-only open does not check the kind of the root, a read-write one
       does. *)
    rm_dir (root_link ~fs);
    Unix.symlink store (Eio.Path.native_exn (root_link ~fs));
    check ~readonly:false ~fs (root_link ~fs)

  (* A loop of symbolic links, a dangling one, or a directory where a file is
     expected, gives an error rather than an exception. *)
  let test_errors ~fs () =
    let module Io = Irmin_pack_unix.Io.Unix in
    let root = Eio.Path.(fs / "_build" / "test_pack_symlink_errors") in
    rm_dir root;
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 root;
    let loop = Eio.Path.(root / "loop") in
    let dangling = Eio.Path.(root / "dangling") in
    Unix.symlink "loop" (Eio.Path.native_exn loop);
    Unix.symlink "missing" (Eio.Path.native_exn dangling);
    Alcotest.(check bool)
      "a loop is not a file" true
      (Io.classify_path loop = `Other);
    Alcotest.(check bool)
      "a dangling link exists" true
      (Io.classify_path dangling = `Other);
    (match Io.mkdir Eio.Path.(loop / "dir") with
    | Error (`Io_misc _) -> ()
    | Ok () | Error _ -> Alcotest.fail "creating a directory in a loop");
    (match Io.mkdir dangling with
    | Error (`File_exists _) -> ()
    | Ok () | Error _ -> Alcotest.fail "creating a directory on a dangling link");
    Eio.Switch.run @@ fun sw ->
    (match Io.open_ ~sw ~path:loop ~readonly:true with
    | Error (`Io_misc _) -> ()
    | Ok _ | Error _ -> Alcotest.fail "opening a loop should fail");
    (match Io.create ~sw ~path:root ~overwrite:true with
    | Error (`File_exists _) -> ()
    | Ok _ | Error _ -> Alcotest.fail "creating a directory should fail");
    match Io.create ~sw ~path:dangling ~overwrite:true with
    | Error (`Io_misc _) -> ()
    | Ok _ | Error _ -> Alcotest.fail "creating a dangling link should fail"

  (* Overwriting a symbolic link replaces it by a new file, and does not
     truncate its target. *)
  let test_overwrite ~fs () =
    let module Io = Irmin_pack_unix.Io.Unix in
    let root = Eio.Path.(fs / "_build" / "test_pack_symlink_overwrite") in
    rm_dir root;
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 root;
    let target = Eio.Path.(root / "target") in
    let link = Eio.Path.(root / "link") in
    Eio.Path.save ~create:(`Exclusive 0o644) target "contents";
    Unix.symlink "target" (Eio.Path.native_exn link);
    Eio.Switch.run (fun sw ->
        match Io.create ~sw ~path:link ~overwrite:true with
        | Ok io -> Io.close io |> Result.get_ok
        | Error _ -> Alcotest.fail "overwriting a symbolic link");
    Alcotest.(check bool)
      "the link is replaced" true
      (Eio.Path.kind ~follow:false link = `Regular_file);
    Alcotest.(check string) "new file" "" (Eio.Path.load link);
    Alcotest.(check string) "target untouched" "contents" (Eio.Path.load target)
end

let tests ~fs ~domain_mgr =
  [
    Alcotest.test_case "Test store through symbolic links" `Quick
      (Test_symlinks.test ~fs);
    Alcotest.test_case "Test symbolic link errors" `Quick
      (Test_symlinks.test_errors ~fs);
    Alcotest.test_case "Test overwriting a symbolic link" `Quick
      (Test_symlinks.test_overwrite ~fs);
    Alcotest.test_case "Test index reconstruction" `Quick
      (Test_reconstruct.test_reconstruct ~fs);
    Alcotest.test_case "Test gc not allowed" `Quick
      (Test_reconstruct.test_gc_allowed ~fs);
    Alcotest.test_case "Test integrity check" `Quick
      (Test_corrupted_stores.test ~fs);
    Alcotest.test_case "Test integrity check minimal stores" `Quick
      (Test_corrupted_stores.test_minimal ~fs);
    Alcotest.test_case "Test integrity check for inodes" `Quick
      (Test_corrupted_inode.test ~fs);
    Alcotest.test_case "Test traverse pack on gced store" `Quick
      (Test_traverse_gced.test_traverse_pack ~fs ~domain_mgr);
  ]
