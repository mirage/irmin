(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

let src = Logs.Src.create "tests.migration" ~doc:"Test migrations"

module Log = (val Logs.src_log src : Logs.LOG)

let config ?(readonly = false) ?(fresh = true) root =
  Irmin_pack.config ~readonly ~index_log_size:1000 ~fresh root

let rec repeat = function
  | 0 -> fun _f x -> x
  | n -> fun f x -> f (repeat (n - 1) f x)

(** The current working directory depends on whether the test binary is directly
    run or is triggered with [dune exec], [dune runtest]. We normalise by
    switching to the project root first. *)
let goto_project_root () =
  let cwd = Fpath.v (Sys.getcwd ()) in
  match cwd |> Fpath.segs |> List.rev with
  | "irmin-pack" :: "test" :: "default" :: "_build" :: _ ->
      let root = cwd |> repeat 4 Fpath.parent in
      Unix.chdir (Fpath.to_string root)
  | _ -> ()

let archive =
  [
    ("bar", [ ([ "a"; "d" ], "x"); ([ "a"; "b"; "c" ], "z") ]);
    ("foo", [ ([ "b" ], "y") ]);
  ]

let exec_cmd cmd =
  [%log.info "exec: %s\n%!" cmd];
  match Sys.command cmd with
  | 0 -> ()
  | n ->
      Fmt.failwith
        "Failed to set up the test environment: command `%s' exited with \
         non-zero exit code %d"
        cmd n

module Config_store = struct
  let root_v1_archive, root_v1, tmp =
    let open Fpath in
    ( v "test" / "irmin-pack" / "data" / "version_1" |> to_string,
      v "_build" / "test_pack_migrate_1_to_2" |> to_string,
      v "_build" / "test_index_reconstruct" |> to_string )

  let setup_test_env () =
    goto_project_root ();
    let cmd =
      Filename.quote_command "cp" [ "-R"; "-p"; root_v1_archive; root_v1 ]
    in
    [%log.info "exec: %s\n%!" cmd];
    match Sys.command cmd with
    | 0 -> ()
    | n ->
        Fmt.failwith
          "Failed to set up the test environment: command `%s' exited with \
           non-zero exit code %d"
          cmd n
end

module Test (S : Irmin.Generic_key.KV with type Schema.Contents.t = string) =
struct
  let check_commit repo commit bindings =
    commit |> S.Commit.key |> S.Commit.of_key repo >>= function
    | None ->
        Alcotest.failf "Commit `%a' is dangling in repo" S.Commit.pp_hash commit
    | Some commit ->
        let tree = S.Commit.tree commit in
        bindings
        |> Lwt_list.iter_s (fun (key, value) ->
               S.Tree.find tree key
               >|= Alcotest.(check (option string))
                     (Fmt.str "Expected binding [%a ↦ %s]"
                        Fmt.(Dump.list string)
                        key value)
                     (Some value))

  let check_repo repo structure =
    structure
    |> Lwt_list.iter_s @@ fun (branch, bindings) ->
       S.Branch.find repo branch >>= function
       | None -> Alcotest.failf "Couldn't find expected branch `%s'" branch
       | Some commit -> check_commit repo commit bindings
end

module Small_conf = struct
  let entries = 2
  let stable_hash = 3
  let contents_length_header = Some `Varint
end

module V1_maker = Irmin_pack.Maker (Small_conf)
module V2_maker = Irmin_pack.Maker (Conf)
module V1 () = V1_maker.Make (Schema)
module V2 () = V2_maker.Make (Schema)

module Test_store = struct
  module S = V2 ()
  include Test (S)
end

module Test_reconstruct = struct
  module S = V2 ()
  include Test (S)

  let setup_test_env () =
    Config_store.setup_test_env ();
    rm_dir Config_store.tmp;
    let cmd =
      Filename.quote_command "cp"
        [ "-R"; "-p"; Config_store.root_v1_archive; Config_store.tmp ]
    in
    [%log.info "exec: %s\n%!" cmd];
    match Sys.command cmd with
    | 0 -> ()
    | n ->
        Fmt.failwith
          "Failed to set up the test environment: command `%s' exited with \
           non-zero exit code %d"
          cmd n

  let test_reconstruct () =
    let module Kind = Irmin_pack.Pack_value.Kind in
    setup_test_env ();
    let conf = config ~readonly:false ~fresh:false Config_store.root_v1 in
    S.traverse_pack_file (`Reconstruct_index `In_place) conf;
    let index_old =
      Index.v ~fresh:false ~readonly:false ~log_size:500_000 Config_store.tmp
    in
    let index_new =
      Index.v ~fresh:false ~readonly:false ~log_size:500_000
        Config_store.root_v1
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
    Index.close index_old;
    Index.close index_new;
    [%log.app
      "Checking old bindings are still reachable post index reconstruction)"];
    let* r = S.Repo.v conf in
    check_repo r archive >>= fun () -> S.Repo.close r
end

module Test_corrupted_stores = struct
  let root_archive, root =
    let open Fpath in
    ( v "test" / "irmin-pack" / "data" / "corrupted" |> to_string,
      v "_build" / "test_integrity" |> to_string )

  let setup_test_env () =
    goto_project_root ();
    rm_dir root;
    let cmd = Filename.quote_command "cp" [ "-R"; "-p"; root_archive; root ] in
    exec_cmd cmd

  let test () =
    setup_test_env ();
    let module S = V2 () in
    let* rw = S.Repo.v (config ~fresh:false root) in
    [%log.app
      "integrity check on a store where 3 entries are missing from pack"];
    (match S.integrity_check ~auto_repair:false rw with
    | Ok `No_error -> Alcotest.fail "Store is corrupted, the check should fail"
    | Error (`Corrupted 3) -> ()
    | _ -> Alcotest.fail "With auto_repair:false should not match");
    (match S.integrity_check ~auto_repair:true rw with
    | Ok (`Fixed 3) -> ()
    | _ -> Alcotest.fail "Integrity check should repair the store");
    (match S.integrity_check ~auto_repair:false rw with
    | Ok `No_error -> ()
    | _ -> Alcotest.fail "Store is repaired, should return Ok");
    S.Repo.close rw
end

module Test_corrupted_inode = struct
  let root_archive, root =
    let open Fpath in
    ( v "test" / "irmin-pack" / "data" / "corrupted_inode" |> to_string,
      v "_build" / "test_integrity_inode" |> to_string )

  let setup_test_env () =
    goto_project_root ();
    rm_dir root;
    let cmd = Filename.quote_command "cp" [ "-R"; "-p"; root_archive; root ] in
    exec_cmd cmd

  let test () =
    setup_test_env ();
    let module S = V1 () in
    let* rw = S.Repo.v (config ~fresh:false root) in
    let get_head c =
      match Irmin.Type.of_string S.Hash.t c with
      | Ok x -> (
          let* commit = S.Commit.of_hash rw x in
          match commit with
          | None -> Alcotest.fail "could not find commit in store"
          | Some x -> Lwt.return [ x ])
      | _ -> Alcotest.fail "could not read hash"
    in
    [%log.app "integrity check of inodes on a store with one corrupted inode"];
    let c2 = "8d89b97726d9fb650d088cb7e21b78d84d132c6e" in
    let* heads = get_head c2 in
    let* result = S.integrity_check_inodes ~heads rw in
    (match result with
    | Ok _ ->
        Alcotest.failf
          "Store is corrupted for second commit, the check should fail"
    | Error _ -> ());
    let c1 = "1b1e259ca4e7bb8dc32c73ade93d8181c29cebe6" in
    let* heads = get_head c1 in
    let* result = S.integrity_check_inodes ~heads rw in
    (match result with
    | Error _ ->
        Alcotest.fail
          "Store is not corrupted for first commit, the check should not fail."
    | Ok _ -> ());
    S.Repo.close rw
end

let tests =
  [
    Alcotest.test_case "Test index reconstruction" `Quick (fun () ->
        Lwt_main.run (Test_reconstruct.test_reconstruct ()));
    Alcotest.test_case "Test integrity check" `Quick (fun () ->
        Lwt_main.run (Test_corrupted_stores.test ()));
    Alcotest.test_case "Test integrity check for inodes" `Quick (fun () ->
        Lwt_main.run (Test_corrupted_inode.test ()));
  ]
