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

let info () = Irmin.Info.empty

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
  Log.info (fun l -> l "exec: %s\n%!" cmd);
  match Sys.command cmd with
  | 0 -> ()
  | n ->
      Fmt.failwith
        "Failed to set up the test environment: command `%s' exited with \
         non-zero exit code %d"
        cmd n

module type Migrate_store = sig
  include
    Irmin.S
      with type step = string
       and type key = string list
       and type contents = string
       and type branch = string

  val migrate : Irmin.config -> unit
end

module Test
    (S : Migrate_store) (Config : sig
      val setup_test_env : unit -> unit
      val root_v1 : string
    end) =
struct
  let check_commit repo commit bindings =
    commit |> S.Commit.hash |> S.Commit.of_hash repo >>= function
    | None ->
        Alcotest.failf "Commit `%a' is dangling in repo" S.Commit.pp_hash commit
    | Some commit ->
        let tree = S.Commit.tree commit in
        bindings
        |> Lwt_list.iter_s (fun (key, value) ->
               S.Tree.find tree key
               >|= Alcotest.(check (option string))
                     (Fmt.strf "Expected binding [%a ↦ %s]"
                        Fmt.(Dump.list string)
                        key value)
                     (Some value))

  let check_repo repo structure =
    structure
    |> Lwt_list.iter_s @@ fun (branch, bindings) ->
       S.Branch.find repo branch >>= function
       | None -> Alcotest.failf "Couldn't find expected branch `%s'" branch
       | Some commit -> check_commit repo commit bindings

  let pair_map f (a, b) = (f a, f b)

  let v1_to_v2 ?(uncached_instance_check_idempotent = fun () -> ())
      ?(uncached_instance_check_commit = fun _ -> Lwt.return_unit) () =
    Config.setup_test_env ();
    let conf_ro, conf_rw =
      pair_map
        (fun readonly -> config ~readonly ~fresh:false Config.root_v1)
        (true, false)
    in
    let* () =
      Alcotest.check_raises_lwt "Opening a V1 store should fail"
        Irmin_pack.Version.(Invalid { expected = `V2; found = `V1 })
        (fun () -> S.Repo.v conf_ro)
    in
    Alcotest.check_raises "Migrating with RO config should fail"
      Irmin_pack.RO_not_allowed (fun () -> ignore (S.migrate conf_ro));
    Log.app (fun m -> m "Running the migration with a RW config");
    S.migrate conf_rw;
    Log.app (fun m -> m "Checking migration is idempotent (cached instance)");
    S.migrate conf_rw;
    uncached_instance_check_idempotent ();
    let* () =
      Log.app (fun m ->
          m
            "Checking old bindings are still reachable post-migration (cached \
             RO instance)");
      let* r = S.Repo.v conf_ro in
      check_repo r archive
    in
    let* new_commit =
      let* rw = S.Repo.v conf_rw in
      Log.app (fun m -> m "Checking new commits can be added to the V2 store");
      let* new_commit =
        S.Tree.add S.Tree.empty [ "c" ] "x"
        >>= S.Commit.v rw ~parents:[] ~info:(info ())
      in
      check_commit rw new_commit [ ([ "c" ], "x") ] >>= fun () ->
      let+ () = S.Repo.close rw in
      new_commit
    in
    uncached_instance_check_commit new_commit
end

module Config_store = struct
  let root_v1_archive, root_v1, tmp =
    let open Fpath in
    ( v "test" / "irmin-pack" / "data" / "version_1" |> to_string,
      v "_build" / "test_pack_migrate_1_to_2" |> to_string,
      v "_build" / "test_index_reconstruct" |> to_string )

  let setup_test_env () =
    goto_project_root ();
    rm_dir root_v1;
    let cmd =
      Filename.quote_command "cp" [ "-R"; "-p"; root_v1_archive; root_v1 ]
    in
    Log.info (fun l -> l "exec: %s\n%!" cmd);
    match Sys.command cmd with
    | 0 -> ()
    | n ->
        Fmt.failwith
          "Failed to set up the test environment: command `%s' exited with \
           non-zero exit code %d"
          cmd n
end

module Hash = Irmin.Hash.SHA1

module V1_maker = Irmin_pack.V1 (struct
  let entries = 2
  let stable_hash = 3
end)

module V2_maker = Irmin_pack.V2 (Conf)

module V1 () =
  V1_maker.Make (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

module V2 () =
  V2_maker.Make (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

module Test_store = struct
  module S = V2 ()
  include Test (S) (Config_store)

  let uncached_instance_check_idempotent () =
    Log.app (fun m -> m "Checking migration is idempotent (uncached instance)");
    let module S = V2 () in
    let conf = config ~readonly:false ~fresh:false Config_store.root_v1 in
    S.migrate conf

  let uncached_instance_check_commit new_commit =
    Log.app (fun m ->
        m "Checking all values can be read from an uncached instance");
    let module S = V2 () in
    let conf = config ~readonly:true ~fresh:false Config_store.root_v1 in
    let* ro = S.Repo.v conf in
    check_repo ro archive >>= fun () ->
    check_commit ro new_commit [ ([ "c" ], "x") ] >>= fun () -> S.Repo.close ro

  let v1_to_v2 =
    v1_to_v2 ~uncached_instance_check_idempotent ~uncached_instance_check_commit
end

module Test_reconstruct = struct
  module S = V2 ()
  include Test (S) (Config_store)

  let setup_test_env () =
    Config_store.setup_test_env ();
    rm_dir Config_store.tmp;
    let cmd =
      Filename.quote_command "cp"
        [ "-R"; "-p"; Config_store.root_v1; Config_store.tmp ]
    in
    Log.info (fun l -> l "exec: %s\n%!" cmd);
    match Sys.command cmd with
    | 0 -> ()
    | n ->
        Fmt.failwith
          "Failed to set up the test environment: command `%s' exited with \
           non-zero exit code %d"
          cmd n

  let test_reconstruct () =
    setup_test_env ();
    let conf = config ~readonly:false ~fresh:false Config_store.root_v1 in
    S.migrate conf;
    S.reconstruct_index conf;
    let index_old =
      Index.v ~fresh:false ~readonly:false ~log_size:500_000 Config_store.tmp
    in
    let index_new =
      Index.v ~fresh:false ~readonly:false ~log_size:500_000
        Config_store.root_v1
    in
    Index.iter
      (fun k (offset, length, magic) ->
        Log.debug (fun l ->
            l "index find k = %a (off, len, magic) = (%a, %d, %c)"
              (Irmin.Type.pp Hash.t) k Int63.pp offset length magic);
        match Index.find index_new k with
        | Some (offset', length', magic') ->
            Alcotest.(check int63) "check offset" offset offset';
            Alcotest.(check int) "check length" length length';
            Alcotest.(check char) "check magic" magic magic'
        | None ->
            Alcotest.failf "expected to find hash %a" (Irmin.Type.pp Hash.t) k)
      index_old;
    Index.close index_old;
    Index.close index_new;
    Log.app (fun m ->
        m "Checking old bindings are still reachable post index reconstruction)");
    let* r = S.Repo.v conf in
    check_repo r archive >>= fun () -> S.Repo.close r
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
      match Irmin.Type.of_string Hash.t c with
      | Ok x -> (
          let* commit = S.Commit.of_hash rw x in
          match commit with
          | None -> Alcotest.fail "could not find commit in store"
          | Some x -> Lwt.return [ x ])
      | _ -> Alcotest.fail "could not read hash"
    in
    Log.app (fun l ->
        l "integrity check of inodes on a store with one corrupted inode");
    let c2 = "8d89b97726d9fb650d088cb7e21b78d84d132c6e" in
    let* heads = get_head c2 in
    let* result = S.integrity_check_inodes ~heads rw in
    (match result with
    | Ok (`Msg msg) ->
        Alcotest.failf
          "Store is corrupted for second commit, the check should fail %s" msg
    | Error _ -> ());
    let c1 = "1b1e259ca4e7bb8dc32c73ade93d8181c29cebe6" in
    let* heads = get_head c1 in
    let* result = S.integrity_check_inodes ~heads rw in
    (match result with
    | Error (`Msg msg) ->
        Alcotest.failf
          "Store is not corrupted for first commit, the check should not fail \
           %s"
          msg
    | Ok _ -> ());
    S.Repo.close rw
end

let tests =
  [
    Alcotest.test_case "Test migration V1 to V2" `Quick (fun () ->
        Lwt_main.run (Test_store.v1_to_v2 ()));
    Alcotest.test_case "Test index reconstuction" `Quick (fun () ->
        Lwt_main.run (Test_reconstruct.test_reconstruct ()));
    Alcotest.test_case "Test integrity check for inodes" `Quick (fun () ->
        Lwt_main.run (Test_corrupted_inode.test ()));
  ]
