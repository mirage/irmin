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
        >>= S.Commit.v rw ~parents:[] ~info:Irmin.Info.empty
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
  V1_maker (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

module V2 () =
  V2_maker (Irmin.Metadata.None) (Irmin.Contents.String)
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
    let module Kind = Irmin_pack.Pack_value.Kind in
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
      (fun k (offset, length, kind) ->
        Log.debug (fun l ->
            l "index find k = %a (off, len, kind) = (%a, %d, %a)"
              (Irmin.Type.pp Hash.t) k Int63.pp offset length Kind.pp kind);
        match Index.find index_new k with
        | Some (offset', length', kind') ->
            Alcotest.(check int63) "check offset" offset offset';
            Alcotest.(check int) "check length" length length';
            Alcotest.(check_repr Kind.t) "check kind" kind kind'
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

module Config_layered_store = struct
  (** the empty store is to simulate the scenario where upper0 is non existing.
      TODO make the test pass with an actual non existing upper0. *)
  let root_v1_archive, empty_store, root_v1, upper1, upper0, lower =
    let open Fpath in
    ( v "test" / "irmin-pack" / "data" / "version_1" |> to_string,
      v "test" / "irmin-pack" / "data" / "empty_store" |> to_string,
      v "_build" / "test_layers_migrate_1_to_2" |> to_string,
      v "_build" / "test_layers_migrate_1_to_2" / "upper1" |> to_string,
      v "_build" / "test_layers_migrate_1_to_2" / "upper0" |> to_string,
      v "_build" / "test_layers_migrate_1_to_2" / "lower" |> to_string )

  let setup_test_env () =
    goto_project_root ();
    rm_dir root_v1;
    let cmd = Filename.quote_command "mkdir" [ root_v1 ] in
    exec_cmd cmd;
    let cmd =
      Filename.quote_command "cp" [ "-R"; "-p"; root_v1_archive; upper1 ]
    in
    exec_cmd cmd;
    let cmd = Filename.quote_command "cp" [ "-R"; "-p"; empty_store; upper0 ] in
    exec_cmd cmd;
    let cmd =
      Filename.quote_command "cp" [ "-R"; "-p"; root_v1_archive; lower ]
    in
    exec_cmd cmd
end

module Make_layered =
  Irmin_pack_layered.Maker (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

module Test_layered_store = Test (Make_layered) (Config_layered_store)

module Test_corrupted_stores = struct
  let root_archive, root, root_layers, upper1, upper0, lower =
    let open Fpath in
    ( v "test" / "irmin-pack" / "data" / "corrupted" |> to_string,
      v "_build" / "test_integrity" |> to_string,
      v "_build" / "test_integrity_layers" |> to_string,
      v "_build" / "test_integrity_layers" / "upper1" |> to_string,
      v "_build" / "test_integrity_layers" / "upper0" |> to_string,
      v "_build" / "test_integrity_layers" / "lower" |> to_string )

  let setup_test_env () =
    goto_project_root ();
    rm_dir root;
    let cmd = Filename.quote_command "cp" [ "-R"; "-p"; root_archive; root ] in
    exec_cmd cmd

  let setup_test_env_layered_store () =
    rm_dir root_layers;
    let copy_root_archive dst =
      Filename.quote_command "cp" [ "-R"; "-p"; root_archive; dst ] |> exec_cmd
    in
    let cmd = Filename.quote_command "mkdir" [ root_layers ] in
    exec_cmd cmd;
    copy_root_archive upper1;
    copy_root_archive upper0;
    copy_root_archive lower

  let test () =
    setup_test_env ();
    let module S = V2 () in
    let* rw = S.Repo.v (config ~fresh:false root) in
    Log.app (fun l ->
        l "integrity check on a store where 3 entries are missing from pack");
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

  let test_layered_store () =
    setup_test_env_layered_store ();
    let module S = Make_layered in
    let* rw = S.Repo.v (config ~fresh:false root_layers) in
    Log.app (fun l ->
        l
          "integrity check on a layered store where 3 entries are missing from \
           the pack file of each layer");
    S.integrity_check ~auto_repair:false rw
    |> List.iter (function
         | Ok `No_error, _ ->
             Alcotest.fail "Store is corrupted, the check should fail"
         | Error (`Corrupted 3), _ -> ()
         | _ -> Alcotest.fail "With auto_repair:false should not match");
    S.integrity_check ~auto_repair:true rw
    |> List.iter (function
         | Ok (`Fixed 3), _ -> ()
         | _ -> Alcotest.fail "Integrity check should repair the store");
    S.integrity_check ~auto_repair:false rw
    |> List.iter (function
         | Ok `No_error, _ -> ()
         | _ -> Alcotest.fail "Store is repaired, should return Ok");
    S.Repo.close rw

  let empty_store, root, lock_file =
    let open Fpath in
    ( v "test" / "irmin-pack" / "data" / "empty_store" |> to_string,
      v "_build" / "test_freeze_lock" |> to_string,
      v "_build" / "test_freeze_lock" / "lock" |> to_string )

  let setup_test () =
    goto_project_root ();
    rm_dir root;
    let cmd = Filename.quote_command "cp" [ "-R"; "-p"; empty_store; root ] in
    exec_cmd cmd;
    let cmd = Filename.quote_command "touch" [ lock_file ] in
    exec_cmd cmd

  let test_freeze_lock () =
    setup_test ();
    let module S = Make_layered in
    let add_commit repo k v =
      S.Tree.add S.Tree.empty k v
      >>= S.Commit.v repo ~parents:[] ~info:Irmin.Info.empty
    in
    let check_commit repo commit k v =
      commit |> S.Commit.hash |> S.Commit.of_hash repo >>= function
      | None ->
          Alcotest.failf "Commit `%a' is dangling in repo" S.Commit.pp_hash
            commit
      | Some commit ->
          let tree = S.Commit.tree commit in
          S.Tree.find tree k
          >|= Alcotest.(check (option string))
                (Fmt.strf "Expected binding [%a ↦ %s]"
                   Fmt.(Dump.list string)
                   k v)
                (Some v)
    in
    let check_upper repo msg exp =
      let got = S.Private_layer.upper_in_use repo in
      let cast x = (x :> [ `Upper0 | `Upper1 | `Lower ]) in
      if not (got = exp) then
        Alcotest.failf "%s expected %a got %a" msg Irmin_layers.Layer_id.pp
          (cast exp) Irmin_layers.Layer_id.pp (cast got)
    in
    let* rw = S.Repo.v (config ~fresh:false root) in
    let* ro = S.Repo.v (config ~fresh:false ~readonly:true root) in
    Log.app (fun l -> l "Open a layered store aborted during a freeze");
    Alcotest.(check bool) "Store needs recovery" true (S.needs_recovery rw);
    check_upper rw "Upper before recovery" `Upper1;
    let* c = add_commit rw [ "a" ] "x" in
    S.sync ro;
    check_commit ro c [ "a" ] "x" >>= fun () ->
    Log.app (fun l -> l "Freeze with recovery flag set");
    let* () =
      S.freeze ~recovery:true ~max_lower:[ c ] rw >>= fun () ->
      S.Private_layer.wait_for_freeze rw
    in
    Alcotest.(check bool)
      "Store doesn't need recovery" false (S.needs_recovery rw);
    check_upper rw "Upper after recovery" `Upper0;
    check_commit rw c [ "a" ] "x" >>= fun () ->
    S.sync ro;
    check_commit ro c [ "a" ] "x" >>= fun () ->
    check_upper ro "RO upper after recovery" `Upper0;
    let* c = add_commit rw [ "b" ] "y" in
    Log.app (fun l ->
        l
          "If recovery flag is set, freeze proceeds with recovery even when it \
           isn't needed");
    let* () =
      S.freeze ~recovery:true ~max_lower:[ c ] rw >>= fun () ->
      S.Private_layer.wait_for_freeze rw
    in
    check_upper rw "Upper after freeze" `Upper1;
    check_commit rw c [ "b" ] "y" >>= fun () ->
    S.sync ro;
    check_upper ro "RO upper after freeze" `Upper1;
    let* () = check_commit ro c [ "b" ] "y" in
    S.Repo.close rw >>= fun () -> S.Repo.close ro
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
    Alcotest.test_case "Test layered store migration V1 to V2" `Quick (fun () ->
        Lwt_main.run (Test_layered_store.v1_to_v2 ()));
    Alcotest.test_case "Test integrity check" `Quick (fun () ->
        Lwt_main.run (Test_corrupted_stores.test ()));
    Alcotest.test_case "Test integrity check on layered stores" `Quick
      (fun () -> Lwt_main.run (Test_corrupted_stores.test_layered_store ()));
    Alcotest.test_case "Test freeze lock on layered stores" `Quick (fun () ->
        Lwt_main.run (Test_corrupted_stores.test_freeze_lock ()));
    Alcotest.test_case "Test integrity check for inodes" `Quick (fun () ->
        Lwt_main.run (Test_corrupted_inode.test ()));
  ]
