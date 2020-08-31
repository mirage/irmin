open Lwt.Infix
open Common

let ( let* ) x f = Lwt.bind x f

let src = Logs.Src.create "tests.migration" ~doc:"Test migrations"

module Log = (val Logs.src_log src : Logs.LOG)

module Hash = Irmin.Hash.SHA1
module Make () =
  Irmin_pack.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

module S = Make ()

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

let root_v1_archive, root_v1 =
  let open Fpath in
  ( v "test" / "irmin-pack" / "data" / "version_1" |> to_string,
    v "_build" / "test_pack_migrate_1_to_2" |> to_string )

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

let archive : (S.branch * (S.key * S.contents) list) list =
  [
    ("bar", [ ([ "a"; "d" ], "x"); ([ "a"; "b"; "c" ], "z") ]);
    ("foo", [ ([ "b" ], "y") ]);
  ]

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

let v1_to_v2 () =
  setup_test_env ();
  let conf_ro, conf_rw =
    pair_map
      (fun readonly -> config ~readonly ~fresh:false root_v1)
      (true, false)
  in
  let* () =
    Alcotest.check_raises_lwt "Opening a V1 store should fail"
      (Irmin_pack.Unsupported_version `V1)
      (fun () -> S.Repo.v conf_ro)
  in
  Alcotest.check_raises "Migrating with RO config should fail"
    Irmin_pack.RO_Not_Allowed (fun () -> ignore (S.migrate conf_ro));
  Log.app (fun m -> m "Running the migration with a RW config");
  S.migrate conf_rw;
  Log.app (fun m -> m "Checking migration is idempotent (cached instance)");
  S.migrate conf_rw;
  let () =
    let module S = Make () in
    Log.app (fun m -> m "Checking migration is idempotent (uncached instance)");
    S.migrate conf_rw
  in
  let* () =
    Log.app (fun m ->
        m
          "Checking old bindings are still reachable post-migration (cached RO \
           instance)");
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
    let* () = check_commit rw new_commit [ ([ "c" ], "x") ] in
    let* () = S.Repo.close rw in
    Lwt.return new_commit
  in
  let* () =
    Log.app (fun m ->
        m "Checking all values can be read from an uncached instance");
    let module S = Make () in
    let* ro = S.Repo.v conf_ro in
    let* () = check_repo ro archive in
    let* () = check_commit ro new_commit [ ([ "c" ], "x") ] in
    S.Repo.close ro
  in
  Lwt.return_unit

let tests =
  [
    Alcotest.test_case "Test migration V1 to V2" `Quick (fun () ->
        Lwt_main.run (v1_to_v2 ()));
  ]
