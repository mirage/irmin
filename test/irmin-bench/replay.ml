open! Import

let test_dir = Filename.concat "_build" "test-pack-trace-replay"

let testable t =
  Alcotest.testable (Irmin.Type.pp_dump t) Irmin.Type.(unstage (equal t))

let check t = Alcotest.check (testable t)

module Conf = struct
  let entries = 32
  let stable_hash = 256
end

module Store = struct
  type store_config = string

  module Store = Irmin_tezos.Store

  let create_repo store_dir =
    let conf = Irmin_pack.config ~readonly:false ~fresh:true store_dir in
    let* repo = Store.Repo.v conf in
    let on_commit _ _ = Lwt.return_unit in
    let on_end () = Lwt.return_unit in
    let pp _ = () in
    Lwt.return (repo, on_commit, on_end, pp)

  include Store
end

module Replay = Irmin_traces.Trace_replay.Make (Store)

let rec repeat = function
  | 0 -> fun _f x -> x
  | n -> fun f x -> f (repeat (n - 1) f x)

(** The current working directory depends on whether the test binary is directly
    run or is triggered with [dune exec], [dune runtest]. We normalise by
    switching to the project root first. *)
let goto_project_root () =
  let cwd = Fpath.v (Sys.getcwd ()) in
  match cwd |> Fpath.segs |> List.rev with
  | "irmin-bench" :: "test" :: "default" :: "_build" :: _ ->
      Printf.eprintf "case a\n%!";
      let root = cwd |> repeat 4 Fpath.parent in
      Unix.chdir (Fpath.to_string root)
  | _ ->
      Printf.eprintf "case b\n%!";
      ()

let replay_1_commit () =
  Printf.eprintf "hello\n%!";
  goto_project_root ();
  let trace_path =
    let open Fpath in
    v "test" / "irmin-bench" / "data" / "tezos_actions_1commit.repr"
    |> to_string
  in
  assert (Sys.file_exists trace_path);
  if Sys.file_exists test_dir then (
    let cmd = Printf.sprintf "rm -rf %s" test_dir in
    [%logs.debug "exec: %s\n%!" cmd];
    let _ = Sys.command cmd in
    ());

  let store_dir = Filename.concat test_dir "store" in
  let replay_config : Irmin_traces.Trace_replay.config =
    {
      ncommits_trace = 1;
      store_dir;
      path_conversion = `None;
      inode_config = (Conf.entries, Conf.stable_hash);
      store_type = `Pack;
      commit_data_file = trace_path;
      artefacts_dir = test_dir;
      keep_store = false;
      keep_stat_trace = false;
      no_summary = false;
      empty_blobs = false;
    }
  in
  let+ pp_result = Replay.run store_dir replay_config in
  [%logs.debug "%t" pp_result];
  let got = Irmin_pack.Stats.get () in
  let expected =
    Irmin_pack.Stats.
      {
        finds = 2;
        cache_misses = 0;
        appended_hashes = 0;
        appended_offsets = 4;
        inode_add = 0;
        inode_remove = 0;
        inode_of_seq = 4;
        inode_of_raw = 2;
        inode_rec_add = 8;
        inode_rec_remove = 0;
        inode_to_binv = 2;
        inode_decode_bin = 0;
        inode_encode_bin = 2;
      }
  in
  check Irmin_pack.Stats.t "Pack counters" expected got;
  ()

module Store_mem = struct
  type store_config = string

  module Maker = Irmin_pack_mem.Maker (Conf)
  module Store = Maker.Make (Irmin_tezos.Schema)

  let create_repo store_dir =
    let conf = Irmin_pack.config ~readonly:false ~fresh:true store_dir in
    let* repo = Store.Repo.v conf in
    let on_commit _ _ = Lwt.return_unit in
    let on_end () = Lwt.return_unit in
    let pp _ = () in
    Lwt.return (repo, on_commit, on_end, pp)

  include Store
end

module Replay_mem = Irmin_traces.Trace_replay.Make (Store_mem)

let replay_1_commit_mem () =
  goto_project_root ();
  let trace_path =
    let open Fpath in
    v "test" / "irmin-bench" / "data" / "tezos_actions_1commit.repr"
    |> to_string
  in
  assert (Sys.file_exists trace_path);
  if Sys.file_exists test_dir then (
    let cmd = Printf.sprintf "rm -rf %s" test_dir in
    [%logs.debug "exec: %s\n%!" cmd];
    let _ = Sys.command cmd in
    ());

  let store_dir = Filename.concat test_dir "store_mem" in
  let replay_config : Irmin_traces.Trace_replay.config =
    {
      ncommits_trace = 1;
      store_dir;
      path_conversion = `None;
      inode_config = (Conf.entries, Conf.stable_hash);
      store_type = `Pack;
      commit_data_file = trace_path;
      artefacts_dir = test_dir;
      keep_store = false;
      keep_stat_trace = false;
      no_summary = false;
      empty_blobs = false;
    }
  in
  let+ pp_result = Replay_mem.run store_dir replay_config in
  [%logs.debug "%t" pp_result];
  ()

let test_cases =
  [
    ( "replay",
      [
        Alcotest.test_case "replay_1_commit" `Quick (fun () ->
            Lwt_main.run (replay_1_commit ()));
        Alcotest.test_case "replay_1_commit_in_memory" `Quick (fun () ->
            Lwt_main.run (replay_1_commit_mem ()));
      ] );
  ]
