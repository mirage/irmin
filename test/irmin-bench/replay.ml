open! Import

let test_dir = Filename.concat "_build" "test-pack-trace-replay"

module Conf = struct
  let entries = 32
  let stable_hash = 256
end

module Store = struct
  type store_config = string

  open Tezos_context_hash_irmin.Encoding

  module Maker =
    Irmin_pack.Maker_ext (Irmin_pack.Version.V1) (Conf) (Node) (Commit)

  module Store = Maker.Make (Metadata) (Contents) (Path) (Branch) (Hash)

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
    Logs.debug (fun l -> l "exec: %s\n%!" cmd);
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
  Logs.debug (fun l -> l "%t" pp_result);
  ()

module Store_mem = struct
  type store_config = string

  open Tezos_context_hash_irmin.Encoding
  module Maker = Irmin_pack_mem.Maker (Node) (Commit) (Conf)
  module Store = Maker.Make (Metadata) (Contents) (Path) (Branch) (Hash)

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
    Logs.debug (fun l -> l "exec: %s\n%!" cmd);
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
  Logs.debug (fun l -> l "%t" pp_result);
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
