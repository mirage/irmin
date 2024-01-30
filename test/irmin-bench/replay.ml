open! Import

let test_dir = Filename.concat "_build" "test-pack-trace-replay"

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Irmin_test.reporter ())

module Conf = Irmin_tezos.Conf

module Store = struct
  type store_config = unit

  module Store = Irmin_tezos.Store
  include Store

  type key = commit_key

  let create_repo ~sw ~root () =
    (* make sure the parent dir exists *)
    let () =
      match Sys.file_exists (Filename.dirname root) with
      | false -> Unix.mkdir (Filename.dirname root) 0o755
      | true -> ()
    in
    let conf = Irmin_pack.config ~readonly:false ~fresh:true root in
    let repo = Store.Repo.v ~sw conf in
    let on_commit _ _ = () in
    let on_end () = () in
    (repo, on_commit, on_end)

  let gc_wait repo =
    let r = Store.Gc.wait repo in
    match r with Ok _ -> () | Error (`Msg err) -> failwith err

  let gc_run ?(finished = fun _ -> ()) repo key =
    let f (result : (_, Store.Gc.msg) result) =
      match result with
      | Error (`Msg err) -> finished @@ Error err
      | Ok stats -> finished @@ Ok stats
    in
    let launched = Store.Gc.run ~finished:f repo key in
    match launched with
    | Ok true -> ()
    | Ok false -> [%logs.app "GC skipped"]
    | Error (`Msg err) -> failwith err
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
      let root = cwd |> repeat 4 Fpath.parent in
      Unix.chdir (Fpath.to_string root)
  | _ -> ()

let setup_env () =
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
  trace_path

let replay_1_commit () =
  let trace_path = setup_env () in
  let replay_config : _ Replay.config =
    {
      number_of_commits_to_replay = 1;
      path_conversion = `None;
      inode_config = (Conf.entries, Conf.stable_hash);
      store_type = `Pack;
      replay_trace_path = trace_path;
      artefacts_path = test_dir;
      keep_store = false;
      keep_stat_trace = false;
      empty_blobs = false;
      return_type = Summary;
      gc_every = 0;
      gc_distance_in_the_past = 0;
      gc_wait_after = 0;
      add_volume_every = 0;
    }
  in
  let summary = Replay.run () replay_config in
  [%logs.debug
    "%a" (Irmin_traces.Trace_stat_summary_pp.pp 5) ([ "" ], [ summary ])];
  let check name = Alcotest.(check int) ("Stats_counters" ^ name) in
  let pack_got = Irmin_pack.Stats.get () in
  let unix_got = Irmin_pack_unix.Stats.get () in
  let pack_store =
    Irmin_pack_unix.Stats.(Pack_store.export unix_got.pack_store)
  in
  let inode = Irmin_pack.Stats.(Inode.export pack_got.inode) in
  check "appended_hashes" pack_store.appended_hashes 0;
  check "appended_offsets" pack_store.appended_offsets 5;
  check "total" pack_store.total 2;
  check "from_staging" pack_store.from_staging 0;
  check "from_lru" pack_store.from_lru 2;
  check "from_pack_direct" pack_store.from_pack_direct 0;
  check "from_pack_indexed" pack_store.from_pack_indexed 0;
  check "inode_add" inode.inode_add 0;
  check "inode_remove" inode.inode_remove 0;
  check "inode_of_seq" inode.inode_of_seq 2;
  check "inode_of_raw" inode.inode_of_raw 2;
  check "inode_rec_add" inode.inode_rec_add 0;
  check "inode_rec_remove" inode.inode_rec_remove 0;
  check "inode_to_binv" inode.inode_to_binv 2;
  check "inode_decode_bin" inode.inode_decode_bin 0;
  check "inode_encode_bin" inode.inode_encode_bin 2

module Store_mem = struct
  type store_config = unit

  module Maker = Irmin_pack_mem.Maker (Conf)
  module Store = Maker.Make (Irmin_tezos.Schema)
  include Store

  type key = commit_key

  let create_repo ~sw ~root () =
    let conf = Irmin_pack.config ~readonly:false ~fresh:true root in
    let repo = Store.Repo.v ~sw conf in
    let on_commit _ _ = () in
    let on_end () = () in
    (repo, on_commit, on_end)

  let split _repo = ()
  let add_volume _repo = ()
  let gc_wait _repo = ()
  let gc_run ?finished:_ _repo _key = ()
end

module Replay_mem = Irmin_traces.Trace_replay.Make (Store_mem)

let replay_1_commit_mem () =
  let trace_path = setup_env () in
  let replay_config : _ Irmin_traces.Trace_replay.config =
    {
      number_of_commits_to_replay = 1;
      path_conversion = `None;
      inode_config = (Conf.entries, Conf.stable_hash);
      store_type = `Pack;
      replay_trace_path = trace_path;
      artefacts_path = test_dir;
      keep_store = false;
      keep_stat_trace = false;
      empty_blobs = false;
      return_type = Summary;
      gc_every = 0;
      gc_distance_in_the_past = 0;
      gc_wait_after = 0;
      add_volume_every = 0;
    }
  in
  let summary = Replay_mem.run () replay_config in
  [%logs.debug
    "%a" (Irmin_traces.Trace_stat_summary_pp.pp 5) ([ "" ], [ summary ])];
  ()

let test_cases =
  let tc msg f = Alcotest.test_case msg `Quick f in
  [
    ( "replay",
      [
        tc "replay_1_commit" replay_1_commit;
        tc "replay_1_commit_in_memory" replay_1_commit_mem;
      ] );
  ]
