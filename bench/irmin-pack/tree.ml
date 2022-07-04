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

open Bench_common
open Irmin.Export_for_backends
open Irmin_traces

type config = {
  ncommits : int;
  number_of_commits_to_replay : int;
  depth : int;
  nchain_trees : int;
  width : int;
  nlarge_trees : int;
  store_dir : string;
  path_conversion : [ `None | `V1 | `V0_and_v1 | `V0 ];
  inode_config : int * int;
  store_type : [ `Pack | `Pack_mem ];
  freeze_commit : int;
  replay_trace_path : string;
  artefacts_path : string;
  keep_store : bool;
  keep_stat_trace : bool;
  no_summary : bool;
  empty_blobs : bool;
  gc_every : int;
  gc_distance_in_the_past : int;
  gc_wait_after : int;
}

module type Store = sig
  type store_config = config
  type key

  include
    Irmin.Generic_key.KV
      with type Schema.Contents.t = bytes
       and type commit_key = key
       and type node_key = key
       and type contents_key = key

  type on_commit := int -> Hash.t -> unit Lwt.t
  type on_end := unit -> unit Lwt.t

  val create_repo :
    root:string -> store_config -> (Repo.t * on_commit * on_end) Lwt.t

  val gc : repo -> commit_key -> unit Lwt.t
  val finalise_gc : ?wait:bool -> repo -> bool Lwt.t
end

let pp_inode_config ppf (entries, stable_hash) =
  Format.fprintf ppf "[%d, %d]" entries stable_hash

let pp_store_type ppf = function
  | `Pack -> Format.fprintf ppf "[pack store]"
  | `Pack_mem -> Format.fprintf ppf "[pack-mem store]"

module Benchmark = struct
  type result = { time : float; size : int }

  let run config f =
    let+ time, res = with_timer f in
    let size = FSHelper.get_size config.store_dir in
    ({ time; size }, res)

  let pp_results ppf result =
    Format.fprintf ppf "Total time: %f@\nSize on disk: %d M" result.time
      result.size
end

module Bench_suite (Store : Store) = struct
  module Info = Info (Store.Info)

  let init_commit repo =
    Store.Commit.v repo ~info:(Info.f ()) ~parents:[] (Store.Tree.empty ())

  module Key = Store.Backend.Commit.Key
  module Trees = Generate_trees (Store)
  module Trace_replay = Trace_replay.Make (Store)

  let checkout_and_commit repo prev_commit f =
    Store.Commit.of_key repo prev_commit >>= function
    | None -> Lwt.fail_with "commit not found"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        let* tree = f tree in
        Store.Commit.v repo ~info:(Info.f ()) ~parents:[ prev_commit ] tree

  let add_commits ~message repo ncommits on_commit on_end f () =
    with_progress_bar ~message ~n:ncommits ~unit:"commit" @@ fun prog ->
    let* c = init_commit repo in
    let rec aux c i =
      if i >= ncommits then on_end ()
      else
        let* c' = checkout_and_commit repo (Store.Commit.key c) f in
        let* () = on_commit i (Store.Commit.hash c') in
        prog 1;
        aux c' (i + 1)
    in
    aux c 0

  let run_large config =
    reset_stats ();
    let root = config.store_dir in
    let* repo, on_commit, on_end = Store.create_repo ~root config in
    let* result, () =
      Trees.add_large_trees config.width config.nlarge_trees
      |> add_commits ~message:"Playing large mode" repo config.ncommits
           on_commit on_end
      |> Benchmark.run config
    in
    let+ () = Store.Repo.close repo in
    fun ppf ->
      Format.fprintf ppf
        "Large trees mode on inode config %a, %a: %d commits, each consisting \
         of %d large trees of %d entries@\n\
         %a"
        pp_inode_config config.inode_config pp_store_type config.store_type
        config.ncommits config.nlarge_trees config.width Benchmark.pp_results
        result

  let run_chains config =
    reset_stats ();
    let root = config.store_dir in
    let* repo, on_commit, on_end = Store.create_repo ~root config in
    let* result, () =
      Trees.add_chain_trees config.depth config.nchain_trees
      |> add_commits ~message:"Playing chain mode" repo config.ncommits
           on_commit on_end
      |> Benchmark.run config
    in
    let+ () = Store.Repo.close repo in
    fun ppf ->
      Format.fprintf ppf
        "Chain trees mode on inode config %a, %a: %d commits, each consisting \
         of %d chains of depth %d@\n\
         %a"
        pp_inode_config config.inode_config pp_store_type config.store_type
        config.ncommits config.nchain_trees config.depth Benchmark.pp_results
        result

  let run_read_trace config =
    let replay_config : _ Irmin_traces.Trace_replay.config =
      {
        number_of_commits_to_replay = config.number_of_commits_to_replay;
        path_conversion = config.path_conversion;
        inode_config = config.inode_config;
        store_type =
          (config.store_type :> [ `Pack | `Pack_layered | `Pack_mem ]);
        replay_trace_path = config.replay_trace_path;
        artefacts_path = config.artefacts_path;
        keep_store = config.keep_store;
        keep_stat_trace = config.keep_stat_trace;
        empty_blobs = config.empty_blobs;
        return_type = Summary;
        gc_every = config.gc_every;
        gc_distance_in_the_past = config.gc_distance_in_the_past;
        gc_wait_after = config.gc_wait_after;
      }
    in
    if config.no_summary then
      let+ () =
        Trace_replay.run config { replay_config with return_type = Unit }
      in
      fun _ppf -> ()
    else
      let+ summary = Trace_replay.run config replay_config in
      fun ppf ->
        if not config.no_summary then (
          let p = Filename.concat config.artefacts_path "stat_summary.json" in
          Trace_stat_summary.save_to_json summary p;
          Format.fprintf ppf "%a"
            (Trace_stat_summary_pp.pp 5)
            ([ "" ], [ summary ]))
end

module Make_basic (Maker : functor (_ : Irmin_pack.Conf.S) ->
  Irmin_pack.Maker)
(Conf : Irmin_pack.Conf.S) =
struct
  type store_config = config

  module Store = struct
    open Maker (Conf)
    include Make (Irmin_tezos.Schema)
  end

  include Store

  type key = commit_key

  let indexing_strategy = Irmin_pack.Indexing_strategy.minimal

  let create_repo ~root _config =
    let conf =
      Irmin_pack.config ~readonly:false ~fresh:true ~indexing_strategy root
    in
    prepare_artefacts_dir root;
    let* repo = Store.Repo.v conf in
    let on_commit _ _ = Lwt.return_unit in
    let on_end () = Lwt.return_unit in
    Lwt.return (repo, on_commit, on_end)

  let gc repo key =
    let* (_launched : bool) =
      Store.start_gc ~unlink:true ~throttle:`Block repo key
    in
    Lwt.return_unit
end

module Make_store_mem = Make_basic (Irmin_pack_mem.Maker)
module Make_store_pack = Make_basic (Irmin_pack_unix.Maker)

module type B = sig
  val run_large : config -> (Format.formatter -> unit) Lwt.t
  val run_chains : config -> (Format.formatter -> unit) Lwt.t
  val run_read_trace : config -> (Format.formatter -> unit) Lwt.t
end

let store_of_config config =
  let entries', stable_hash' = config.inode_config in
  let module Conf = struct
    include Irmin_tezos.Conf

    let entries = entries'
    let stable_hash = stable_hash'
  end in
  match config.store_type with
  | `Pack -> (module Bench_suite (Make_store_pack (Conf)) : B)
  | `Pack_mem -> (module Bench_suite (Make_store_mem (Conf)) : B)

type suite_elt = {
  mode : [ `Read_trace | `Chains | `Large ];
  speed : [ `Quick | `Slow | `Custom ];
  run : config -> (Format.formatter -> unit) Lwt.t;
}

let suite : suite_elt list =
  [
    {
      mode = `Read_trace;
      speed = `Quick;
      run =
        (fun config ->
          let config =
            { config with inode_config = (32, 256); store_type = `Pack }
          in
          let (module Store) = store_of_config config in
          Store.run_read_trace config);
    };
    {
      mode = `Read_trace;
      speed = `Slow;
      run =
        (fun config ->
          let config =
            { config with inode_config = (32, 256); store_type = `Pack }
          in
          let (module Store) = store_of_config config in
          Store.run_read_trace config);
    };
    {
      mode = `Chains;
      speed = `Quick;
      run =
        (fun config ->
          let config =
            { config with inode_config = (32, 256); store_type = `Pack }
          in
          let (module Store) = store_of_config config in
          Store.run_chains config);
    };
    {
      mode = `Chains;
      speed = `Slow;
      run =
        (fun config ->
          let config =
            { config with inode_config = (2, 5); store_type = `Pack }
          in
          let (module Store) = store_of_config config in
          Store.run_chains config);
    };
    {
      mode = `Large;
      speed = `Quick;
      run =
        (fun config ->
          let config =
            { config with inode_config = (32, 256); store_type = `Pack }
          in
          let (module Store) = store_of_config config in
          Store.run_large config);
    };
    {
      mode = `Large;
      speed = `Slow;
      run =
        (fun config ->
          let config =
            { config with inode_config = (2, 5); store_type = `Pack }
          in
          let (module Store) = store_of_config config in
          Store.run_large config);
    };
    {
      mode = `Read_trace;
      speed = `Custom;
      run =
        (fun config ->
          let (module Store) = store_of_config config in
          Store.run_read_trace config);
    };
  ]

let get_suite suite_filter =
  List.filter
    (fun { mode; speed; _ } ->
      match (suite_filter, speed, mode) with
      | `Slow, `Slow, `Read_trace ->
          (* The suite contains several `Read_trace benchmarks, let's keep the
             slow one only *)
          true
      | `Slow, _, `Read_trace -> false
      | `Slow, (`Slow | `Quick), _ -> true
      | `Quick, `Quick, _ -> true
      | `Custom_trace, `Custom, `Read_trace -> true
      | `Custom_chains, `Custom, `Chains -> true
      | `Custom_large, `Custom, `Large -> true
      | (`Slow | `Quick | `Custom_trace | `Custom_chains | `Custom_large), _, _
        ->
          false)
    suite

let main () ncommits number_of_commits_to_replay suite_filter inode_config
    store_type freeze_commit path_conversion depth width nchain_trees
    nlarge_trees replay_trace_path artefacts_path keep_store keep_stat_trace
    no_summary empty_blobs gc_every gc_distance_in_the_past gc_wait_after =
  let default = match suite_filter with `Quick -> 10000 | _ -> 13315 in
  let number_of_commits_to_replay =
    Option.value ~default number_of_commits_to_replay
  in
  let config =
    {
      ncommits;
      number_of_commits_to_replay;
      store_dir = Filename.concat artefacts_path "store";
      path_conversion;
      depth;
      width;
      nchain_trees;
      nlarge_trees;
      replay_trace_path;
      inode_config;
      store_type;
      freeze_commit;
      artefacts_path;
      keep_store;
      keep_stat_trace;
      no_summary;
      empty_blobs;
      gc_every;
      gc_distance_in_the_past;
      gc_wait_after;
    }
  in
  Printexc.record_backtrace true;
  Random.self_init ();
  (* Enforce the older allocation policy, for consistency of the existing
     results. *)
  Gc.set { (Gc.get ()) with Gc.allocation_policy = 0 };
  FSHelper.rm_dir config.store_dir;
  let suite = get_suite suite_filter in
  let run_benchmarks () = Lwt_list.map_s (fun b -> b.run config) suite in
  let results =
    Lwt_main.run
      (Lwt.finalize run_benchmarks (fun () ->
           if keep_store then (
             [%logs.app "Store kept at %s" config.store_dir];
             let ( / ) = Filename.concat in
             let ro p = if Sys.file_exists p then Unix.chmod p 0o444 in
             ro (config.store_dir / "store.branches");
             ro (config.store_dir / "store.dict");
             ro (config.store_dir / "store.pack");
             ro (config.store_dir / "index" / "data");
             ro (config.store_dir / "index" / "log");
             ro (config.store_dir / "index" / "log_async"))
           else FSHelper.rm_dir config.store_dir;
           Lwt.return_unit))
  in
  [%logs.app "%a@." Fmt.(list ~sep:(any "@\n@\n") (fun ppf f -> f ppf)) results]

open Cmdliner

let mode =
  let mode =
    [
      ("slow", `Slow);
      ("quick", `Quick);
      ("trace", `Custom_trace);
      ("chains", `Custom_chains);
      ("large", `Custom_large);
    ]
  in
  let doc = Arg.info ~doc:(Arg.doc_alts_enum mode) [ "mode" ] in
  Arg.(value @@ opt (Arg.enum mode) `Slow doc)

let inode_config =
  let doc = Arg.info ~doc:"Inode config" [ "inode-config" ] in
  Arg.(value @@ opt (pair int int) (32, 256) doc)

let store_type =
  let mode = [ ("pack", `Pack); ("pack-mem", `Pack_mem) ] in
  let doc = Arg.info ~doc:(Arg.doc_alts_enum mode) [ "store-type" ] in
  Arg.(value @@ opt (Arg.enum mode) `Pack doc)

let freeze_commit =
  let doc =
    Arg.info
      ~doc:"Index of the commit after which to start the layered store freeze."
      [ "freeze-commit" ]
  in
  Arg.(value @@ opt int 1664 doc)

let path_conversion =
  let mode =
    [ ("none", `None); ("v0", `V0); ("v1", `V1); ("v0+v1", `V0_and_v1) ]
  in
  let doc = Arg.info ~doc:(Arg.doc_alts_enum mode) [ "p"; "path-conversion" ] in
  Arg.(value @@ opt (Arg.enum mode) `None doc)

let ncommits =
  let doc =
    Arg.info ~doc:"Number of commits for the large and chain modes."
      [ "n"; "ncommits" ]
  in
  Arg.(value @@ opt int 2 doc)

let number_of_commits_to_replay =
  let doc =
    Arg.info ~doc:"Number of commits to read from trace." [ "ncommits-trace" ]
  in
  Arg.(value @@ opt (some int) None doc)

let keep_store =
  let doc =
    Arg.info ~doc:"Whether or not the irmin store on disk should be kept."
      [ "keep-store" ]
  in
  Arg.(value @@ flag doc)

let no_summary =
  let doc =
    Arg.info
      ~doc:
        "Whether or not the stat trace should be converted to a summary at the \
         end of a replay."
      [ "no-summary" ]
  in
  Arg.(value @@ flag doc)

let keep_stat_trace =
  let doc =
    Arg.info
      ~doc:
        "Whether or not the stat trace should be discarded are the end, after \
         the summary has been saved the disk."
      [ "keep-stat-trace" ]
  in
  Arg.(value @@ flag doc)

let empty_blobs =
  let doc =
    Arg.info
      ~doc:
        "Whether or not the blobs added to the store should be the empty \
         string, during trace replay. This greatly increases the replay speed."
      [ "empty-blobs" ]
  in
  Arg.(value @@ flag doc)

let depth =
  let doc =
    Arg.info ~doc:"Depth of a commit's tree in chains-mode." [ "d"; "depth" ]
  in
  Arg.(value @@ opt int 1000 doc)

let nchain_trees =
  let doc =
    Arg.info ~doc:"Number of chain trees per commit in chains-mode."
      [ "c"; "nchain" ]
  in
  Arg.(value @@ opt int 1 doc)

let width =
  let doc =
    Arg.info ~doc:"Width of a commit's tree in large-mode." [ "w"; "width" ]
  in
  Arg.(value @@ opt int 1000000 doc)

let nlarge_trees =
  let doc =
    Arg.info ~doc:"Number of large trees per commit in large-mode."
      [ "l"; "nlarge" ]
  in
  Arg.(value @@ opt int 1 doc)

let replay_trace_path =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Trace of Tezos operations to be replayed." []
  in
  Arg.(required @@ pos 0 (some string) None doc)

let artefacts_path =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Destination of the bench artefacts."
      [ "artefacts" ]
  in
  Arg.(value @@ opt string default_artefacts_dir doc)

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let gc_every =
  let doc = Arg.info ~doc:"Distance between calls to GC" [ "gc-every" ] in
  Arg.(value @@ opt int 1000 doc)

let gc_distance_in_the_past =
  let doc =
    Arg.info ~doc:"Distance between the GC commit and the latest commit"
      [ "gc-distance-in-the-past" ]
  in
  Arg.(value @@ opt int 5000 doc)

let gc_wait_after =
  let doc =
    Arg.info
      ~doc:
        "How many commits separate the start of a GC and the moment we wait \
         for the end of it"
      [ "gc-wait-after" ]
  in
  Arg.(value @@ opt int 0 doc)

let main_term =
  Term.(
    const main
    $ setup_log
    $ ncommits
    $ number_of_commits_to_replay
    $ mode
    $ inode_config
    $ store_type
    $ freeze_commit
    $ path_conversion
    $ depth
    $ width
    $ nchain_trees
    $ nlarge_trees
    $ replay_trace_path
    $ artefacts_path
    $ keep_store
    $ keep_stat_trace
    $ no_summary
    $ empty_blobs
    $ gc_every
    $ gc_distance_in_the_past
    $ gc_wait_after)

let deprecated_info = (Term.info [@alert "-deprecated"])
let deprecated_exit = (Term.exit [@alert "-deprecated"])
let deprecated_eval = (Term.eval [@alert "-deprecated"])

let () =
  let man =
    [
      `S "DESCRIPTION";
      `P
        "Benchmarks for tree operations. Requires traces of operations, \
         download them (`wget trace.repr`) from:";
      `P
        "Trace with $(b,10310) commits \
         http://data.tarides.com/irmin/data4_10310commits.repr";
      `P
        "Trace with $(b,100066) commits \
         http://data.tarides.com/irmin/data4_100066commits.repr";
      `P
        "Trace with $(b,1343496) commits \
         http://data.tarides.com/irmin/data_1343496commits.repr";
    ]
  in
  let info =
    deprecated_info ~man ~doc:"Benchmarks for tree operations" "tree"
  in
  deprecated_exit @@ deprecated_eval (main_term, info)
