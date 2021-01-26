open! Import
open Bench_common

let reset_stats () =
  reset_stats ();
  Irmin_layers.Stats.reset_stats ()

let () = Random.self_init ()

type config = {
  ncommits : int;
  ncycles : int;
  depth : int;
  root : string;
  clear : bool;
  no_freeze : bool;
  show_stats : bool;
  merge_throttle : Irmin_pack.Config.merge_throttle;
  freeze_throttle : Irmin_pack.Config.freeze_throttle;
}
[@@deriving repr]

module Hash = Irmin.Hash.SHA1

module Store =
  Irmin_pack_layered.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

let configure_store root merge_throttle freeze_throttle =
  let conf =
    Irmin_pack.config ~readonly:false ~fresh:true ~freeze_throttle
      ~merge_throttle root
  in
  Irmin_pack_layered.config ~conf ~with_lower:false ~blocking_copy_size:1000
    ~copy_in_upper:true ()

let init config =
  FSHelper.rm_dir config.root;
  Memtrace.trace_if_requested ();
  reset_stats ()

module Trees = Generate_trees (Store)

let init_commit repo =
  Store.Commit.v repo ~info:(info ()) ~parents:[] Store.Tree.empty

let checkout_and_commit config repo c nb =
  Store.Commit.of_hash repo c >>= function
  | None -> Lwt.fail_with "commit not found"
  | Some commit ->
      let tree = Store.Commit.tree commit in
      let* tree =
        if nb mod 1000 = 0 then Trees.add_large_trees 256 2 tree
        else Trees.add_chain_trees config.depth (config.depth / 2) tree
      in
      Store.Commit.v repo ~info:(info ()) ~parents:[ c ] tree

let total = ref 0

let print_commit_stats config c i time =
  let num_objects = Irmin_layers.Stats.get_adds () in
  total := !total + num_objects;
  Irmin_layers.Stats.reset_adds ();
  if config.show_stats then
    Logs.app (fun l ->
        l "Commit %a %d in cycle completed in %f; objects created: %d"
          Store.Commit.pp_hash c i time num_objects)

let print_stats config =
  let t = Irmin_layers.Stats.get () in
  let copied_objects =
    List.map2 (fun x y -> x + y) t.copied_contents t.copied_commits
    |> List.map2 (fun x y -> x + y) t.copied_nodes
    |> List.map2 (fun x y -> x + y) t.copied_branches
  in
  if config.show_stats then (
    Logs.app (fun l ->
        l
          "Irmin-layers stats: nb_freeze=%d copied_objects=%a \
           waiting_freeze=%a completed_freeze=%a \
           objects_added_in_upper_since_last_freeze=%d"
          t.nb_freeze
          Fmt.(Dump.list int)
          copied_objects
          Fmt.(Dump.list float)
          t.waiting_freeze
          Fmt.(Dump.list float)
          t.completed_freeze !total);
    total := 0)

let write_cycle config repo init_commit =
  let rec go c i =
    if i = config.ncommits then Lwt.return c
    else
      let* time, c' =
        with_timer (fun () ->
            checkout_and_commit config repo (Store.Commit.hash c) i)
      in
      print_commit_stats config c' i time;
      go c' (i + 1)
  in
  go init_commit 0

let freeze ~min_upper ~max config repo =
  if config.no_freeze then Lwt.return_unit
  else Store.freeze ~max ~min_upper repo

let min_uppers = Queue.create ()
let add_min c = Queue.add c min_uppers
let consume_min () = Queue.pop min_uppers

let first_5_cycles config repo =
  let* c = init_commit repo in
  print_commit_stats config c 0 0.0;
  let rec aux i c =
    add_min c;
    if i > 4 then Lwt.return c
    else write_cycle config repo c >>= fun c -> aux (i + 1) c
  in
  aux 0 c

let run_cycles config repo head =
  let rec run_one_cycle head i =
    if i = config.ncycles then Lwt.return head
    else
      let* max = write_cycle config repo head in
      print_stats config;
      let min = consume_min () in
      add_min max;
      let* time, () =
        with_timer (fun () ->
            freeze ~min_upper:[ min ] ~max:[ max ] config repo)
      in
      if config.show_stats then
        Logs.app (fun l -> l "call to freeze completed in %f" time);
      run_one_cycle max (i + 1)
  in
  run_one_cycle head 0

let rw config =
  let conf =
    configure_store config.root config.merge_throttle config.freeze_throttle
  in
  Store.Repo.v conf

let close config repo =
  let+ t, () = with_timer (fun () -> Store.Repo.close repo) in
  if config.show_stats then Logs.app (fun l -> l "close %f" t)

let run config =
  let* repo = rw config in
  let* c = first_5_cycles config repo in
  let* _ = run_cycles config repo c in
  close config repo >|= fun () ->
  if config.show_stats then (
    Fmt.epr "After freeze thread finished : ";
    FSHelper.print_size_layers config.root)

type result = {
  total_time : float;
  time_per_commit : float;
  commits_per_sec : float;
}
[@@deriving yojson]

let get_json_str total_time time_per_commit commits_per_sec =
  let res = { total_time; time_per_commit; commits_per_sec } in
  let obj =
    `Assoc
      [
        ( "results",
          `Assoc
            [
              ("name", `String "benchmarks"); ("metrics", result_to_yojson res);
            ] );
      ]
  in
  Yojson.Safe.to_string obj

let main () ncommits ncycles depth clear no_freeze show_stats json
    merge_throttle freeze_throttle =
  let config =
    {
      ncommits;
      ncycles;
      depth;
      root = "test-bench";
      clear;
      no_freeze;
      show_stats;
      merge_throttle;
      freeze_throttle;
    }
  in
  Format.eprintf "@[<v 2>Running benchmarks in %s:@,@,%a@,@]@." __FILE__
    (Repr.pp_dump config_t) config;
  init config;
  let d, _ = Lwt_main.run (with_timer (fun () -> run config)) in
  let all_commits = ncommits * (ncycles + 5) in
  let rate = d /. float all_commits in
  let freq = 1. /. rate in
  if json then Logs.app (fun l -> l "%s" (get_json_str d rate freq))
  else
    Logs.app (fun l ->
        l
          "%d commits completed in %.2fs.\n\
           [%.3fs per commit, %.0f commits per second]" all_commits d rate freq)

open Cmdliner

let ncommits =
  let doc = Arg.info ~doc:"Number of commits per cycle." [ "n"; "ncommits" ] in
  Arg.(value @@ opt int 4096 doc)

let ncycles =
  let doc =
    Arg.info
      ~doc:
        "Number of cycles. This will be in addition to the 5 cycles that run \
         to emulate freeze."
      [ "b"; "ncycles" ]
  in
  Arg.(value @@ opt int 10 doc)

let depth =
  let doc = Arg.info ~doc:"Depth of a commit's tree." [ "d"; "depth" ] in
  Arg.(value @@ opt int 10 doc)

let clear =
  let doc =
    Arg.info ~doc:"Clear the tree after each commit." [ "c"; "clear" ]
  in
  Arg.(value @@ opt bool false doc)

let no_freeze =
  let doc = Arg.info ~doc:"Without freeze." [ "f"; "no_freeze" ] in
  Arg.(value @@ opt bool false doc)

let stats =
  let doc = Arg.info ~doc:"Show performance stats." [ "s"; "stats" ] in
  Arg.(value @@ flag doc)

let json =
  let doc = Arg.info ~doc:"Json output on command line." [ "j"; "json" ] in
  Arg.(value @@ flag doc)

let merge_throttle =
  [ ("block-writes", `Block_writes); ("overcommit-memory", `Overcommit_memory) ]

let freeze_throttle = ("cancel-existing", `Cancel_existing) :: merge_throttle

let merge_throttle =
  let doc =
    Arg.info ~doc:(Arg.doc_alts_enum merge_throttle) [ "merge-throttle" ]
  in
  Arg.(value @@ opt (Arg.enum merge_throttle) `Block_writes doc)

let freeze_throttle =
  let doc =
    Arg.info ~doc:(Arg.doc_alts_enum freeze_throttle) [ "freeze-throttle" ]
  in
  Arg.(value @@ opt (Arg.enum freeze_throttle) `Block_writes doc)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let main_term =
  Term.(
    const main
    $ setup_log
    $ ncommits
    $ ncycles
    $ depth
    $ clear
    $ no_freeze
    $ stats
    $ json
    $ merge_throttle
    $ freeze_throttle)

let () =
  let info = Term.info "Benchmarks for layered store" in
  Term.exit @@ Term.eval (main_term, info)
