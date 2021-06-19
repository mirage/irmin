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
open Bench_common

let () = Random.self_init ()

type config = {
  ncommits : int;
  ncycles : int;
  depth : int;
  root : string;
  clear : bool;
  no_freeze : bool;
  show_stats : bool;
  merge_throttle : Irmin_pack.Conf.merge_throttle;
  freeze_throttle : Irmin_pack.Conf.freeze_throttle;
}
[@@deriving repr]

module Contents = struct
  type t = bytes

  let ty = Irmin.Type.(pair (bytes_of `Int64) unit)
  let pre_hash_ty = Irmin.Type.(unstage (pre_hash ty))
  let pre_hash_v1 x = pre_hash_ty (x, ())
  let t = Irmin.Type.(like bytes ~pre_hash:(stage @@ fun x -> pre_hash_v1 x))
  let merge = Irmin.Merge.(idempotent (Irmin.Type.option t))
end

module Schema = struct
  module Metadata = Irmin.Metadata.None
  module Contents = Contents
  module Path = Irmin.Path.String_list
  module Branch = Irmin.Branch.String
  module Hash = Irmin.Hash.SHA1
  module Node = Irmin.Node.Make (Hash) (Path) (Metadata)
  module Commit = Irmin.Commit.Make (Hash)
  module Info = Irmin.Info.Default
end

module Store = struct
  open Irmin_pack_layered.Maker (Conf)
  include Make (Schema)
end

module Info = Info (Store.Info)

let configure_store root merge_throttle freeze_throttle =
  let conf =
    Irmin_pack.config ~readonly:false ~fresh:true ~freeze_throttle
      ~merge_throttle root
  in
  Irmin_pack_layered.config ~with_lower:false ~blocking_copy_size:1000 conf

let init config =
  FSHelper.rm_dir config.root;
  Memtrace.trace_if_requested ();
  reset_stats ()

module Trees = Generate_trees (Store)

let init_commit repo =
  Store.Commit.v repo ~info:(Info.f ()) ~parents:[] Store.Tree.empty

let checkout_and_commit config repo c nb =
  Store.Commit.of_hash repo c >>= function
  | None -> Lwt.fail_with "commit not found"
  | Some commit ->
      let tree = Store.Commit.tree commit in
      let* tree =
        if nb mod 1000 = 0 then Trees.add_large_trees 256 2 tree
        else Trees.add_chain_trees config.depth (config.depth / 2) tree
      in
      Store.Commit.v repo ~info:(Info.f ()) ~parents:[ c ] tree

let total = ref 0

let print_commit_stats config c i time =
  let num_objects = Irmin_layers.Stats.get_add_count () in
  total := !total + num_objects;
  if config.show_stats then
    Logs.app (fun l ->
        l "Commit %a %d in cycle completed in %f; objects created: %d"
          Store.Commit.pp_hash c i time num_objects)

let get_maxrss () =
  let usage = Rusage.(get Self) in
  let ( / ) = Int64.div in
  Int64.to_int (usage.maxrss / 1024L / 1024L)

let print_stats () = Logs.app (fun l -> l "%t" Irmin_layers.Stats.pp_latest)

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
  else Store.freeze ~max_lower:max ~min_upper repo

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

let run_cycles config repo head json =
  let rec run_one_cycle head i =
    if i = config.ncycles then Lwt.return head
    else
      let* max = write_cycle config repo head in
      if not json then print_stats ();
      let min = consume_min () in
      add_min max;
      let* time, () =
        with_timer (fun () ->
            freeze ~min_upper:[ min ] ~max:[ max ] config repo)
      in
      if config.show_stats then
        Logs.app (fun l ->
            l "call to freeze completed in %f, maxrss = %d" time (get_maxrss ()));
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
  if config.show_stats then
    Logs.app (fun l -> l "close %f, maxrss = %d" t (get_maxrss ()))

let run config json =
  let* repo = rw config in
  let* c = first_5_cycles config repo in
  let* _ = run_cycles config repo c json in
  close config repo >|= fun () ->
  if config.show_stats then (
    Logs.app (fun l -> l "After freeze thread finished : ");
    FSHelper.print_size_layers config.root)

module Continuous_benchmarks_results = struct
  type metrics = (string * float) list [@@deriving repr]

  let metrics_t = Irmin.Type.(Json.assoc float)

  type result = { name : string; metrics : metrics } [@@deriving repr]
  type t = { results : (result[@nobuiltin]) list } [@@deriving repr]

  let get_json_str total_time time_per_commit commits_per_sec =
    {
      results =
        [
          {
            name = "Layers";
            metrics =
              [
                ("total_time", total_time);
                ("time_per_commit", time_per_commit);
                ("commits_per_sec", commits_per_sec);
              ];
          };
        ];
    }
    |> Fmt.str "%a\n" (Irmin.Type.pp_json t)
end

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
  if not json then
    Logs.app (fun l ->
        l "@[<v 2>Running benchmarks in %s:@,@,%a@,@]@." __FILE__
          (Repr.pp_dump config_t) config);
  init config;
  let d, _ = Lwt_main.run (with_timer (fun () -> run config json)) in
  let all_commits = ncommits * (ncycles + 5) in
  let rate = d /. float all_commits in
  let freq = 1. /. rate in
  if json then
    Printf.printf "%s" (Continuous_benchmarks_results.get_json_str d rate freq)
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
