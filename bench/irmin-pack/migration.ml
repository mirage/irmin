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

type config = { store_dir : string }

let config ~root = Irmin_pack.config ~fresh:false root

module Store = struct
  module Maker = Irmin_pack.KV (Irmin_pack.Version.V1) (Conf)
  include Maker.Make (Irmin.Contents.String)
end

module Info = Info (Store.Info)

module Generate_store = struct
  let init root =
    Store.Repo.v (Irmin_pack.config ~readonly:false ~fresh:true root)

  let close repo = Store.Repo.close repo

  type path = { length : int; repetition : int }

  let generate_tree tree (a, b, c, d) nb_entries =
    let path pattern =
      let rec aux_path acc i =
        if i >= pattern.repetition then acc
        else aux_path (random_string pattern.length :: acc) (i + 1)
      in
      aux_path [] 0
    in
    let rec aux tree i =
      if i >= nb_entries then Lwt.return tree
      else
        let key = a @ path b @ c @ path d in
        let blob = random_string 2 in
        let* tree = Store.Tree.add tree key blob in
        aux tree (i + 1)
    in
    aux tree 0

  let commit_init repo =
    Store.Commit.v repo ~info:(Info.f ()) ~parents:[] Store.Tree.empty

  let commit_tree repo parent pattern nb_entries =
    let rec aux parent i =
      if i * 100 >= nb_entries then Lwt.return parent
      else
        let tree = Store.Commit.tree parent in
        let hash = Store.Commit.hash parent in
        let* tree = generate_tree tree pattern 100 in
        let* commit =
          Store.Commit.v repo ~info:(Info.f ()) ~parents:[ hash ] tree
        in
        aux commit (i + 1)
    in
    aux parent 0

  let commit repo =
    let* init_commit = commit_init repo in
    Logs.info (fun l ->
        l "committing /contracts/index/xx/xx/xx/xx/xx/xx/yyyyyyyyyy");
    report_mem_stats ();
    let pattern =
      ( [ "data"; "contracts"; "index" ],
        { length = 2; repetition = 6 },
        [],
        { length = 10; repetition = 1 } )
    in
    let nb_entries = 100_000 in
    let* commit = commit_tree repo init_commit pattern nb_entries in
    Logs.info (fun l ->
        l "committing /contracts/index/yyyyyyyyyy/delegated/xx/xx/xx/xx/xx/xx");
    report_mem_stats ();
    let pattern =
      ( [ "data"; "contracts"; "index" ],
        { length = 10; repetition = 1 },
        [ "delegated" ],
        { length = 2; repetition = 6 } )
    in
    let nb_entries = 1_000 in
    (* let nb_entries = 104 in *)
    let* commit = commit_tree repo commit pattern nb_entries in
    Logs.info (fun l -> l "committing /rolls/owner/snapshot/n1/n2/x/y/n3");
    report_mem_stats ();
    let pattern =
      ( [ "data"; "rolls"; "owner"; "snapshot" ],
        { length = 8; repetition = 2 },
        [],
        { length = 2; repetition = 3 } )
    in
    let nb_entries = 3_000_000 in
    (* let nb_entries = 3 in *)
    let* commit = commit_tree repo commit pattern nb_entries in
    report_mem_stats ();
    Lwt.return commit

  let v root =
    let* repo = init root in
    let* head = commit repo in
    let* () = Store.Branch.set repo "migrate" head in
    close repo
end

include Flatten.Make (Store)

let run_migration config =
  let run () = run_migration config.store_dir in
  let+ result, () = Benchmark.run config.store_dir run in
  Format.printf "%a\n@." Benchmark.pp_results result

let main () store_dir =
  Printexc.record_backtrace true;
  let tmp_store_dir = Filename.concat "_build" "migration" in
  let run =
    let* () =
      if Sys.file_exists store_dir then (
        FSHelper.cp_dir store_dir tmp_store_dir;
        Lwt.return_unit)
      else
        let* () = Generate_store.v store_dir in
        FSHelper.cp_dir store_dir tmp_store_dir;
        Lwt.return_unit
    in
    let config = { store_dir = tmp_store_dir } in
    run_migration config
  in
  Lwt_main.run run

open Cmdliner

let default_store_dir = Filename.concat "test-bench" "migration"

let store_dir =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Destination of the store before flattening."
      [ "store" ]
  in
  Arg.(value @@ opt string default_store_dir doc)

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let main_term = Term.(const main $ setup_log $ store_dir)

let () =
  let info = Term.info "Benchmarks for migration to flattened store" in
  Term.exit @@ Term.eval (main_term, info)
