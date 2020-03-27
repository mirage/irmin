open Lwt.Infix
open Common

let reporter ?(prefix = "") () =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Unix.gettimeofday () in
      Fmt.kpf k ppf
        ("%s%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        prefix dt
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k fmt
  in
  { Logs.report }

let index_src =
  let open Metrics in
  let open Index.Stats in
  let tags = Tags.[] in
  let data t =
    Data.v
      [
        int "bytes_read" t.bytes_read;
        int "bytes_written" t.bytes_written;
        int "merge" t.nb_merge;
        int "replace" t.nb_replace;
      ]
  in
  Src.v "bench_index" ~tags ~data

let pack_src =
  let open Metrics in
  let open Irmin_pack.Stats in
  let tags = Tags.[] in
  let data t =
    Data.v
      [
        int "find" t.finds;
        int "appended_hashes" t.appended_hashes;
        int "appended_offsets" t.appended_offsets;
      ]
  in
  Src.v "bench_irmin" ~tags ~data

let layers_src =
  let open Metrics in
  let string_of_ints ls = String.concat "," (List.map string_of_int ls) in
  let int_list k v =
    field ~doc:"List of int" k (Other (Fmt.of_to_string string_of_ints)) v
  in
  let open Irmin_layers.Stats in
  let tags = Tags.[] in
  let data t =
    Data.v
      [
        int "freezes" t.nb_freeze;
        int_list "copied_contents" t.copied_contents;
        int_list "copied_nodes" t.copied_nodes;
        int_list "copied_commits" t.copied_commits;
        int_list "copied_branches" t.copied_branches;
      ]
  in
  Src.v "bench_layers" ~tags ~data

let add_metrics () =
  let no_tags x = x in
  let index_stats = Index.Stats.get () in
  let pack_stats = Irmin_pack.Stats.get () in
  let layers_stats = Irmin_layers.Stats.get () in
  Metrics_lwt.add index_src no_tags (fun m -> Lwt.return (m index_stats))
  >>= fun () ->
  Metrics_lwt.add pack_src no_tags (fun m -> Lwt.return (m pack_stats))
  >>= fun () ->
  Metrics_lwt.add layers_src no_tags (fun m -> Lwt.return (m layers_stats))

let reset_stats () =
  Index.Stats.reset_stats ();
  Irmin_pack.Stats.reset_stats ();
  Irmin_layers.Stats.reset_stats ()

open Cmdliner

let ncommits =
  let doc = Arg.info ~doc:"Number of commits per batch." [ "n"; "ncommits" ] in
  Arg.(value @@ opt int 1000 doc)

let nbatches =
  let doc = Arg.info ~doc:"Number of batches." [ "b"; "nbatches" ] in
  Arg.(value @@ opt int 5 doc)

let depth =
  let doc = Arg.info ~doc:"Depth of a commit's tree." [ "d"; "depth" ] in
  Arg.(value @@ opt int 100 doc)

let clear =
  let doc =
    Arg.info ~doc:"Clear the tree after each commit." [ "c"; "clear" ]
  in
  Arg.(value @@ opt bool false doc)

let metrics_flag =
  let doc =
    Arg.info ~doc:"Use Metrics; note that it has an impact on performance"
      [ "m"; "metrics" ]
  in
  Arg.(value @@ opt bool false doc)

let squash =
  let doc = Arg.info ~doc:"Freeze with squash." [ "s"; "squash" ] in
  Arg.(value @@ opt bool false doc)

let keep_max =
  let doc = Arg.info ~doc:"Freeze with keep_max." [ "k"; "keep_max" ] in
  Arg.(value @@ opt bool true doc)

type config = {
  ncommits : int;
  nbatches : int;
  depth : int;
  root : string;
  clear : bool;
  with_metrics : bool;
  squash : bool;
  keep_max : bool;
}

let index_log_size = Some 1_000

let long_random_blob () = random_string 100

let long_random_key () = random_string 100

module Conf = struct
  let entries = 32

  let stable_hash = 256

  let keep_max = true
end

module Hash = Irmin.Hash.SHA1
module Store =
  Irmin_pack.Make_layered (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

module FSHelper = struct
  let file f =
    try (Unix.stat f).st_size with Unix.Unix_error (Unix.ENOENT, _, _) -> 0

  let dict root = file (Filename.concat root "store.dict") / 1024 / 1024

  let pack root = file (Filename.concat root "store.pack") / 1024 / 1024

  let branches root = file (Filename.concat root "store.branches") / 1024 / 1024

  let index root =
    let index_dir = Filename.concat root "index" in
    let a = file (Filename.concat index_dir "data") in
    let b = file (Filename.concat index_dir "log") in
    let c = file (Filename.concat index_dir "log_async") in
    (a + b + c) / 1024 / 1024

  let size root = dict root + pack root + index root

  let print_size root =
    let dt = Unix.gettimeofday () in
    let upper1 = Filename.concat root "upper1" in
    let upper0 = Filename.concat root "upper0" in
    let lower = Filename.concat root "lower" in
    Fmt.epr "%+04.0fus: upper1 = %d M, upper0 = %d M, lower = %d M\n%!" dt
      (size upper1) (size upper0) (size lower)
end

let configure_store ?(readonly = false) ?(fresh = true)
    ?(keep_max = Conf.keep_max) root =
  let conf = Irmin_pack.config ~readonly ?index_log_size ~fresh root in
  Irmin_layers.config ~conf ~keep_max root

let init config =
  rm_dir config.root;
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.App);
  Logs.set_reporter (reporter ());
  reset_stats ();
  if config.with_metrics then (
    Metrics.enable_all ();
    Metrics_gnuplot.set_reporter ();
    Metrics_unix.monitor_gc 0.1 )

let info i () =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let author = Printf.sprintf "TESTS" in
  Irmin.Info.v ~date ~author ("commit " ^ string_of_int i)

let key conf commit =
  let rec go i acc =
    if i = conf.depth then acc
    else
      let k =
        String.concat "_" [ commit; string_of_int i; long_random_key () ]
      in
      go (i + 1) (k :: acc)
  in
  go 0 []

let add_tree conf tree i =
  let k = key conf (string_of_int i) in
  let tree = if conf.clear then Store.Tree.empty else tree in
  Store.Tree.add tree k (long_random_blob ())

let commit config store tree i =
  add_tree config tree i >>= fun tree ->
  Store.set_tree_exn store ~info:(info i) [] tree >|= fun () -> tree

let run_batch config store tree =
  let rec go tree i =
    if i = config.ncommits then Lwt.return tree
    else
      commit config store tree (i * config.ncommits) >>= fun tree ->
      go tree (i + 1)
  in
  go tree 0

let with_timer f =
  let t0 = Sys.time () in
  f () >|= fun tree ->
  let t1 = Sys.time () -. t0 in
  (t1, tree)

let freeze config repo i =
  if i = 0 then (
    Fmt.epr "freeze\n%!";
    Store.freeze ~squash:config.squash repo )
  else Lwt.return_unit

let run_batches config store repo =
  let rec go tree i =
    if i = config.nbatches then Lwt.return tree
    else
      with_timer (fun () -> run_batch config store tree) >>= fun (time, tree) ->
      Fmt.epr "batch = %d, time = %f\n%!" i time;
      (if config.with_metrics then add_metrics () else Lwt.return_unit)
      >>= fun () ->
      freeze config repo i >>= fun () -> go tree (i + 1)
  in
  go Store.Tree.empty 0

let run config =
  init config;
  let conf = configure_store config.root ~keep_max:config.keep_max in
  Store.Repo.v conf >>= fun repo ->
  Store.master repo >>= fun store ->
  run_batches config store repo >>= fun _ ->
  FSHelper.print_size config.root;
  (* closing the repo guarantees that we are waiting for the last freeze to
     finish *)
  Store.Repo.close repo >>= fun () ->
  FSHelper.print_size config.root;
  (if config.with_metrics then add_metrics () else Lwt.return_unit)
  >>= fun () ->
  Store.Repo.v conf >|= fun repo ->
  match Store.integrity_check ~auto_repair:false repo with
  | Ok `No_error -> Fmt.epr "No errors detected by integrity check\n%!"
  | Error (`Corrupted n) -> Fmt.epr "Corrupted entries %d\n%!" n
  | _ -> Fmt.epr "unexpected"

let main ncommits nbatches depth clear with_metrics squash keep_max =
  let config =
    {
      ncommits;
      nbatches;
      depth;
      root = "test-bench";
      clear;
      with_metrics;
      squash;
      keep_max;
    }
  in
  Fmt.epr
    "Benchmarking ./%s with depth = %d, ncommits/batch = %d, nbatches = %d, \
     clear = %b, metrics = %b, squash = %b, keep_max = %b \n\
     %!"
    config.root config.depth config.ncommits config.nbatches config.clear
    config.with_metrics config.squash config.keep_max;
  Lwt_main.run (run config)

let main_term =
  Term.(
    const main
    $ ncommits
    $ nbatches
    $ depth
    $ clear
    $ metrics_flag
    $ squash
    $ keep_max)

let () =
  let info = Term.info "Simple benchmarks for layered store" in
  Term.exit @@ Term.eval (main_term, info)
