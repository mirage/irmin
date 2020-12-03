open Lwt.Infix

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
        prefix dt Logs_fmt.pp_header (level, h)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k fmt
  in
  { Logs.report }

let reset_stats () =
  Index.Stats.reset_stats ();
  Irmin_pack.Stats.reset_stats ();
  Irmin_layers.Stats.reset_stats ()

type config = {
  ncommits : int;
  ncycles : int;
  depth : int;
  root : string;
  clear : bool;
  no_freeze : bool;
  show_stats : bool;
}
[@@deriving repr]

let () = Random.self_init ()

let random_char () = char_of_int (Random.int 256)

let random_string n = String.init n (fun _i -> random_char ())

let long_random_blob () = random_string 100

let random_blob () = random_string 10

let random_key () = random_string 3

let rm_dir root =
  if Sys.file_exists root then (
    let cmd = Printf.sprintf "rm -rf %s" root in
    Logs.info (fun l -> l "exec: %s\n%!" cmd);
    let _ = Sys.command cmd in
    ())

module Conf = struct
  let entries = 32

  let stable_hash = 256
end

module Hash = Irmin.Hash.SHA1
module Store =
  Irmin_pack.Layered.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

module FSHelper = struct
  let file f =
    try (Unix.stat f).st_size with Unix.Unix_error (Unix.ENOENT, _, _) -> 0

  let dict root = file (Irmin_pack.Layout.dict ~root) / 1024 / 1024

  let pack root = file (Irmin_pack.Layout.pack ~root) / 1024 / 1024

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

let configure_store root =
  let conf =
    Irmin_pack.config ~readonly:false ~fresh:true
      ~index_throttle:`Overcommit_memory root
  in
  Irmin_pack.config_layers ~conf ~with_lower:false ~blocking_copy_size:1000
    ~copy_in_upper:true ()

let init config =
  rm_dir config.root;
  Memtrace.trace_if_requested ();
  reset_stats ()

let info () =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let author = Printf.sprintf "TESTS" in
  Irmin.Info.v ~date ~author "commit "

let key depth =
  let rec go i acc =
    if i >= depth then acc
    else
      let k = random_key () in
      go (i + 1) (k :: acc)
  in
  go 0 []

let large_dir path tree width =
  let rec aux i tree k =
    if i >= width then Lwt.return (k, tree)
    else
      let k = path @ [ random_key (); random_key () ] in
      Store.Tree.add tree k (random_blob ()) >>= fun tree ->
      aux (i + 1) tree (Some k)
  in
  aux 0 tree None >|= fun (path, tree) -> (Option.get path, tree)

let add_large_tree tree =
  let path = key 5 in
  large_dir path tree 256 >>= fun (path, tree) ->
  let path = path @ key 5 in
  Store.Tree.add tree path (long_random_blob ())

let chain_tree tree root depth =
  let rec aux i tree =
    if i >= depth / 2 then Lwt.return tree
    else
      let k = root :: key depth in
      Store.Tree.add tree k (random_blob ()) >>= fun tree -> aux (i + 1) tree
  in
  aux 0 tree

let add_small_tree conf tree =
  let tree = if conf.clear then Store.Tree.empty else tree in
  let k = random_key () in
  chain_tree tree k conf.depth

let init_commit repo =
  Store.Commit.v repo ~info:(info ()) ~parents:[] Store.Tree.empty

let checkout_and_commit config repo c nb =
  Store.Commit.of_hash repo c >>= function
  | None -> Lwt.fail_with "commit not found"
  | Some commit ->
      let tree = Store.Commit.tree commit in
      (if nb mod 1000 = 0 then add_large_tree tree
      else add_small_tree config tree)
      >>= fun tree -> Store.Commit.v repo ~info:(info ()) ~parents:[ c ] tree

let with_timer f =
  let t0 = Sys.time () in
  f () >|= fun a ->
  let t1 = Sys.time () -. t0 in
  (t1, a)

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
      with_timer (fun () ->
          checkout_and_commit config repo (Store.Commit.hash c) i)
      >>= fun (time, c') ->
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
  init_commit repo >>= fun c ->
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
      write_cycle config repo head >>= fun max ->
      print_stats config;
      let min = consume_min () in
      add_min max;
      with_timer (fun () -> freeze ~min_upper:[ min ] ~max:[ max ] config repo)
      >>= fun (time, ()) ->
      if config.show_stats then
        Logs.app (fun l -> l "call to freeze completed in %f" time);
      run_one_cycle max (i + 1)
  in
  run_one_cycle head 0

let rw config =
  let conf = configure_store config.root in
  Store.Repo.v conf

let close config repo =
  with_timer (fun () -> Store.Repo.close repo) >|= fun (t, ()) ->
  if config.show_stats then Logs.app (fun l -> l "close %f" t)

let run config =
  rw config >>= fun repo ->
  first_5_cycles config repo >>= fun c ->
  run_cycles config repo c >>= fun _ ->
  close config repo >|= fun () ->
  if config.show_stats then (
    Fmt.epr "After freeze thread finished : ";
    FSHelper.print_size config.root)

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

let main () ncommits ncycles depth clear no_freeze show_stats json =
  let config =
    {
      ncommits;
      ncycles;
      depth;
      root = "test-bench";
      clear;
      no_freeze;
      show_stats;
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
    $ json)

let () =
  let info = Term.info "Benchmarks for layered store" in
  Term.exit @@ Term.eval (main_term, info)
