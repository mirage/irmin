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
}

let long_random_blob () = random_string 100

let random_blob () = random_string 10

let random_key () = random_string 3

module Conf = struct
  let entries = 32

  let stable_hash = 256

  let copy_in_upper = true
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

let configure_store root =
  let conf =
    Irmin_pack.config ~readonly:false ~fresh:true
      ~index_throttle:`Overcommit_memory root
  in
  Irmin_pack.config_layers ~conf ~with_lower:false ~blocking_copy_size:1000
    ~copy_in_upper:true ()

let verbose = ref false

let init config =
  rm_dir config.root;
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.App);
  if !verbose then Logs.set_reporter (reporter ());
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
  (* large_dir path tree 256 >>= fun (path, tree) -> *)
  (* let path = path @ key 5 in *)
  large_dir path tree 256 >>= fun (path, tree) ->
  let path = path @ key 5 in
  Store.Tree.add tree path (long_random_blob ())

(* large_dir path tree 256 >|= fun (_, tree) -> tree *)

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

(* let rec aux i tree path =
        if i >= conf.depth then Lwt.return tree
        else large_dir path tree 9 >>= fun (path, tree) -> aux (i + 1) tree path
      in
   aux 0 tree [ random_key () ] *)

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

let pp_commit_stats c i time =
  let num_objects = Irmin_layers.Stats.get_adds () in
  total := !total + num_objects;
  Irmin_layers.Stats.reset_adds ();
  if !verbose then
    Logs.app (fun l ->
        l "Commit %a %d in cycle completed in %f; objects created = %d"
          Store.Commit.pp_hash c i time num_objects)
  else Fmt.epr "%f\n%!" time

let print_stats () =
  let t = Irmin_layers.Stats.get () in
  let copied_objects =
    List.map2 (fun x y -> x + y) t.copied_contents t.copied_commits
    |> List.map2 (fun x y -> x + y) t.copied_nodes
    |> List.map2 (fun x y -> x + y) t.copied_branches
  in
  let pp_comma ppf () = Fmt.pf ppf "," in
  if !verbose then
    Logs.app (fun l ->
        l
          "Irmin-layers stats: nb_freeze = %d copied_objects = %a \
           waiting_freeze = %a completed_freeze = %a, objects added in upper \
           since last freeze = %d"
          t.nb_freeze
          Fmt.(list ~sep:pp_comma int)
          copied_objects
          Fmt.(list ~sep:pp_comma float)
          t.waiting_freeze
          Fmt.(list ~sep:pp_comma float)
          t.completed_freeze !total);
  total := 0

let write_cycle config repo init_commit =
  let rec go c i =
    if i = config.ncommits then Lwt.return c
    else
      with_timer (fun () ->
          checkout_and_commit config repo (Store.Commit.hash c) i)
      >>= fun (time, c') ->
      pp_commit_stats c' i time;
      go c' (i + 1)
  in
  go init_commit 0

let freeze ~min_upper ~max config repo =
  if config.no_freeze then Lwt.return_unit
  else Store.freeze ~max ~min_upper repo

let pp_float ppf x = Fmt.pf ppf "%f\n" x

let min_uppers = Queue.create ()

let add_min c = Queue.add c min_uppers

let consume_min () = Queue.pop min_uppers

let first_5_cycles config repo =
  init_commit repo >>= fun c ->
  pp_commit_stats c 0 0.0;
  let rec aux i c =
    add_min c;
    if i >= 4 then Lwt.return c
    else write_cycle config repo c >>= fun c -> aux (i + 1) c
  in
  aux 0 c

let run_cycles config repo head =
  let rec run_one_cycle head i =
    if i = config.ncycles then Lwt.return head
    else
      write_cycle config repo head >>= fun max ->
      print_stats ();
      let min = consume_min () in
      add_min max;
      with_timer (fun () -> freeze ~min_upper:[ min ] ~max:[ max ] config repo)
      >>= fun (time, ()) ->
      if !verbose then
        Logs.app (fun l -> l "call to freeze completed in %f" time)
      else Fmt.epr "%f\n%!" time;
      run_one_cycle max (i + 1)
  in
  run_one_cycle head 0

let rw config =
  let conf = configure_store config.root in
  Store.Repo.v conf

let close repo =
  with_timer (fun () -> Store.Repo.close repo) >|= fun (t, ()) ->
  if !verbose then Logs.app (fun l -> l "close %f" t)

let run config =
  init config;
  rw config >>= fun repo ->
  first_5_cycles config repo >>= fun c ->
  Memtrace.trace_if_requested ();
  run_cycles config repo c >>= fun _ ->
  close repo >|= fun () ->
  if !verbose then (
    Fmt.epr "After freeze thread finished : ";
    FSHelper.print_size config.root)

let main ncommits ncycles depth clear no_freeze vv =
  verbose := vv;
  let config =
    { ncommits; ncycles; depth; root = "test-bench"; clear; no_freeze }
  in
  Logs.app (fun l ->
      l
        "Benchmarking ./%s with depth = %d, ncommits/cycle = %d, ncycles = %d, \
         clear = %b, no_freeze = %b. Each commit adds small trees (9 * depth \
         objects); every 100th commit adds\n\
        \     a large tree (256 *3 + 8 objects). First 5 cycles run without \
         freeze, after that each cycle calls freeze once and copies the last 5 \
         cycles."
        config.root config.depth config.ncommits config.ncycles config.clear
        config.no_freeze);
  Lwt_main.run (run config)

open Cmdliner

let ncommits =
  let doc = Arg.info ~doc:"Number of commits per cycle." [ "n"; "ncommits" ] in
  Arg.(value @@ opt int 4096 doc)

let ncycles =
  let doc = Arg.info ~doc:"Number of cycles." [ "b"; "ncycles" ] in
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

let verbose =
  let doc = Arg.info ~doc:"Report more stats." [ "v"; "verbose" ] in
  Arg.(value @@ flag doc)

let main_term =
  Term.(const main $ ncommits $ ncycles $ depth $ clear $ no_freeze $ verbose)

let () =
  let info = Term.info "Benchmarks for layered store" in
  Term.exit @@ Term.eval (main_term, info)
