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

open Cmdliner

let ncommits =
  let doc = Arg.info ~doc:"Number of commits per batch." [ "n"; "ncommits" ] in
  Arg.(value @@ opt int 50 doc)

let nbatches =
  let doc = Arg.info ~doc:"Number of batches." [ "b"; "nbatches" ] in
  Arg.(value @@ opt int 5 doc)

let depth =
  let doc = Arg.info ~doc:"Depth of a commit's tree." [ "d"; "depth" ] in
  Arg.(value @@ opt int 1000 doc)

let clear =
  let doc =
    Arg.info ~doc:"Clear the tree after each commit." [ "c"; "clear" ]
  in
  Arg.(value @@ opt bool false doc)

let copy_in_upper =
  let doc =
    Arg.info ~doc:"Freeze with copy_in_upper." [ "k"; "copy_in_upper" ]
  in
  Arg.(value @@ opt bool true doc)

let reader =
  let doc = Arg.info ~doc:"Benchmark RO reads." [ "r"; "reader" ] in
  Arg.(value @@ opt bool false doc)

let no_freeze =
  let doc = Arg.info ~doc:"Without freeze." [ "f"; "no_freeze" ] in
  Arg.(value @@ opt bool false doc)

type config = {
  ncommits : int;
  nbatches : int;
  depth : int;
  root : string;
  clear : bool;
  copy_in_upper : bool;
  reader : bool;
  no_freeze : bool;
}

let index_log_size = Some 1_000

let long_random_blob () = random_string 100

let long_random_key () = random_string 100

let keys : string list list ref = ref []

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

let configure_store ?(readonly = false) ?(fresh = true)
    ?(copy_in_upper = Conf.copy_in_upper) root =
  let conf = Irmin_pack.config ~readonly ?index_log_size ~fresh root in
  Irmin_pack.config_layers ~conf ~copy_in_upper ()

let init config =
  rm_dir config.root;
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.App);
  Logs.set_reporter (reporter ());
  reset_stats ()

let info () =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let author = Printf.sprintf "TESTS" in
  Irmin.Info.v ~date ~author "commit "

let key conf =
  let rec go i acc =
    if i = conf.depth then acc
    else
      let k = long_random_key () in
      go (i + 1) (k :: acc)
  in
  go 0 []

let generate_keys conf =
  let rec go i =
    if i = conf.ncommits then ()
    else
      let k = key conf in
      keys := k :: !keys;
      go (i + 1)
  in
  go 0

let add_tree ?i conf tree =
  let k = match i with None -> key conf | Some i -> List.nth !keys i in
  let tree = if conf.clear then Store.Tree.empty else tree in
  Store.Tree.add tree k (long_random_blob ())

let init_commit repo =
  Store.Commit.v repo ~info:(info ()) ~parents:[] Store.Tree.empty

let checkout_and_commit ?i config repo c =
  Store.Commit.of_hash repo c >>= function
  | None -> Lwt.fail_with "commit not found"
  | Some commit ->
      let tree = Store.Commit.tree commit in
      add_tree ?i config tree >>= fun tree ->
      Store.Commit.v repo ~info:(info ()) ~parents:[ c ] tree

let with_timer f =
  let t0 = Sys.time () in
  f () >|= fun a ->
  let t1 = Sys.time () -. t0 in
  (t1, a)

let print_stats () =
  let s = Irmin_pack.Stats.get () in
  let i = Index.Stats.get () in
  let t = Irmin_layers.Stats.get () in
  let copied_objects =
    List.map2 (fun x y -> x + y) t.copied_contents t.copied_commits
    |> List.map2 (fun x y -> x + y) t.copied_nodes
    |> List.map2 (fun x y -> x + y) t.copied_branches
  in
  let pp_comma ppf () = Fmt.pf ppf "," in
  Logs.app (fun l ->
      l
        "Irmin stats : finds = %d cache_misses= %d appended_hashes= %d \
         appended_offsets= %d \n\
         Index stats:         nb_reads = %d nb_writes = %d \n\
         Irmin-layers stats : nb_freeze = %d copied_objects = %a \n\
         Lwt engine stats: readable = %d, writable = %d, timer = %d" s.finds
        s.cache_misses s.appended_hashes s.appended_offsets i.nb_reads
        i.nb_writes t.nb_freeze
        Fmt.(list ~sep:pp_comma int)
        copied_objects
        (Lwt_engine.readable_count ())
        (Lwt_engine.writable_count ())
        (Lwt_engine.timer_count ()))

let write_batch ?(generated_keys = false) config repo init_commit =
  let rec go (times, c) i =
    if i = config.ncommits then Lwt.return (List.rev times, c)
    else
      let hash_c = Store.Commit.hash c in
      let commit =
        if generated_keys then checkout_and_commit ~i config
        else checkout_and_commit config
      in
      with_timer (fun () -> commit repo hash_c) >>= fun (time, c') ->
      go (time :: times, c') (i + 1)
  in
  go ([], init_commit) 0

let write_keys config repo c = write_batch ~generated_keys:true config repo c

let freeze ~min_upper ~max config repo =
  if config.no_freeze then Lwt.return_unit
  else Store.freeze ~max ~min_upper repo

let read_batch store =
  let find key =
    Store.find store key >|= function
    | None -> failwith "RO does not find key"
    | Some _ -> ()
  in
  Lwt_list.fold_left_s
    (fun acc key ->
      with_timer (fun () -> find key) >|= fun (time, ()) -> time :: acc)
    [] !keys
  >|= List.rev

let pp_float ppf x = Fmt.pf ppf "%f\n" x

let run_batches config repo init_commit =
  let rec go min i =
    if i = config.nbatches then Lwt.return min
    else
      write_batch config repo min >>= fun (times, max) ->
      if not config.reader then (
        Fmt.epr "write batch :\n%a\n%!" (Fmt.list pp_float) times;
        print_stats ());
      with_timer (fun () -> freeze ~min_upper:[ min ] ~max:[ max ] config repo)
      >>= fun (t, ()) ->
      Fmt.epr "Freeze %f\n%!" t;
      go max (i + 1)
  in
  go init_commit 0

let rw config =
  let conf = configure_store config.root ~copy_in_upper:config.copy_in_upper in
  Store.Repo.v conf >>= fun repo ->
  Store.master repo >|= fun store -> (repo, store)

let worker_thread config r_pipe =
  let ro_conf = configure_store ~readonly:true ~fresh:false config.root in
  ignore (Concurrent.read r_pipe);
  let rec read i =
    if i = 20 then Lwt.return_unit
    else
      let sec = 0.2 *. float_of_int i in
      Lwt_unix.sleep sec >>= fun () ->
      Store.Repo.v ro_conf >>= fun repo ->
      Store.master repo >>= fun store ->
      read_batch store >>= fun times ->
      Store.Repo.close repo >>= fun () ->
      Fmt.epr "read batch :\n%a\n%!" (Fmt.list pp_float) times;
      read (i + 1)
  in
  read 0

let close repo =
  with_timer (fun () -> Store.Repo.close repo) >|= fun (t, ()) ->
  Fmt.epr "close %f\n%!" t

let concurrent_reads config =
  generate_keys config;
  let read_worker, write_main = Unix.pipe () in
  match Lwt_unix.fork () with
  | 0 ->
      Printexc.record_backtrace true;
      Fmt_tty.setup_std_outputs ();
      Logs.set_level (Some Logs.App);
      Logs.set_reporter (reporter ());
      Logs.debug (fun l -> l "Worker %d created" (Unix.getpid ()));
      worker_thread config read_worker >|= fun () -> exit 0
  | pid ->
      init config;
      Logs.debug (fun l -> l "Main %d started" (Unix.getpid ()));
      rw config >>= fun (repo, _store) ->
      init_commit repo >>= fun c ->
      Logs.debug (fun l -> l "Write keys to be read by the readonly process");
      write_keys config repo c >>= fun (_times, c) ->
      Store.Branch.set repo "master" c >>= fun () ->
      Store.flush repo;
      with_timer (fun () -> freeze ~min_upper:[] ~max:[ c ] config repo)
      >>= fun (t, ()) ->
      Fmt.epr "freeze %f\n%!" t;
      Concurrent.write write_main;
      run_batches config repo c >>= fun _ ->
      Concurrent.wait pid >>= fun () ->
      Unix.close read_worker;
      Unix.close write_main;
      close repo

let run config =
  (if config.reader then concurrent_reads config
  else (
    init config;
    rw config >>= fun (repo, _store) ->
    init_commit repo >>= fun c ->
    run_batches config repo c >>= fun _ -> close repo))
  >|= fun () ->
  Fmt.epr "After freeze thread finished : ";
  FSHelper.print_size config.root

let main ncommits nbatches depth clear copy_in_upper reader no_freeze =
  let config =
    {
      ncommits;
      nbatches;
      depth;
      root = "test-bench";
      clear;
      copy_in_upper;
      reader;
      no_freeze;
    }
  in
  Fmt.epr
    "Benchmarking ./%s with depth = %d, ncommits/batch = %d, nbatches = %d, \
     clear = %b, copy_in_upper  = %b reader = %b no_freeze = %b \n\
     %!"
    config.root config.depth config.ncommits config.nbatches config.clear
    config.copy_in_upper config.reader config.no_freeze;
  Lwt_main.run (run config)

let main_term =
  Term.(
    const main
    $ ncommits
    $ nbatches
    $ depth
    $ clear
    $ copy_in_upper
    $ reader
    $ no_freeze)

let () =
  let info = Term.info "Benchmarks for layered store" in
  Term.exit @@ Term.eval (main_term, info)
