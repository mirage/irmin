open Lwt.Infix
module Store = Irmin_pack.KV (Irmin.Contents.String)

type t = {
  root : Fpath.t;
  ncommits : int;
  depth : int;
  tree_add : int;
  display : int;
  clear : bool
}

let info () = Irmin.Info.v ~date:0L ~author:"author" "commit message"

let times ~n ~init f =
  let rec go i k =
    if i = 0 then k init else go (i - 1) (fun r -> f i r >>= k)
  in
  go n Lwt.return

let path ~depth n = List.init depth string_of_int @ [ string_of_int n ]

let print_headers () =
  Fmt.epr
    "# time, level, dict, index, pack, mem, bf_misses, pack_page_faults, \
     index_page_faults, pack_cache_misses, search_steps\n\
     %!"

let get_maxrss () =
  let usage = Rusage.get SELF in
  let ( / ) = Int64.div in
  Int64.to_int (usage.maxrss / 1024L / 1024L)

let file f =
  (* in MiB *)
  try (Unix.stat f).st_size with Unix.Unix_error (Unix.ENOENT, _, _) -> 0

let t0 = Unix.gettimeofday ()

let print_stats ~level t =
  let mem = get_maxrss () in
  let root = Fpath.to_string t.root in
  let dict = file (Filename.concat root "store.dict") / 1024 / 1024 in
  let index =
    let rec aux acc i =
      if i = 256 then acc
      else
        let filename = Format.sprintf "store.index.%d" i in
        let s = file (Filename.concat root filename) in
        aux (acc + s) (i + 1)
    in
    aux 0 0 / 1024 / 1024
  in
  let pack = file (Filename.concat root "store.pack") / 1024 / 1024 in
  let time =
    (* in seconds *)
    int_of_float (Unix.gettimeofday () -. t0)
  in
  let stats = Irmin_pack.stats () in
  Fmt.epr "%d, %d, %d, %d, %d, %d, %f, %f, %f, %f, %f\n%!" time level dict
    index pack mem stats.bf_misses stats.pack_page_faults
    stats.index_page_faults stats.pack_cache_misses stats.search_steps

let run t =
  let config = Irmin_pack.config ~fresh:false (Fpath.to_string t.root) in
  let tree = Store.Tree.empty in
  Store.Repo.v config >>= Store.master >>= fun v ->
  print_headers ();
  times ~n:t.ncommits ~init:tree (fun i tree ->
      if i mod t.display = 0 then print_stats ~level:i t;
      times ~n:t.tree_add ~init:tree (fun n tree ->
          Store.Tree.add tree (path ~depth:t.depth n) (string_of_int i) )
      >>= fun tree ->
      Store.set_tree_exn v ~info [] tree >>= fun () ->
      if t.clear then Store.Tree.clear tree;
      Lwt.return tree )
  >>= fun _ -> Lwt_io.printl "ok"

let main t =
  Bos.OS.Dir.with_tmp "irmin%s"
    (fun root () -> Lwt_main.run (run { t with root }))
    ()
  |> Rresult.R.failwith_error_msg

(* logs *)

let ignore_srcs src =
  List.mem (Logs.Src.name src)
    [ "git.inflater.decoder";
      "git.deflater.encoder";
      "git.encoder";
      "git.decoder";
      "git.loose";
      "git.store";
      "cohttp.lwt.io"
    ]

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
    msgf @@ fun ?header ?tags fmt ->
    if ignore_srcs src then Format.ikfprintf k ppf fmt
    else with_stamp header tags k fmt
  in
  { Logs.report }

(* cli *)

open Cmdliner

let log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter ());
  ()

let log = Term.(const log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let ncommits =
  let doc = Arg.info ~doc:"Number of iterations." [ "n"; "ncommits" ] in
  Arg.(value @@ opt int 1000 doc)

let depth =
  let doc = Arg.info ~doc:"Depth of the tree." [ "d"; "depth" ] in
  Arg.(value @@ opt int 30 doc)

let tree_add =
  let doc =
    Arg.info ~doc:"Number of tree entries added per commit" [ "a"; "tree-add" ]
  in
  Arg.(value @@ opt int 1000 doc)

let display =
  let doc =
    Arg.info ~doc:"Number of commits after which the stats are displayed."
      [ "s"; "stats" ]
  in
  Arg.(value @@ opt int 10 doc)

let clear =
  let doc = Arg.info ~doc:"Clear the tree after each commit." [ "clear" ] in
  Arg.(value @@ flag doc)

let t =
  Term.(
    const (fun () ncommits depth tree_add display clear ->
        { ncommits; depth; tree_add; display; root = Fpath.v "."; clear } )
    $ log $ ncommits $ depth $ tree_add $ display $ clear)

let main = Term.(const main $ t)

let () =
  at_exit (fun () ->
      Fmt.epr "tree counters:\n%a\n%!" Store.Tree.dump_counters () )

let () =
  let info = Term.info "Simple benchmark for trees" in
  Term.exit @@ Term.eval (main, info)
