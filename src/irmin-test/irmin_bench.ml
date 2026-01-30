(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2019      Etienne Millon
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

open Irmin.Export_for_backends

type t = {
  ncommits : int;
  depth : int;
  tree_add : int;
  display : int;
  clear : bool;
  gc : int;
}

type stats = { commits : int; size : int; maxrss : int }

let src =
  let open Metrics in
  let tags = Tags.[] in
  let data t =
    Data.v
      [
        int "commits" t.commits;
        int "size" ~unit:"MiB" t.size;
        int "maxrss" ~unit:"MiB" t.maxrss;
      ]
  in
  Src.v "bench" ~tags ~data

(* cli *)

open Cmdliner

type mode = Fast | Full

let log style_renderer _level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  (* Suppress all log output during benchmarks *)
  Logs.set_level (Some Logs.Error);
  Logs.set_reporter Logs.nop_reporter;
  ()

let log = Term.(const log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let mode_arg =
  let doc = "Benchmark mode: 'fast' for quick runs, 'full' for thorough runs" in
  let mode_conv =
    Arg.conv
      ( (fun s ->
          match String.lowercase_ascii s with
          | "fast" -> Ok Fast
          | "full" -> Ok Full
          | _ -> Error (`Msg "mode must be 'fast' or 'full'")),
        fun ppf m ->
          Format.pp_print_string ppf
            (match m with Fast -> "fast" | Full -> "full") )
  in
  Arg.(value @@ pos 0 mode_conv Fast @@ info [] ~docv:"MODE" ~doc)

let output_dir_arg =
  let doc = "Output directory for benchmark results" in
  Arg.(value @@ pos 1 string "_metrics" @@ info [] ~docv:"OUTPUT_DIR" ~doc)

let ncommits_opt =
  let doc =
    Arg.info ~doc:"Number of iterations (overrides mode default)."
      [ "n"; "ncommits" ]
  in
  Arg.(value @@ opt (some int) None doc)

let depth_opt =
  let doc =
    Arg.info ~doc:"Depth of the tree (overrides mode default)." [ "d"; "depth" ]
  in
  Arg.(value @@ opt (some int) None doc)

let tree_add_opt =
  let doc =
    Arg.info
      ~doc:"Number of tree entries added per commit (overrides mode default)"
      [ "a"; "tree-add" ]
  in
  Arg.(value @@ opt (some int) None doc)

let display =
  let doc =
    Arg.info ~doc:"Number of commits after which the stats are displayed."
      [ "s"; "stats" ]
  in
  Arg.(value @@ opt int 10 doc)

let gc =
  let doc =
    Arg.info ~doc:"Number of commits after which Gc.full_major is called."
      [ "gc" ]
  in
  Arg.(value @@ opt int 100 doc)

let clear =
  let doc = Arg.info ~doc:"Clear the tree after each commit." [ "clear" ] in
  Arg.(value @@ flag doc)

let defaults_for_mode = function
  | Fast -> (100, 10, 100) (* ncommits, depth, tree_add *)
  | Full -> (1000, 30, 1000)

let t =
  Term.(
    const
      (fun
        ()
        mode
        _output_dir
        ncommits_opt
        depth_opt
        tree_add_opt
        display
        clear
        gc
      ->
        let default_n, default_d, default_a = defaults_for_mode mode in
        let ncommits = Option.value ~default:default_n ncommits_opt in
        let depth = Option.value ~default:default_d depth_opt in
        let tree_add = Option.value ~default:default_a tree_add_opt in
        { ncommits; depth; tree_add; display; clear; gc })
    $ log
    $ mode_arg
    $ output_dir_arg
    $ ncommits_opt
    $ depth_opt
    $ tree_add_opt
    $ display
    $ clear
    $ gc)

let output_dir_term = output_dir_arg

module Make (Store : Irmin.Generic_key.KV with type Schema.Contents.t = string) =
struct
  let info () = Store.Info.v ~author:"author" ~message:"commit message" 0L

  let times ~n ~init f =
    let rec go i k =
      if i = 0 then k init else go (i - 1) (fun r -> k (f i r))
    in
    go n Fun.id

  let path ~depth n =
    let rec aux acc = function
      | i when i = depth -> List.rev (string_of_int n :: acc)
      | i -> aux (string_of_int i :: acc) (i + 1)
    in
    aux [] 0

  let get_maxrss () =
    let usage = Rusage.get SELF in
    let ( / ) = Int64.div in
    Int64.to_int (usage.maxrss / 1024L / 1024L)

  let no_tags x = x

  let print_stats ~commits ~size =
    let maxrss = get_maxrss () in
    let size = size () in
    Metrics.add src no_tags (fun f -> f { size; commits; maxrss })

  (* init: create a tree with [t.depth] levels and each levels has
     [t.tree_add] files + one directory going to the next levele. *)
  let init t config =
    let tree = Store.Tree.empty () in
    let v = Store.Repo.v config |> Store.main in
    let tree =
      times ~n:t.depth ~init:tree (fun depth tree ->
          let paths = Array.init (t.tree_add + 1) (path ~depth) in
          times ~n:t.tree_add ~init:tree (fun n tree ->
              Store.Tree.add tree paths.(n) "init"))
    in
    Store.set_tree_exn v ~info [] tree

  let run t config size =
    let r = Store.Repo.v config in
    let v = Store.main r in
    Store.Tree.reset_counters ();
    let paths = Array.init (t.tree_add + 1) (path ~depth:t.depth) in
    let () =
      times ~n:t.ncommits ~init:() (fun i () ->
          let tree = Store.get_tree v [] in
          if i mod t.gc = 0 then Gc.full_major ();
          if i mod t.display = 0 then print_stats ~size ~commits:i;
          let tree =
            times ~n:t.tree_add ~init:tree (fun n tree ->
                Store.Tree.add tree paths.(n) (string_of_int i))
          in
          Store.set_tree_exn v ~info [] tree;
          if t.clear then Store.Tree.clear tree)
    in
    Store.Repo.close r

  let mkdir_p dir =
    let rec aux dir =
      if Sys.file_exists dir then ()
      else (
        aux (Filename.dirname dir);
        try Unix.mkdir dir 0o755
        with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    in
    aux dir

  let get_root output_dir =
    let path =
      match Sys.getenv_opt "IRMIN_BENCH_ROOT" with
      | Some r -> r
      | None -> output_dir
    in
    mkdir_p path;
    Unix.realpath path

  let write_tree_counters metrics_dir =
    let file = Filename.concat metrics_dir "tree_counters.json" in
    let oc = open_out file in
    let ppf = Format.formatter_of_out_channel oc in
    Store.Tree.dump_counters ppf ();
    Format.pp_print_flush ppf ();
    close_out oc

  (* Redirect stdout/stderr to /dev/null to suppress verbose output.
     This is done once at startup and restored only for our summary message. *)
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0
  let saved_stdout = Unix.dup Unix.stdout
  let saved_stderr = Unix.dup Unix.stderr

  let suppress_output () =
    Unix.dup2 dev_null Unix.stdout;
    Unix.dup2 dev_null Unix.stderr

  let restore_output () =
    Unix.dup2 saved_stdout Unix.stdout;
    Unix.dup2 saved_stderr Unix.stderr

  let main t output_dir config size =
    let root = get_root output_dir in
    (* Set up metrics reporter with predictable output directory *)
    let metrics_dir = Filename.concat root "metrics" in
    mkdir_p metrics_dir;
    (* Only enable our specific metrics source, not all sources *)
    Metrics.Src.enable (Metrics.Src.Src src);
    (* Suppress verbose output (printed at exit by Metrics_gnuplot) *)
    suppress_output ();
    Metrics_gnuplot.set_reporter ~dir:metrics_dir ();
    (* Store goes in "store" subdirectory *)
    let store_root = Filename.concat root "store" in
    mkdir_p store_root;
    let config = config ~root:store_root in
    let size () = size ~root:store_root in
    Eio_main.run @@ fun _ ->
    init t config;
    run t config size;
    write_tree_counters metrics_dir;
    restore_output ();
    Printf.printf "Results: %s\n%!" root;
    suppress_output ()

  let main_term config size =
    Term.(const main $ t $ output_dir_term $ const config $ const size)

  let run ~config ~size =
    let info = Cmd.info "Simple benchmark for trees" in
    Stdlib.exit @@ Cmd.eval @@ Cmd.v info (main_term config size)
end
