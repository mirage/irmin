(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

type t = {
  root : string;
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

(* logs *)

let ignore_srcs src =
  List.mem (Logs.Src.name src)
    [
      "git.inflater.decoder";
      "git.deflater.encoder";
      "git.encoder";
      "git.decoder";
      "git.loose";
      "git.store";
      "cohttp.lwt.io";
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

let gc =
  let doc =
    Arg.info ~doc:"Number of commits after which Gc.full_major is called."
      [ "gc" ]
  in
  Arg.(value @@ opt int 100 doc)

let clear =
  let doc = Arg.info ~doc:"Clear the tree after each commit." [ "clear" ] in
  Arg.(value @@ flag doc)

let t =
  Term.(
    const (fun () ncommits depth tree_add display clear gc ->
        { ncommits; depth; tree_add; display; root = "."; clear; gc })
    $ log
    $ ncommits
    $ depth
    $ tree_add
    $ display
    $ clear
    $ gc)

module Make (Store : Irmin.KV with type contents = string) = struct
  let info () = Irmin.Info.v ~date:0L ~author:"author" "commit message"

  let times ~n ~init f =
    let rec go i k =
      if i = 0 then k init else go (i - 1) (fun r -> f i r >>= k)
    in
    go n Lwt.return

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

  let plot_progress n t = Fmt.epr "\rcommits: %4d/%d%!" n t

  (* init: create a tree with [t.depth] levels and each levels has
     [t.tree_add] files + one directory going to the next levele. *)
  let init t config =
    let tree = Store.Tree.empty in
    Store.Repo.v config >>= Store.master >>= fun v ->
    times ~n:t.depth ~init:tree (fun depth tree ->
        let paths = Array.init (t.tree_add + 1) (path ~depth) in
        times ~n:t.tree_add ~init:tree (fun n tree ->
            Store.Tree.add tree paths.(n) "init"))
    >>= fun tree ->
    Store.set_tree_exn v ~info [] tree >|= fun () -> Fmt.epr "[init done]\n%!"

  let run t config size =
    Store.Repo.v config >>= Store.master >>= fun v ->
    Store.Tree.reset_counters ();
    let paths = Array.init (t.tree_add + 1) (path ~depth:t.depth) in
    times ~n:t.ncommits ~init:() (fun i () ->
        Store.get_tree v [] >>= fun tree ->
        if i mod t.gc = 0 then Gc.full_major ();
        if i mod t.display = 0 then (
          plot_progress i t.ncommits;
          print_stats ~size ~commits:i );
        times ~n:t.tree_add ~init:tree (fun n tree ->
            Store.Tree.add tree paths.(n) (string_of_int i))
        >>= fun tree ->
        Store.set_tree_exn v ~info [] tree >|= fun () ->
        if t.clear then Store.Tree.clear tree)
    >|= fun _ -> Fmt.epr "\n[run done]\n%!"

  let main t config size =
    let root = "_build/_bench" in
    let config = config ~root in
    let size () = size ~root in
    let t = { t with root } in
    Lwt_main.run (init t config >>= fun () -> run t config size)

  let main_term config size = Term.(const main $ t $ pure config $ pure size)

  let () =
    at_exit (fun () ->
        Fmt.epr "tree counters:\n%a\n%!" Store.Tree.dump_counters ())

  let run ~config ~size =
    let info = Term.info "Simple benchmark for trees" in
    Term.exit @@ Term.eval (main_term config size, info)
end

let () =
  Metrics.enable_all ();
  Metrics_gnuplot.set_reporter ()
