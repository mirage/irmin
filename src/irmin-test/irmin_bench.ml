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

(* cli *)

open Cmdliner

let deprecated_info = (Term.info [@alert "-deprecated"])
let deprecated_exit = (Term.exit [@alert "-deprecated"])
let deprecated_eval = (Term.eval [@alert "-deprecated"])

let log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Irmin_test.reporter ());
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

  let plot_progress n t = Fmt.epr "\rcommits: %4d/%d%!" n t

  (* init: create a tree with [t.depth] levels and each levels has
     [t.tree_add] files + one directory going to the next levele. *)
  let init t config =
    Eio.Switch.run @@ fun sw ->
    let tree = Store.Tree.empty () in
    let v = Store.Repo.v ~sw config |> Store.main in
    let tree =
      times ~n:t.depth ~init:tree (fun depth tree ->
          let paths = Array.init (t.tree_add + 1) (path ~depth) in
          times ~n:t.tree_add ~init:tree (fun n tree ->
              Store.Tree.add tree paths.(n) "init"))
    in
    Store.set_tree_exn v ~info [] tree;
    Fmt.epr "[init done]\n%!"

  let run t config size =
    Eio.Switch.run @@ fun sw ->
    let r = Store.Repo.v ~sw config in
    let v = Store.main r in
    Store.Tree.reset_counters ();
    let paths = Array.init (t.tree_add + 1) (path ~depth:t.depth) in
    let () =
      times ~n:t.ncommits ~init:() (fun i () ->
          let tree = Store.get_tree v [] in
          if i mod t.gc = 0 then Gc.full_major ();
          if i mod t.display = 0 then (
            plot_progress i t.ncommits;
            print_stats ~size ~commits:i);
          let tree =
            times ~n:t.tree_add ~init:tree (fun n tree ->
                Store.Tree.add tree paths.(n) (string_of_int i))
          in
          Store.set_tree_exn v ~info [] tree;
          if t.clear then Store.Tree.clear tree)
    in
    Store.Repo.close r;
    Fmt.epr "\n[run done]\n%!"

  let main t config size =
    let root = "_build/_bench" in
    let config = config ~root in
    let size () = size ~root in
    let t = { t with root } in
    Eio_main.run @@ fun _ ->
    init t config;
    run t config size

  let main_term config size = Term.(const main $ t $ const config $ const size)

  let () =
    at_exit (fun () ->
        Fmt.epr "tree counters:\n%a\n%!" Store.Tree.dump_counters ())

  let run ~config ~size =
    let info = deprecated_info "Simple benchmark for trees" in
    deprecated_exit @@ deprecated_eval (main_term config size, info)
end

let () =
  Metrics.enable_all ();
  Metrics_gnuplot.set_reporter ()
