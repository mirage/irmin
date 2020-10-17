(*
 * Copyright (c) 2013-2020 Ioana Cristescu <ioana@tarides.com>
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
  mutable nb_freeze : int;
  mutable copied_contents : int list;
  mutable copied_nodes : int list;
  mutable copied_commits : int list;
  mutable copied_branches : int list;
  mutable waiting_freeze : float list;
  mutable completed_freeze : float list;
  mutable skips : int;
}

type i = {
  mutable contents : int;
  mutable nodes : int;
  mutable commits : int;
  mutable branches : int;
  mutable adds : int;
}

let fresh_stats_t () =
  {
    nb_freeze = 0;
    copied_contents = [];
    copied_nodes = [];
    copied_commits = [];
    copied_branches = [];
    waiting_freeze = [];
    completed_freeze = [];
    skips = 0;
  }

let fresh_stats_i () =
  { contents = 0; nodes = 0; commits = 0; branches = 0; adds = 0 }

let stats_t = fresh_stats_t ()

let stats_i = fresh_stats_i ()

let reset_stats_i () =
  stats_i.contents <- 0;
  stats_i.nodes <- 0;
  stats_i.commits <- 0;
  stats_i.branches <- 0

let reset_stats () =
  stats_t.nb_freeze <- 0;
  stats_t.copied_contents <- [];
  stats_t.copied_nodes <- [];
  stats_t.copied_commits <- [];
  stats_t.copied_branches <- [];
  stats_t.skips <- 0;
  reset_stats_i ()

let get () =
  {
    nb_freeze = stats_t.nb_freeze;
    copied_contents = stats_i.contents :: stats_t.copied_contents;
    copied_nodes = stats_i.nodes :: stats_t.copied_nodes;
    copied_commits = stats_i.commits :: stats_t.copied_commits;
    copied_branches = stats_i.branches :: stats_t.copied_branches;
    waiting_freeze = stats_t.waiting_freeze;
    completed_freeze = stats_t.completed_freeze;
    skips = stats_t.skips;
  }

(** Ensure lists are not growing indefinitely by dropping elements. *)
let limit_length_list = 10

let drop_last_elements n =
  let shorter ls =
    List.fold_left
      (fun (acc, i) x -> if i < n then (x :: acc, i + 1) else (acc, i + 1))
      ([], 0) ls
    |> fst
    |> List.rev
  in
  stats_t.copied_contents <- shorter stats_t.copied_contents;
  stats_t.copied_nodes <- shorter stats_t.copied_nodes;
  stats_t.copied_commits <- shorter stats_t.copied_commits;
  stats_t.copied_branches <- shorter stats_t.copied_branches;
  stats_t.completed_freeze <- shorter stats_t.completed_freeze;
  stats_t.waiting_freeze <- shorter stats_t.waiting_freeze

let freeze () =
  stats_t.nb_freeze <- succ stats_t.nb_freeze;
  stats_t.skips <- 0;
  if stats_t.nb_freeze <> 1 then (
    stats_t.copied_contents <- stats_i.contents :: stats_t.copied_contents;
    stats_t.copied_nodes <- stats_i.nodes :: stats_t.copied_nodes;
    stats_t.copied_commits <- stats_i.commits :: stats_t.copied_commits;
    stats_t.copied_branches <- stats_i.branches :: stats_t.copied_branches;
    reset_stats_i ());
  if List.length stats_t.completed_freeze >= limit_length_list then
    drop_last_elements (limit_length_list / 2)

let copy_contents () = stats_i.contents <- succ stats_i.contents

let copy_nodes () = stats_i.nodes <- succ stats_i.nodes

let copy_commits () = stats_i.commits <- succ stats_i.commits

let copy_branches () = stats_i.branches <- succ stats_i.branches

let skip () = stats_t.skips <- succ stats_t.skips

let add () = stats_i.adds <- succ stats_i.adds

let get_adds () = stats_i.adds

let reset_adds () = stats_i.adds <- 0

let with_timer (operation : [ `Freeze | `Waiting ]) f =
  let timer = Mtime_clock.counter () in
  f () >|= fun () ->
  let span = Mtime.Span.to_us (Mtime_clock.count timer) in
  match operation with
  | `Freeze -> stats_t.completed_freeze <- span :: stats_t.completed_freeze
  | `Waiting -> stats_t.waiting_freeze <- span :: stats_t.waiting_freeze
