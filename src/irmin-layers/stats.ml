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
open! Import

(* This file avoids the [option] type and other clean functionnal paradigms in
   order to lower the cpu footprint. The integer incrementation functions are
   called millions of times per freeze. *)

(** Ensure lists are not growing indefinitely by dropping elements. *)
let limit_length_list = 10

let minimum_seconds_to_be_considered_long = 1.0

type counters = {
  mutable contents : int;
  mutable nodes : int;
  mutable commits : int;
  mutable branches : int;
  mutable adds : int;
  mutable skip_tests : int;
  mutable skips : int;
  mutable yields : int;
}

type freeze_profile = {
  idx : int;
  past_adds : int;
  t0 : Mtime.t;
  mutable t1 : Mtime.t;
  mutable current_section : string;
  mutable current_counters : counters;
  mutable rev_timeline : (string * Mtime.t * float * counters) list;
  mutable copy_newies_loops : int;
  mutable outside_totlen : float;
  mutable inside_totlen : float;
  mutable outside_maxlen : string * float;
  mutable inside_maxlen : string * float;
  mutable rev_longest_yields : (string, int * float) Hashtbl.t;
  mutable rev_longest_blocks : (string, int * float) Hashtbl.t;
}

let fresh_counters () =
  {
    contents = 0;
    nodes = 0;
    commits = 0;
    branches = 0;
    adds = 0;
    skips = 0;
    skip_tests = 0;
    yields = 0;
  }

let fresh_freeze_profile idx t0 initial_section past_adds =
  {
    idx;
    past_adds;
    t0;
    t1 = t0;
    current_section = initial_section;
    current_counters = fresh_counters ();
    rev_timeline = [];
    copy_newies_loops = 0;
    outside_totlen = 0.;
    inside_totlen = 0.;
    outside_maxlen = ("never", 0.);
    inside_maxlen = ("never", 0.);
    rev_longest_yields = Hashtbl.create 2;
    rev_longest_blocks = Hashtbl.create 2;
  }

let get_elapsed =
  let c = ref (Mtime_clock.counter ()) in
  fun ~reset ->
    let elapsed = Mtime.Span.to_s (Mtime_clock.count !c) in
    if reset then c := Mtime_clock.counter ();
    elapsed

let freeze_start_counter =
  let c = ref (-1) in
  fun () ->
    incr c;
    !c

let freeze_profiles = ref []
let latest = ref (fresh_freeze_profile (-1) (Mtime_clock.now ()) "" 0)
let are_all_counters_zero c = c = fresh_counters ()

let reset_stats () =
  freeze_profiles := [];
  latest := fresh_freeze_profile (-1) (Mtime_clock.now ()) "" 0

let freeze_start t0 initial_section =
  let past_adds = !latest.current_counters.adds in
  let (_ : float) = get_elapsed ~reset:true in
  latest :=
    fresh_freeze_profile (freeze_start_counter ()) t0 initial_section past_adds

let freeze_section ev_name' =
  let ev_name = !latest.current_section in
  let now = Mtime_clock.now () in
  let now_inside = get_elapsed ~reset:false +. !latest.inside_totlen in
  let c = !latest.current_counters in
  !latest.current_counters <- fresh_counters ();
  !latest.current_section <- ev_name';
  !latest.rev_timeline <- (ev_name, now, now_inside, c) :: !latest.rev_timeline

let copy_contents () =
  !latest.current_counters.contents <- succ !latest.current_counters.contents

let copy_nodes () =
  !latest.current_counters.nodes <- succ !latest.current_counters.nodes

let copy_commits () =
  !latest.current_counters.commits <- succ !latest.current_counters.commits

let copy_branches () =
  !latest.current_counters.branches <- succ !latest.current_counters.branches

let add () =
  (* The only incrementator not called from freeze. *)
  !latest.current_counters.adds <- succ !latest.current_counters.adds

let skip_test should_skip =
  !latest.current_counters.skip_tests <-
    succ !latest.current_counters.skip_tests;
  if should_skip then
    !latest.current_counters.skips <- succ !latest.current_counters.skips

let copy_newies_loop () =
  !latest.copy_newies_loops <- succ !latest.copy_newies_loops

let fold_counters v f =
  List.fold_left
    (fun acc (_, _, _, c) -> acc + f c)
    (f v.current_counters) v.rev_timeline

let get_add_count () = fold_counters !latest (fun c -> c.adds)
let get_copied_commits_count () = fold_counters !latest (fun c -> c.commits)
let get_copied_branches_count () = fold_counters !latest (fun c -> c.branches)
let get_copied_contents_count () = fold_counters !latest (fun c -> c.contents)
let get_copied_nodes_count () = fold_counters !latest (fun c -> c.nodes)
let get_freeze_count () = List.length !freeze_profiles

let freeze_yield () =
  !latest.current_counters.yields <- succ !latest.current_counters.yields;
  let d1 = get_elapsed ~reset:true in
  let d0 = !latest.inside_totlen in
  !latest.inside_totlen <- d0 +. d1;
  let _, d0 = !latest.inside_maxlen in
  if d1 > d0 then !latest.inside_maxlen <- (!latest.current_section, d1);
  if d1 >= minimum_seconds_to_be_considered_long then
    let tbl = !latest.rev_longest_blocks in
    let s = !latest.current_section in
    let new_entry =
      match Hashtbl.find_opt tbl s with
      | None -> (1, d1)
      | Some (i, d) -> (i + 1, d +. d1)
    in
    Hashtbl.replace tbl s new_entry

let freeze_yield_end () =
  let d1 = get_elapsed ~reset:true in
  let d0 = !latest.outside_totlen in
  !latest.outside_totlen <- d0 +. d1;
  let _, d0 = !latest.outside_maxlen in
  if d1 > d0 then !latest.outside_maxlen <- (!latest.current_section, d1);
  if d1 >= minimum_seconds_to_be_considered_long then
    let tbl = !latest.rev_longest_yields in
    let s = !latest.current_section in
    let new_entry =
      match Hashtbl.find_opt tbl s with
      | None -> (1, d1)
      | Some (i, d) -> (i + 1, d +. d1)
    in
    Hashtbl.replace tbl s new_entry

let freeze_stop () =
  let v = !latest in
  freeze_yield ();
  v.current_counters.yields <- pred v.current_counters.yields;
  v.t1 <- Mtime_clock.now ();
  v.rev_timeline <-
    (v.current_section, Mtime_clock.now (), v.inside_totlen, v.current_counters)
    :: v.rev_timeline;
  v.current_counters <- fresh_counters ();
  let shorter ls =
    List.fold_left
      (fun (acc, i) x ->
        if i < limit_length_list then (x :: acc, i + 1) else (acc, i + 1))
      ([], 0) ls
    |> fst
    |> List.rev
  in
  freeze_profiles := shorter (v :: !freeze_profiles)

let pp_latest_when_any ppf v =
  let ongoing = Mtime.equal v.t0 v.t1 in
  let timeline =
    let l, t0, t0_block =
      List.fold_right
        (fun (s, t1, t1_block, counters) (acc, t0, t0_block) ->
          let span = Mtime.span t0 t1 in
          let span_block = t1_block -. t0_block in
          let data = (s, span, span_block, counters, false) in
          (data :: acc, t1, t1_block))
        v.rev_timeline ([], v.t0, 0.)
    in
    let l =
      if not ongoing then l
      else
        let t1 = Mtime_clock.now () in
        let t1_block = get_elapsed ~reset:false +. v.inside_totlen in
        let span = Mtime.span t0 t1 in
        let span_block = t1_block -. t0_block in
        (v.current_section, span, span_block, v.current_counters, true) :: l
    in
    List.rev l
  in
  let totlen =
    if ongoing then Mtime.span v.t0 (Mtime_clock.now ())
    else Mtime.span v.t0 v.t1
  in
  let frac_out, frac_in =
    let totlen = Mtime.Span.to_s totlen in
    (v.outside_totlen /. totlen, v.inside_totlen /. totlen)
  in
  let pp_timeline_timings_section ppf (name, span, span_block, _, is_ongoing) =
    let totlen = Mtime.Span.to_s totlen in
    let span = Mtime.Span.to_s span in
    let pp = Mtime.Span.pp_float_s in
    let pp' ppf v = Format.fprintf ppf "%.0f%%" (v *. 100.) in
    Format.fprintf ppf
      "@\n    %20s took %a (%a of total) and blocked %a (%a of total)%s." name
      pp span pp' (span /. totlen) pp span_block pp'
      (span_block /. v.inside_totlen)
      (if is_ongoing then " (ongoing)" else "")
  in
  let pp_timeline_timings ppf =
    Format.fprintf ppf "%a"
      Fmt.(list ~sep:(any "") pp_timeline_timings_section)
      timeline
  in
  let pp_timeline_counters_section ppf (name, _, _, c, is_ongoing) =
    Format.fprintf ppf "@\n    %20s %a%s." name
      Fmt.(list ~sep:(any ", ") (pair ~sep:(any ":") string int))
      [
        ("copy_contents", c.contents);
        ("copy_nodes", c.nodes);
        ("copy_commits", c.commits);
        ("copy_branches", c.branches);
        ("adds", c.adds);
        ("skips", c.skips);
        ("skip_tests", c.skip_tests);
        ("yields", c.yields);
      ]
      (if is_ongoing then " (ongoing)" else "")
  in
  let pp_timeline_counters ppf =
    let timeline =
      List.filter
        (fun (_, _, _, c, _) -> not @@ are_all_counters_zero c)
        timeline
    in
    Format.fprintf ppf "%a"
      Fmt.(list ~sep:(any "") pp_timeline_counters_section)
      timeline
  in
  let pp_long_segment ppf (action_name, (max_section, max_len), tbl) =
    if max_len = 0. then Format.fprintf ppf "No %ss" action_name
    else if Hashtbl.length tbl = 0 then
      Format.fprintf ppf "Longest %s: %a (during \"%s\")" action_name
        Mtime.Span.pp_float_s max_len max_section
    else
      let pp_per_section ppf (section, (count, totlen)) =
        let pp_if_max ppf =
          if section = max_section then
            Format.fprintf ppf " (max:%a)" Mtime.Span.pp_float_s max_len
        in
        if count = 1 then
          Format.fprintf ppf "1 long %s in \"%s\" of %a" action_name section
            Mtime.Span.pp_float_s totlen
        else
          Format.fprintf ppf "%d long %ss in \"%s\" of ~%a%t" count action_name
            section Mtime.Span.pp_float_s
            (totlen /. float_of_int count)
            pp_if_max
      in
      Format.fprintf ppf "Longests %ss: [%a]" action_name
        Fmt.(list ~sep:(any "; ") pp_per_section)
        (Hashtbl.to_seq tbl |> List.of_seq)
  in
  Format.fprintf ppf
    "freeze %d (%s) blocked %a (%.0f%%), and yielded %a (%.0f%%). Total %a. %d \
     adds before freeze. Copy newies loops: %d.@\n\
    \  %a.@\n\
    \  %a.@\n\
    \  Timeline timings: %t@\n\
    \  Timeline counters: %t@\n"
    v.idx
    (if ongoing then "ongoing" else "finished")
    Mtime.Span.pp_float_s v.inside_totlen (frac_in *. 100.)
    Mtime.Span.pp_float_s v.outside_totlen (frac_out *. 100.) Mtime.Span.pp
    totlen v.past_adds v.copy_newies_loops pp_long_segment
    ("block", v.inside_maxlen, v.rev_longest_blocks)
    pp_long_segment
    ("yield", v.outside_maxlen, v.rev_longest_yields)
    pp_timeline_timings pp_timeline_counters

let pp_latest ppf =
  let v = !latest in
  if v.idx = -1 then Format.fprintf ppf "No freeze started yet."
  else pp_latest_when_any ppf v
