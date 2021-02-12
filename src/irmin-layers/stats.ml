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
open! Import

(* This file avoids [option] type and clean functionnal paradigms in order to
   lower the cpu footprint *)

(** Ensure lists are not growing indefinitely by dropping elements. *)
let limit_length_list = 10

type freeze_profile = {
  mutable idx : int;
  mutable t0 : Mtime.t;
  mutable t1 : Mtime.t;
  mutable current_section : string;
  mutable rev_timeline : (string * Mtime.t * float) list;
  mutable contents : int;
  mutable nodes : int;
  mutable commits : int;
  mutable branches : int;
  mutable adds : int;
  mutable skips : int;
  mutable copy_newies_loops : int;
  mutable yields : int;
  mutable outside_totlen : float;
  mutable inside_totlen : float;
  mutable outside_maxlen : string * float;
  mutable inside_maxlen : string * float;
}

let fresh_freeze_profile idx t0 current_section =
  {
    idx;
    t0;
    t1 = t0;
    current_section;
    rev_timeline = [];
    contents = 0;
    nodes = 0;
    commits = 0;
    branches = 0;
    adds = 0;
    skips = 0;
    copy_newies_loops = 0;
    yields = 0;
    outside_totlen = 0.;
    inside_totlen = 0.;
    outside_maxlen = ("never", 0.);
    inside_maxlen = ("never", 0.);
  }

let get_elapsed =
  let c = ref (Mtime_clock.counter ()) in
  fun ~reset ->
    let elapsed = Mtime.Span.to_s (Mtime_clock.count !c) in
    if reset then c := Mtime_clock.counter ();
    elapsed

let freeze_start_counter =
  let c = ref 0 in
  fun () ->
    incr c;
    !c - 1

let freeze_profiles = ref []
let latest = ref (fresh_freeze_profile ~-1 (Mtime_clock.now ()) "")

let reset_stats () =
  freeze_profiles := [];
  latest := fresh_freeze_profile ~-1 (Mtime_clock.now ()) ""

let freeze_start t0 current_section =
  (* Printf.eprintf "\n> freeze: start\n%!"; *)
  let (_ : float) = get_elapsed ~reset:true in
  latest := fresh_freeze_profile (freeze_start_counter ()) t0 current_section

let freeze_section ev_name' =
  let ev_name = !latest.current_section in
  let now = Mtime_clock.now () in
  let now_inside = get_elapsed ~reset:false +. !latest.inside_totlen in
  (* Format.printf "\n> freeze: end of \"%s\", entering \"%s\"\n%!" ev_name
   *   ev_name'; *)
  !latest.current_section <- ev_name';
  !latest.rev_timeline <- (ev_name, now, now_inside) :: !latest.rev_timeline

let copy_contents () = !latest.contents <- succ !latest.contents
let copy_nodes () = !latest.nodes <- succ !latest.nodes
let copy_commits () = !latest.commits <- succ !latest.commits
let copy_branches () = !latest.branches <- succ !latest.branches
let add () = !latest.adds <- succ !latest.adds
let skip () = !latest.skips <- succ !latest.skips

let copy_newies_loop () =
  !latest.copy_newies_loops <- succ !latest.copy_newies_loops

let get_add_count () = !latest.adds
let get_copied_commits_count () = !latest.commits
let get_copied_branches_count () = !latest.branches
let get_copied_contents_count () = !latest.contents
let get_copied_nodes_count () = !latest.nodes
let get_freeze_count () = List.length !freeze_profiles

let freeze_yield () =
  !latest.yields <- !latest.yields + 1;
  let d1 = get_elapsed ~reset:true in
  let d0 = !latest.inside_totlen in
  !latest.inside_totlen <- d0 +. d1;
  let _, d0 = !latest.inside_maxlen in
  if d1 > d0 then !latest.inside_maxlen <- (!latest.current_section, d1)

let freeze_yield_end () =
  let d1 = get_elapsed ~reset:true in
  let d0 = !latest.outside_totlen in
  !latest.outside_totlen <- d0 +. d1;
  let _, d0 = !latest.outside_maxlen in
  if d1 > d0 then !latest.outside_maxlen <- (!latest.current_section, d1)

let freeze_stop () =
  freeze_yield ();
  !latest.yields <- !latest.yields - 1;
  !latest.t1 <- Mtime_clock.now ();
  !latest.rev_timeline <-
    (!latest.current_section, Mtime_clock.now (), !latest.inside_totlen)
    :: !latest.rev_timeline;
  (* Printf.eprintf "\n> freeze: end of \"%s\" (end)\n%!" !latest.current_section; *)
  let shorter ls =
    List.fold_left
      (fun (acc, i) x ->
        if i < limit_length_list then (x :: acc, i + 1) else (acc, i + 1))
      ([], 0) ls
    |> fst
    |> List.rev
  in
  freeze_profiles := shorter (!latest :: !freeze_profiles)

let pp_latest ppf () =
  let v = !latest in
  let timeline =
    List.fold_right
      (fun (s, t1, t1_block) (acc, t0, t0_block) ->
        let acc = (s, Mtime.span t0 t1, t1_block -. t0_block) :: acc in
        (acc, t1, t1_block))
      v.rev_timeline ([], v.t0, 0.)
    |> (fun (l, _, _) -> l)
    |> List.rev
  in

  let totlen = Mtime.span v.t0 v.t1 in
  let frac_out, frac_in =
    let totlen = Mtime.Span.to_s totlen in
    if totlen = 0. then (0., 0.)
    else (v.outside_totlen /. totlen, v.inside_totlen /. totlen)
  in
  let pp_timeline_section ppf (name, span, span_block) =
    let totlen = Mtime.Span.to_s totlen in
    let span = Mtime.Span.to_s span in
    let pp = Mtime.Span.pp_float_s in
    let pp' ppf v = Format.fprintf ppf "%.0f%%" (v *. 100.) in
    Format.fprintf ppf
      "@\n    %20s took %a (%a of total) and blocked %a (%a of total)." name pp
      span pp' (span /. totlen) pp span_block pp'
      (span_block /. v.inside_totlen)
  in
  Format.fprintf ppf
    "freeze %d blocked %a (%.0f%%), and yielded %a (%.0f%%). Total %a.@\n\
    \  Event counts: %a@\n\
    \  Longest block %a (during \"%s\"). Longest yield %a (during \"%s\"). \
     %.3f yield per sec.@\n\
    \  Timeline: %a@\n"
    v.idx Mtime.Span.pp_float_s v.inside_totlen (frac_in *. 100.)
    Mtime.Span.pp_float_s v.outside_totlen (frac_out *. 100.) Mtime.Span.pp
    totlen
    Fmt.(list ~sep:(any ", ") (pair ~sep:(any ":") string int))
    [
      ("copy_contents", v.contents);
      ("copy_nodes", v.nodes);
      ("copy_commits", v.commits);
      ("copy_branches", v.branches);
      ("adds", v.adds);
      ("skips", v.skips);
      ("yields", v.yields);
      ("copy_newies_loops", v.copy_newies_loops);
    ]
    Mtime.Span.pp_float_s (snd v.inside_maxlen) (fst v.inside_maxlen)
    Mtime.Span.pp_float_s (snd v.outside_maxlen) (fst v.outside_maxlen)
    (float_of_int v.yields /. Mtime.Span.to_s totlen)
    Fmt.(list ~sep:(any "") pp_timeline_section)
    timeline
