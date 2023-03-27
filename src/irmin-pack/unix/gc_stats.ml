(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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

module Steps_timer = struct
  type duration = Stats.Latest_gc.duration = {
    wall : float;
    sys : float;
    user : float;
  }

  type t = { timer : duration; prev_stepname : string }

  let get_wtime () =
    (Mtime_clock.now () |> Mtime.to_uint64_ns |> Int64.to_float) /. 1e9

  let get_stime () = Rusage.((get Self).stime)
  let get_utime () = Rusage.((get Self).utime)

  let create first_stepname =
    let wall = get_wtime () in
    let sys = get_stime () in
    let user = get_utime () in
    let timer = { wall; sys; user } in
    { timer; prev_stepname = first_stepname }

  let progress prev next_stepname =
    let wall = get_wtime () in
    let sys = get_stime () in
    let user = get_utime () in
    let next = { wall; sys; user } in

    let wall = next.wall -. prev.timer.wall in
    let sys = next.sys -. prev.timer.sys in
    let user = next.user -. prev.timer.user in
    let delta = (prev.prev_stepname, { wall; sys; user }) in

    let next = { timer = next; prev_stepname = next_stepname } in
    (next, delta)
end

module Main = struct
  module S = Stats.Latest_gc

  type t = { stats : S.stats; timer : Steps_timer.t }
  (** [t] is the running state while computing the stats *)

  let create first_stepname ~generation ~commit_offset
      ~before_suffix_start_offset ~before_suffix_end_offset
      ~after_suffix_start_offset =
    let stats = Irmin.Type.(random S.stats_t |> unstage) () in
    (* [repr] provides doesn't provide a generator that fills a type with
       zeroes but it provides a random generator. Let's use it for our initial
       value. *)
    let stats =
      S.
        {
          stats with
          generation;
          steps = [];
          commit_offset;
          before_suffix_start_offset;
          before_suffix_end_offset;
          after_suffix_start_offset;
        }
    in
    let timer = Steps_timer.create first_stepname in
    { stats; timer }

  let finish_current_step t next_stepname =
    let timer, prev_step = Steps_timer.progress t.timer next_stepname in
    let stats = { t.stats with steps = prev_step :: t.stats.steps } in
    { stats; timer }

  let finalise t worker ~after_suffix_end_offset =
    let t = finish_current_step t "will not appear in the stats" in
    {
      t.stats with
      S.worker;
      after_suffix_end_offset;
      steps = List.rev t.stats.steps;
    }
end

module Worker = struct
  module S = Stats.Latest_gc

  type t = {
    stats : S.worker;
    current_stepname : string;
    prev_wtime : float;
    prev_stime : float;
    prev_utime : float;
    prev_rusage : S.rusage;
    prev_ocaml_gc : S.ocaml_gc;
  }
  (** [t] is the running state while computing the stats *)

  let is_darwin =
    lazy
      (try
         match Unix.open_process_in "uname" |> input_line with
         | "Darwin" -> true
         | _ -> false
       with Unix.Unix_error _ -> false)

  let get_wtime () =
    (Mtime_clock.now () |> Mtime.to_uint64_ns |> Int64.to_float) /. 1e9

  let get_stime () = Rusage.((get Self).stime)
  let get_utime () = Rusage.((get Self).utime)

  let get_rusage : unit -> S.rusage =
   fun () ->
    let Rusage.{ maxrss; minflt; majflt; inblock; oublock; nvcsw; nivcsw; _ } =
      Rusage.(get Self)
    in
    let maxrss =
      if Lazy.force is_darwin then Int64.div maxrss 1000L else maxrss
    in
    S.{ maxrss; minflt; majflt; inblock; oublock; nvcsw; nivcsw }

  let get_ocaml_gc : unit -> S.ocaml_gc =
   fun () ->
    let open Stdlib.Gc in
    let v = quick_stat () in
    S.
      {
        minor_words = v.minor_words;
        promoted_words = v.promoted_words;
        major_words = v.major_words;
        minor_collections = v.minor_collections;
        major_collections = v.major_collections;
        heap_words = v.heap_words;
        compactions = v.compactions;
        top_heap_words = v.top_heap_words;
        stack_size = v.stack_size;
      }

  let create : string -> t =
   fun first_stepname ->
    (* Reseting all irmin-pack stats. We'll reset again at every step.

       Since the GC worker lives alone in a fork, these global variable mutations
       will not interfere with the rest of the world. *)
    Stats.reset_stats ();
    Irmin_pack.Stats.reset_stats ();

    let wtime = get_wtime () in
    let stime = get_stime () in
    let utime = get_utime () in
    let rusage = get_rusage () in
    let ocaml_gc = get_ocaml_gc () in

    let stats =
      S.
        {
          initial_maxrss = rusage.maxrss;
          initial_heap_words = ocaml_gc.heap_words;
          initial_top_heap_words = ocaml_gc.top_heap_words;
          initial_stack_size = ocaml_gc.stack_size;
          steps = [];
          files = [];
          objects_traversed = Int63.zero;
          suffix_transfers = [];
        }
    in
    {
      stats;
      current_stepname = first_stepname;
      prev_utime = utime;
      prev_wtime = wtime;
      prev_stime = stime;
      prev_rusage = rusage;
      prev_ocaml_gc = ocaml_gc;
    }

  let set_objects_traversed t v =
    let stats = { t.stats with objects_traversed = Int63.of_int v } in
    { t with stats }

  let add_suffix_transfer t count =
    let stats =
      { t.stats with suffix_transfers = count :: t.stats.suffix_transfers }
    in
    { t with stats }

  let finish_current_step t next_stepname =
    let wtime = get_wtime () in
    let stime = get_stime () in
    let utime = get_utime () in
    let duration =
      let wall = wtime -. t.prev_wtime in
      let sys = stime -. t.prev_stime in
      let user = utime -. t.prev_utime in
      S.{ wall; sys; user }
    in

    let prev_rusage, rusage =
      let x = t.prev_rusage in
      let y = get_rusage () in
      let ( - ) = Int64.sub in
      ( y,
        S.
          {
            y with
            minflt = y.minflt - x.minflt;
            majflt = y.majflt - x.majflt;
            inblock = y.inblock - x.inblock;
            oublock = y.oublock - x.oublock;
            nvcsw = y.nvcsw - x.nvcsw;
            nivcsw = y.nivcsw - x.nivcsw;
          } )
    in
    let prev_ocaml_gc, ocaml_gc =
      let x = t.prev_ocaml_gc in
      let y = get_ocaml_gc () in
      ( y,
        S.
          {
            y with
            minor_words = y.minor_words -. x.minor_words;
            promoted_words = y.promoted_words -. x.promoted_words;
            major_words = y.major_words -. x.major_words;
            minor_collections = y.minor_collections - x.minor_collections;
            major_collections = y.major_collections - x.major_collections;
            compactions = y.compactions - x.compactions;
          } )
    in

    (* [clone] duplicates a value. Used below to snapshot mutable values. *)
    let clone typerepr v =
      match
        Irmin.Type.to_string typerepr v |> Irmin.Type.of_string typerepr
      with
      | Error _ -> assert false
      | Ok v -> v
    in
    let pack_store =
      Stats.((get ()).pack_store |> Pack_store.export |> clone Pack_store.t)
    in
    Stats.report_index ();
    let index = Stats.((get ()).index |> Index.export |> clone Index.t) in
    let inode =
      Irmin_pack.Stats.((get ()).inode |> Inode.export |> clone Inode.t)
    in
    Stats.reset_stats ();
    Irmin_pack.Stats.reset_stats ();

    let step = S.{ duration; rusage; ocaml_gc; index; pack_store; inode } in

    (* The [steps] list is built in reverse order and reversed in [finalise] *)
    let steps = (t.current_stepname, step) :: t.stats.steps in

    let stats = { t.stats with steps } in
    {
      current_stepname = next_stepname;
      stats;
      prev_wtime = wtime;
      prev_stime = stime;
      prev_utime = utime;
      prev_rusage;
      prev_ocaml_gc;
    }

  let add_file_size t file_name size =
    let stats = { t.stats with files = (file_name, size) :: t.stats.files } in
    { t with stats }

  let finalise : t -> S.worker =
   fun t ->
    let t = finish_current_step t "will not appear in the stats" in
    {
      t.stats with
      steps = List.rev t.stats.steps;
      suffix_transfers = List.rev t.stats.suffix_transfers;
    }
end
