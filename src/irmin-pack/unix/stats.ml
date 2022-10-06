(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

open Import
module Metrics = Irmin.Metrics

module Pack_store = struct
  include Stats_intf.Pack_store

  type Metrics.origin += Pack_store_stats
  type stat = t Metrics.t

  let create_pack_store () =
    {
      appended_hashes = 0;
      appended_offsets = 0;
      total = 0;
      from_staging = 0;
      from_lru = 0;
      from_pack_direct = 0;
      from_pack_indexed = 0;
    }

  let init () =
    let initial_state = create_pack_store () in
    Metrics.v ~origin:Pack_store_stats ~name:"pack_store_metric" ~initial_state
      t

  let clear m =
    let v = Metrics.state m in
    v.appended_hashes <- 0;
    v.appended_offsets <- 0;
    v.total <- 0;
    v.from_staging <- 0;
    v.from_lru <- 0;
    v.from_pack_direct <- 0;
    v.from_pack_indexed <- 0

  let export m = Metrics.state m

  let update ~field finds =
    let f v =
      match field with
      | Appended_hashes -> v.appended_hashes <- succ v.appended_hashes
      | Appended_offsets -> v.appended_offsets <- succ v.appended_offsets
      | Staging ->
          v.total <- succ v.total;
          v.from_staging <- succ v.from_staging
      | Lru ->
          v.total <- succ v.total;
          v.from_lru <- succ v.from_lru
      | Pack_direct ->
          v.total <- succ v.total;
          v.from_pack_direct <- succ v.from_pack_direct
      | Pack_indexed ->
          v.total <- succ v.total;
          v.from_pack_indexed <- succ v.from_pack_indexed
      | Not_found ->
          v.total <- succ v.total;
          ()
    in
    let mut = Metrics.Mutate f in
    Metrics.update finds mut

  let cache_misses
      {
        (* Total finds (hits + misses): *)
        total;
        (* In-memory hits: *)
        from_staging;
        from_lru;
        _;
      } =
    total - (from_staging + from_lru)
end

module Index = struct
  include Stats_intf.Index

  type Metrics.origin += Index_stats
  type stat = t Metrics.t

  let create_index () =
    {
      bytes_read = 0;
      nb_reads = 0;
      bytes_written = 0;
      nb_writes = 0;
      nb_merge = 0;
      merge_durations = [];
      nb_replace = 0;
      replace_durations = [];
      nb_sync = 0;
      time_sync = 0.0;
      lru_hits = 0;
      lru_misses = 0;
    }

  let clear (data : stat) =
    let s = Metrics.state data in
    Index.Stats.reset_stats ();
    s.bytes_read <- 0;
    s.nb_reads <- 0;
    s.bytes_written <- 0;
    s.nb_writes <- 0;
    s.nb_merge <- 0;
    s.merge_durations <- [];
    s.nb_replace <- 0;
    s.replace_durations <- [];
    s.nb_sync <- 0;
    s.time_sync <- 0.0;
    s.lru_hits <- 0;
    s.lru_misses <- 0

  let init () =
    let initial_state = create_index () in
    Metrics.v ~origin:Index_stats ~name:"index_metric" ~initial_state t

  let report index =
    let modifier = Metrics.Replace (fun _ -> Stats_intf.Index.S.get ()) in
    Metrics.(update index modifier)

  let export m = Metrics.state m
end

module File_manager = struct
  include Stats_intf.File_manager

  type Metrics.origin += File_manager

  (* NOTE return a new instance each time, since fields are mutable *)
  let create () =
    {
      dict_flushes = 0;
      suffix_flushes = 0;
      index_flushes = 0;
      auto_dict = 0;
      auto_suffix = 0;
      auto_index = 0;
      flush = 0;
    }

  (* NOTE type [stat] is an abstract type in stats.mli *)
  type stat = t Metrics.t

  let init () : stat =
    let initial_state = create () in
    Metrics.v ~origin:File_manager ~name:"file_manager_metric" ~initial_state t

  (* [export] reveals the [t] contained in the [Metrics.t] container *)
  let export : stat -> t = fun m -> Metrics.state m

  (* support [reset_stats] function below *)
  let clear' (t : t) =
    t.dict_flushes <- 0;
    t.suffix_flushes <- 0;
    t.index_flushes <- 0;
    ()

  let clear (t : stat) = clear' (export t)

  (* we want to support an interface where the particular fields of type [t] are reified
     as variants, so that we can call [incr_fm_field Dict_flushes] for example *)

  type field =
    | Dict_flushes
    | Suffix_flushes
    | Index_flushes
    | Auto_dict
    | Auto_suffix
    | Auto_index
    | Flush

  let update ~field t =
    let f t =
      match field with
      | Dict_flushes -> t.dict_flushes <- t.dict_flushes + 1
      | Suffix_flushes -> t.suffix_flushes <- t.suffix_flushes + 1
      | Index_flushes -> t.index_flushes <- t.index_flushes + 1
      | Auto_dict -> t.auto_dict <- t.auto_dict + 1
      | Auto_suffix -> t.auto_suffix <- t.auto_suffix + 1
      | Auto_index -> t.auto_index <- t.auto_index + 1
      | Flush -> t.flush <- t.flush + 1
    in
    Metrics.update t (Metrics.Mutate f)
end

module Latest_gc = struct
  include Stats_intf.Latest_gc

  type Metrics.origin += Latest_gc
  type stat = t Metrics.t

  (* [stats] is the latest_gc stats.
     [t] is [stats option].
     [stat] is the [Metrics] wrapper around [t] *)

  let init : unit -> stat =
   fun () ->
    let initial_state = None in
    Metrics.v ~origin:Latest_gc ~name:"latest_gc_metric" ~initial_state t

  let clear : stat -> unit =
   fun m -> Metrics.update m (Metrics.Replace (fun _ -> None))

  let export : stat -> t = Metrics.state

  let update : stats -> stat -> unit =
   fun stat m -> Metrics.update m (Metrics.Replace (fun _ -> Some stat))

  let new_suffix_end_offset_before_finalise worker =
    match List.assoc_opt "suffix" worker.files with
    | Some x -> x
    | None -> assert false

  let finalise_duration t =
    let steps = t.steps |> List.map (fun (k, v) -> (k, v.wall)) in
    let duration = steps |> List.map snd |> List.fold_left Float.add 0. in
    duration
    -. List.assoc "worker startup" steps
    -. List.assoc "before finalise" steps

  let total_duration t =
    let steps = t.steps |> List.map (fun (k, v) -> (k, v.wall)) in
    steps |> List.map snd |> List.fold_left Float.add 0.

  let finalise_suffix_transfer t =
    let open Int63.Syntax in
    let size_of_the_new_suffix_file =
      t.after_suffix_end_offset - t.before_suffix_end_offset
    in
    let copied_by_the_worker =
      t.worker.suffix_transfers |> List.fold_left Int63.add Int63.zero
    in
    size_of_the_new_suffix_file - copied_by_the_worker
end

type t = {
  pack_store : Pack_store.stat;
  index : Index.stat;
  file_manager : File_manager.stat;
  latest_gc : Latest_gc.stat;
}

let s =
  {
    pack_store = Pack_store.init ();
    index = Index.init ();
    file_manager = File_manager.init ();
    latest_gc = Latest_gc.init ();
  }

let reset_stats () =
  Pack_store.clear s.pack_store;
  Index.clear s.index;
  File_manager.clear s.file_manager;
  Latest_gc.clear s.latest_gc;
  ()

let get () = s
let report_pack_store ~field = Pack_store.update ~field s.pack_store
let report_index () = Index.report s.index

let incr_appended_hashes () =
  Pack_store.update ~field:Pack_store.Appended_hashes s.pack_store

let incr_appended_offsets () =
  Pack_store.update ~field:Pack_store.Appended_offsets s.pack_store

type cache_stats = { cache_misses : float }
type offset_stats = { offset_ratio : float; offset_significance : int }

let div_or_zero a b = if b = 0 then 0. else float_of_int a /. float_of_int b

let get_cache_stats () =
  let pack_store = Metrics.state s.pack_store in
  let cache_misses = Pack_store.cache_misses pack_store in
  { cache_misses = div_or_zero cache_misses pack_store.total }

let get_offset_stats () =
  let pack_store = Metrics.state s.pack_store in
  {
    offset_ratio =
      div_or_zero pack_store.appended_offsets
        (pack_store.appended_offsets + pack_store.appended_hashes);
    offset_significance =
      pack_store.appended_offsets + pack_store.appended_hashes;
  }

let incr_fm_field field = File_manager.update ~field s.file_manager
let report_latest_gc x = Latest_gc.update x s.latest_gc
