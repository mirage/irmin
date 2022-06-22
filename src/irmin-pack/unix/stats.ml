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

module Metrics = Irmin.Metrics

module Pack_store = struct
  type Metrics.origin += Pack_store_stats

  type field =
    | Appended_hashes
    | Appended_offsets
    | Staging
    | Lru
    | Pack_direct
    | Pack_indexed
    | Not_found
  [@@deriving irmin]

  type t = {
    mutable appended_hashes : int;
    mutable appended_offsets : int;
    mutable total : int;
    mutable from_staging : int;
    mutable from_lru : int;
    mutable from_pack_direct : int;
    mutable from_pack_indexed : int;
  }
  [@@deriving irmin]

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
  module S = Index.Stats

  type Metrics.origin += Index_stats

  type t = Index.Stats.t = {
    mutable bytes_read : int;
    mutable nb_reads : int;
    mutable bytes_written : int;
    mutable nb_writes : int;
    mutable nb_merge : int;
    mutable merge_durations : float list;
    mutable nb_replace : int;
    mutable replace_durations : float list;
    mutable nb_sync : int;
    mutable time_sync : float;
    mutable lru_hits : int;
    mutable lru_misses : int;
  }
  [@@deriving irmin]

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
    let modifier = Metrics.Replace (fun _ -> Index.Stats.get ()) in
    Metrics.(update index modifier)

  let export m = Metrics.state m
end

module File_manager = struct
  type Metrics.origin += File_manager

  type t = {
    mutable dict_flushes : int;
    mutable suffix_flushes : int;
    mutable index_flushes : int;
    mutable auto_dict : int;
    mutable auto_suffix : int;
    mutable auto_index : int;
    mutable flush : int;
  }
  [@@deriving irmin]

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

  (* type [stat] is an abstract type in stats.mli; FIXME what is it for? *)
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

type t = {
  pack_store : Pack_store.stat;
  index : Index.stat;
  file_manager : File_manager.stat;
}

let s =
  {
    pack_store = Pack_store.init ();
    index = Index.init ();
    file_manager = File_manager.init ();
  }

let reset_stats () =
  Pack_store.clear s.pack_store;
  Index.clear s.index;
  File_manager.clear s.file_manager;
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
