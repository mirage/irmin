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

type t = { pack_store : Pack_store.stat; index : Index.stat }

let s = { pack_store = Pack_store.init (); index = Index.init () }

let reset_stats () =
  Pack_store.clear s.pack_store;
  Index.clear s.index

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
