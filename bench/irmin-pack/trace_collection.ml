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

(** Trace file construction.

    This file is meant to be used from Tezos. OCaml version 4.09 and the 32bit
    architecture should be supported.

    A module [Make_replayable] has yet to be implemented. *)

module Mtime = Bench_common.Mtime

(** Make state trace collector. *)
module Make_stat (Store : Irmin.Generic_key.KV) = struct
  module Def = Trace_definitions.Stat_trace

  type t = {
    writer : Def.writer;
    store_path : string;
    mutable t0 : Mtime_clock.counter;
    mutable prev_merge_durations : float list;
    mutable commit_before : Def.bag_of_stats * Def.store_before;
  }
  (** Imperative stat trace collector. It is optimised to minimise the CPU
      footprint. *)

  module Bag_of_stats = struct
    let pack () =
      let module Pack_stats = Irmin_pack.Stats in
      let module Unix_pack_stats = Irmin_pack_unix.Stats in
      let pack_s = Pack_stats.get () in
      let unix_s = Unix_pack_stats.get () in
      let inode = Pack_stats.(Inode.export pack_s.inode) in
      let pack_store = Unix_pack_stats.(Pack_store.export unix_s.pack_store) in
      let finds =
        Def.
          {
            total = pack_store.total;
            from_staging = pack_store.from_staging;
            from_lru = pack_store.from_lru;
            from_pack_direct = pack_store.from_pack_direct;
            from_pack_indexed = pack_store.from_pack_indexed;
          }
      in
      Def.
        {
          finds;
          appended_hashes = pack_store.appended_hashes;
          appended_offsets = pack_store.appended_offsets;
          inode_add = inode.inode_add;
          inode_remove = inode.inode_remove;
          inode_of_seq = inode.inode_of_seq;
          inode_of_raw = inode.inode_of_raw;
          inode_rec_add = inode.inode_rec_add;
          inode_rec_remove = inode.inode_rec_remove;
          inode_to_binv = inode.inode_to_binv;
          inode_decode_bin = inode.inode_decode_bin;
          inode_encode_bin = inode.inode_encode_bin;
        }

    let tree () =
      let open Store.Tree in
      let v = counters () in
      Def.
        {
          contents_hash = v.contents_hash;
          contents_find = v.contents_find;
          contents_add = v.contents_add;
          node_hash = v.node_hash;
          node_mem = v.node_mem;
          node_add = v.node_add;
          node_find = v.node_find;
          node_val_v = v.node_val_v;
          node_val_find = v.node_val_find;
          node_val_list = v.node_val_list;
        }

    let index prev_merge_durations =
      let open Index.Stats in
      let v = get () in
      let new_merge_durations =
        if v.merge_durations == prev_merge_durations then []
        else
          (* This is anoying to compute. We can't rely on nb_merge.
             Assume that all merge durations are unique.
             Assume that we never have >10 merges at once.
          *)
          let rec aux acc = function
            | [] -> acc
            | hd :: tl ->
                if List.mem hd prev_merge_durations then (
                  assert (acc = []) (* No oldie after a newies *);
                  aux acc tl)
                else aux ((hd /. 1e6) :: acc) tl
          in
          let l = aux [] v.merge_durations in
          assert (l <> []) (* At least one newie *);
          l
      in
      Def.
        {
          bytes_read = v.bytes_read;
          nb_reads = v.nb_reads;
          bytes_written = v.bytes_written;
          nb_writes = v.nb_writes;
          nb_merge = v.nb_merge;
          new_merge_durations;
        }

    let gc () =
      let open Gc in
      let v = quick_stat () in
      Def.
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

    let size_of_file path =
      let open Unix.LargeFile in
      try (stat path).st_size with Unix.Unix_error _ -> 0L

    let disk store_path =
      let ( / ) left right = Filename.concat left right in
      Def.
        {
          index_data = store_path / "index" / "data" |> size_of_file;
          index_log = store_path / "index" / "log" |> size_of_file;
          index_log_async = store_path / "index" / "log_async" |> size_of_file;
          store_dict = store_path / "store.dict" |> size_of_file;
          store_pack = store_path / "store.pack" |> size_of_file;
        }

    let now () =
      Mtime_clock.now () |> Mtime.to_uint64_ns |> Int64.to_float |> ( *. ) 1e-9

    let create store_path prev_merge_durations =
      Def.
        {
          pack = pack ();
          tree = tree ();
          index = index prev_merge_durations;
          gc = gc ();
          disk = disk store_path;
          timestamp_wall = now ();
          timestamp_cpu = Sys.time ();
        }
  end

  let create_file : Eio.Fs.dir_ty Eio.Path.t -> Def.config -> string -> t =
   fun path config store_path ->
    let header =
      Def.
        {
          config;
          hostname = Unix.gethostname ();
          word_size = Sys.word_size;
          timeofday = Unix.gettimeofday ();
          initial_stats =
            Bag_of_stats.create store_path
              Index.Stats.((get ()).merge_durations);
        }
    in
    let dummy_commit_before =
      ( header.initial_stats,
        Def.{ nodes = 0; leafs = 0; skips = 0; depth = 0; width = 0 } )
    in
    {
      writer = Def.create_file path header;
      store_path;
      t0 = Mtime_clock.counter ();
      prev_merge_durations = Index.Stats.((get ()).merge_durations);
      commit_before = dummy_commit_before;
    }

  let flush { writer; _ } = Def.flush writer
  let close { writer; _ } = Def.close writer
  let remove { writer; _ } = Def.remove writer
  let short_op_begin t = t.t0 <- Mtime_clock.counter ()

  let short_op_end { t0; writer; _ } short_op =
    let duration =
      Mtime_clock.count t0 |> Mtime.span_to_s |> Int32.bits_of_float
    in
    let op =
      match short_op with
      | `Add -> `Add duration
      | `Remove -> `Remove duration
      | `Find -> `Find duration
      | `Mem -> `Mem duration
      | `Mem_tree -> `Mem_tree duration
      | `Checkout -> `Checkout duration
      | `Copy -> `Copy duration
    in
    Def.append_row writer op

  let create_store_before tree =
    let Store.Tree.{ nodes; leafs; skips; depth; width } =
      Store.Tree.stats ~force:false tree
    in
    Def.{ nodes; leafs; skips; depth; width }

  let create_store_after tree =
    let watched_nodes_length =
      List.map
        (fun (_, steps) -> Store.Tree.length tree steps)
        Def.step_list_per_watched_node
    in
    Def.{ watched_nodes_length }

  let commit_begin t tree =
    short_op_begin t;
    let stats_before =
      Bag_of_stats.create t.store_path t.prev_merge_durations
    in
    t.prev_merge_durations <- Index.Stats.((get ()).merge_durations);
    let store_before = create_store_before tree in
    t.commit_before <- (stats_before, store_before)

  let commit_end t tree =
    let duration = Mtime_clock.count t.t0 |> Mtime.span_to_s in
    let duration = duration |> Int32.bits_of_float in
    let stats_after = Bag_of_stats.create t.store_path t.prev_merge_durations in
    t.prev_merge_durations <- Index.Stats.((get ()).merge_durations);
    let store_after = create_store_after tree in
    let op =
      `Commit
        Def.
          {
            duration;
            before = fst t.commit_before;
            after = stats_after;
            store_before = snd t.commit_before;
            store_after;
          }
    in
    Def.append_row t.writer op
end
