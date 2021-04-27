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

(** Conversion of a [Stat_trace] to a summary that is both pretty-printable and
    exportable to JSON.

    The main type [t] here isn't versioned like a [Stat_trace.t] is.

    Computing a summary may take a long time if the input [Stat_trace] is long.
    Count ~1000 commits per second.

    This file is NOT meant to be used from Tezos, as opposed to some other
    "trace_*" files.

    {3 Phases}

    A stat trace can be chunked into {e blocks}. A {e blocks} is made of 2
    phases, first the {e buildup} and then the {e commit}.

    The duration of a {e buildup} can be split into 2 parts: 1. the time spend
    in each operation and 2. the sum of the time spent between, before and after
    all operations. The first being the {e seen buildup} and the second the
    {e unseen buildup}.

    The total duration of a block is the sum of the durations of the {e commit},
    the {e seen buildup} and the {e unseen buildup}.

    Caveat: There isn't a one to one correspondance between summary blocks and
    Tezos' blocks. A Tezos block is associated to a commit, but a commit is not
    necessarily associated to a Tezos block. There are ~50 more commits than
    Tezos blocks up to the Edo protocol.

    {3 REMOVE ME Before Merge, next PR}

    TODO: Document / clean trace_stats.exe (rename?)

    TODO: Document / clean pp

    - (change the "Where is better" scheme)
    - Print section spacers (rework types to do so)
    - (do I separate concerns more between summaries, I should?)
    - color comma ? or color units?

    TODO: A create_pp_seconds with examples. YES!

    # *)

module Def = Trace_definitions.Stat_trace
module Conf = Trace_stat_summary_conf
module Utils = Trace_stat_summary_utils
module Vs = Utils.Variable_summary

(* Section 1/4 - Type of a summary. *)

type curve = Utils.curve [@@deriving repr]

module Op = struct
  module Key = struct
    type t =
      [ `Add
      | `Remove
      | `Find
      | `Mem
      | `Mem_tree
      | `Checkout
      | `Copy
      | `Commit
      | `Unseen ]
    [@@deriving repr, enum]

    let to_string v =
      match String.split_on_char '"' (Irmin.Type.to_string t v) with
      | [ ""; s; "" ] -> s |> String.lowercase_ascii
      | _ -> failwith "Could not encode op name to json"

    let of_string s =
      let s = "\"" ^ String.capitalize_ascii s ^ "\"" in
      match Irmin.Type.of_string t s with Ok v -> Ok v | Error _ as e -> e
  end

  module Val = struct
    (* The [count] count curve for [commit] and [unseen] is trivialy equal to 1.
       Almost for [checkout] too.

       Since there are always, per block, 1 commit and 1 unseen (and 1 checkout,
       except for the first block), the [count]
    *)
    type t = {
      count : Vs.t;
      cumu_count : Vs.t;
      duration : Vs.t;
      duration_log_scale : Vs.t;
      cumu_duration : Vs.t;
    }
    [@@deriving repr]
  end

  module Map = Map.Make (struct
    type t = Key.t

    let compare = compare
  end)

  type map = Val.t Map.t

  let map_t : map Irmin.Type.t =
    let encode map =
      Map.bindings map |> List.map (fun (k, v) -> (Key.to_string k, v))
    in
    let decode l =
      let key_of_string k =
        match Key.of_string k with
        | Ok k -> k
        | Error (`Msg msg) ->
            Fmt.failwith "Could not convert string back to key: %s" msg
      in
      List.map (fun (k, v) -> (key_of_string k, v)) l
      |> List.to_seq
      |> Map.of_seq
    in
    Irmin.Type.(map (Json.assoc Val.t) decode encode)
end

module Watched_node = struct
  module Key = struct
    type t = Def.watched_node [@@deriving repr]

    let to_string v =
      match String.split_on_char '"' (Irmin.Type.to_string t v) with
      | [ ""; s; "" ] -> s |> String.lowercase_ascii
      | _ -> failwith "Could not encode op name to json"

    let of_string s =
      let s = "\"" ^ String.capitalize_ascii s ^ "\"" in
      match Irmin.Type.of_string t s with Ok v -> Ok v | Error _ as e -> e
  end

  module Val = struct
    type t = { value : Vs.t; diff_per_block : Vs.t } [@@deriving repr]
  end

  module Map = Map.Make (struct
    type t = Key.t

    let compare = compare
  end)

  type map = Val.t Map.t

  let map_t : map Irmin.Type.t =
    let encode map =
      Map.bindings map |> List.map (fun (k, v) -> (Key.to_string k, v))
    in
    let decode l =
      let key_of_string k =
        match Key.of_string k with
        | Ok k -> k
        | Error (`Msg msg) ->
            Fmt.failwith "Could not convert string back to key: %s" msg
      in
      List.map (fun (k, v) -> (key_of_string k, v)) l
      |> List.to_seq
      |> Map.of_seq
    in
    Irmin.Type.(map (Json.assoc Val.t) decode encode)
end

type linear_bag_stat = {
  value : Vs.t;
  diff_per_block : Vs.t;
  diff_per_buildup : Vs.t;
  diff_per_commit : Vs.t;
}
[@@deriving repr]
(** Summary of an entry of [Def.bag_of_stat], recorded in header, before and
    after each commit. *)

type pack = {
  finds : linear_bag_stat;
  cache_misses : linear_bag_stat;
  appended_hashes : linear_bag_stat;
  appended_offsets : linear_bag_stat;
}
[@@deriving repr]

type tree = {
  contents_hash : linear_bag_stat;
  contents_find : linear_bag_stat;
  contents_add : linear_bag_stat;
  node_hash : linear_bag_stat;
  node_mem : linear_bag_stat;
  node_add : linear_bag_stat;
  node_find : linear_bag_stat;
  node_val_v : linear_bag_stat;
  node_val_find : linear_bag_stat;
  node_val_list : linear_bag_stat;
}
[@@deriving repr]

type index = {
  bytes_read : linear_bag_stat;
  nb_reads : linear_bag_stat;
  bytes_written : linear_bag_stat;
  nb_writes : linear_bag_stat;
  nb_merge : linear_bag_stat;
  merge_durations : float list;
}
[@@deriving repr]

type gc = {
  minor_words : linear_bag_stat;
  promoted_words : linear_bag_stat;
  major_words : linear_bag_stat;
  minor_collections : linear_bag_stat;
  major_collections : linear_bag_stat;
  compactions : linear_bag_stat;
  major_heap_bytes : linear_bag_stat;
  major_heap_top_bytes : curve;
}
[@@deriving repr]

type disk = {
  index_data : linear_bag_stat;
  index_log : linear_bag_stat;
  index_log_async : linear_bag_stat;
  store_dict : linear_bag_stat;
  store_pack : linear_bag_stat;
}
[@@deriving repr]

type store = { watched_nodes : Watched_node.map } [@@deriving repr]

type t = {
  summary_timeofday : float;
  summary_hostname : string;
  curves_sample_count : int;
  moving_average_half_life_ratio : float;
  (* Stats from [Def.header]. *)
  config : Def.config;
  hostname : string;
  word_size : int;
  timeofday : float;
  timestamp_wall0 : float;
  timestamp_cpu0 : float;
  (* Stats derived from [Def.row]s. *)
  elapsed_wall : float;
  elapsed_wall_over_blocks : Utils.curve;
  elapsed_cpu : float;
  elapsed_cpu_over_blocks : Utils.curve;
  op_count : int;
  ops : Op.map;
  block_count : int;
  index : index;
  pack : pack;
  tree : tree;
  gc : gc;
  disk : disk;
  store : store;
}
[@@deriving repr]

(* Section 2/4 - Converters from stat_strace to element of summary. *)
let create_vs block_count =
  Vs.create_acc ~distribution_bin_count:Conf.histo_bin_count
    ~out_sample_count:Conf.curves_sample_count
    ~in_period_count:(block_count + 1) ~evolution_resampling_mode:`Next_neighbor

let create_vs_exact block_count =
  create_vs block_count ~evolution_smoothing:`None ~scale:`Linear

let create_vs_smooth block_count =
  let hlr = Conf.moving_average_half_life_ratio in
  let rt = Conf.moving_average_relevance_threshold in
  create_vs block_count ~evolution_smoothing:(`Ema (hlr, rt)) ~scale:`Linear

let create_vs_smooth_log block_count =
  let hlr = Conf.moving_average_half_life_ratio in
  let rt = Conf.moving_average_relevance_threshold in
  create_vs block_count ~evolution_smoothing:(`Ema (hlr, rt)) ~scale:`Log

(** Accumulator for the [ops] field of [t]. *)
module Ops_folder = struct
  let ops = List.init (Op.Key.max + 1) (fun i -> Op.Key.of_enum i |> Option.get)

  type op_acc = {
    durations_in_block : float list;
    sum_count : int;
    sum_duration : float;
    count : Vs.acc;
    cumu_count : Vs.acc;
    duration : Vs.acc;
    duration_log_scale : Vs.acc;
    cumu_duration : Vs.acc;
  }

  type acc = { per_op : op_acc Op.Map.t; timestamp_before : float }

  let create timestamp_before block_count =
    let acc0 =
      let acc0_per_op =
        let count = create_vs_smooth block_count in
        let count = Vs.accumulate count [] in
        let cumu_count = create_vs_exact block_count in
        let cumu_count = Vs.accumulate cumu_count [ 0. ] in
        let duration = create_vs_smooth block_count in
        let duration = Vs.accumulate duration [] in
        let duration_log_scale = create_vs_smooth_log block_count in
        let duration_log_scale = Vs.accumulate duration_log_scale [] in
        let cumu_duration = create_vs_exact block_count in
        let cumu_duration = Vs.accumulate cumu_duration [ 0. ] in
        {
          durations_in_block = [];
          sum_count = 0;
          sum_duration = 0.;
          count;
          cumu_count;
          duration;
          duration_log_scale;
          cumu_duration;
        }
      in
      let per_op =
        List.map (fun op -> (op, acc0_per_op)) ops
        |> List.to_seq
        |> Op.Map.of_seq
      in
      { per_op; timestamp_before }
    in

    let accumulate acc row =
      let on_duration acc op d =
        let acc' = Op.Map.find op acc.per_op in
        let acc' =
          {
            acc' with
            durations_in_block = d :: acc'.durations_in_block;
            sum_count = acc'.sum_count + 1;
            sum_duration = acc'.sum_duration +. d;
          }
        in
        { acc with per_op = Op.Map.add op acc' acc.per_op }
      in
      let on_duration32 acc op d = on_duration acc op (Int32.float_of_bits d) in
      let on_commit (c : Def.commit) acc =
        let seen_duration =
          Op.Map.fold
            (fun _ { durations_in_block; _ } x ->
              List.fold_left ( +. ) x durations_in_block)
            acc.per_op 0.
        in
        let total_duration = c.after.timestamp_wall -. acc.timestamp_before in
        let unseen_duration = total_duration -. seen_duration in
        let acc = on_duration acc `Unseen unseen_duration in

        let per_op =
          Op.Map.map
            (fun acc' ->
              let xs = acc'.durations_in_block in
              let lenxs = List.length xs |> float_of_int in
              let count = Vs.accumulate acc'.count [ lenxs ] in
              let cumu_count =
                Vs.accumulate acc'.cumu_count [ acc'.sum_count |> float_of_int ]
              in
              let duration = Vs.accumulate acc'.duration xs in
              let duration_log_scale = Vs.accumulate acc'.duration xs in
              let cumu_duration =
                Vs.accumulate acc'.cumu_duration [ acc'.sum_duration ]
              in
              {
                acc' with
                durations_in_block = [];
                count;
                cumu_count;
                duration;
                duration_log_scale;
                cumu_duration;
              })
            acc.per_op
        in
        { per_op; timestamp_before = c.after.timestamp_wall }
      in
      match row with
      | `Add d -> on_duration32 acc `Add d
      | `Remove d -> on_duration32 acc `Remove d
      | `Find d -> on_duration32 acc `Find d
      | `Mem d -> on_duration32 acc `Mem d
      | `Mem_tree d -> on_duration32 acc `Mem_tree d
      | `Checkout d -> on_duration32 acc `Checkout d
      | `Copy d -> on_duration32 acc `Copy d
      | `Commit c -> on_duration32 acc `Commit c.Def.duration |> on_commit c
    in
    let finalise { per_op; _ } =
      Op.Map.map
        (fun acc ->
          {
            Op.Val.count = Vs.finalise acc.count;
            cumu_count = Vs.finalise acc.cumu_count;
            duration = Vs.finalise acc.duration;
            duration_log_scale = Vs.finalise acc.duration_log_scale;
            cumu_duration = Vs.finalise acc.cumu_duration;
          })
        per_op
    in
    Utils.Parallel_folders.folder acc0 accumulate finalise
end

(** Summary computation for statistics recorded in [Def.bag_of_stat].

    Properties of such a variables:

    - Is sampled before each commit operation.
    - Is sampled after each commit operation.
    - Is sampled in header.
    - Is expected to grow linearly (or mostly, i.e. [disk.store_pack] with
      layered store). This implies that the curves/histos are best viewed on a
      linear scale - as opposed to a log scale. *)
module Linear_bag_stat_folder = struct
  type acc = {
    value : Vs.acc;
    diff_per_block : Vs.acc;
    diff_per_buildup : Vs.acc;
    diff_per_commit : Vs.acc;
    prev_value : float;
    value_of_bag : Def.bag_of_stats -> float;
  }

  let create_acc header block_count value_of_bag =
    let value_in_header = value_of_bag header.Def.initial_stats in
    let value = create_vs_exact block_count in
    let value = Vs.accumulate value [ value_in_header ] in
    let diff_per_block = create_vs_smooth block_count in
    let diff_per_block = Vs.accumulate diff_per_block [] in
    let diff_per_buildup = create_vs_smooth block_count in
    let diff_per_buildup = Vs.accumulate diff_per_buildup [] in
    let diff_per_commit = create_vs_smooth block_count in
    let diff_per_commit = Vs.accumulate diff_per_commit [] in
    {
      value;
      diff_per_block;
      diff_per_buildup;
      diff_per_commit;
      prev_value = value_in_header;
      value_of_bag;
    }

  let accumulate acc row =
    match row with
    | `Commit c ->
        let va = acc.value_of_bag c.Def.before in
        let vb = acc.value_of_bag c.Def.after in
        let diff_block = vb -. acc.prev_value in
        let diff_buildup = va -. acc.prev_value in
        let diff_commit = vb -. va in
        let value = Vs.accumulate acc.value [ vb ] in
        let diff_per_block = Vs.accumulate acc.diff_per_block [ diff_block ] in
        let diff_per_buildup =
          Vs.accumulate acc.diff_per_buildup [ diff_buildup ]
        in
        let diff_per_commit =
          Vs.accumulate acc.diff_per_commit [ diff_commit ]
        in
        {
          acc with
          value;
          diff_per_block;
          diff_per_buildup;
          diff_per_commit;
          prev_value = vb;
        }
    | _ -> acc

  let finalise acc : linear_bag_stat =
    {
      value = Vs.finalise acc.value;
      diff_per_block = Vs.finalise acc.diff_per_block;
      diff_per_buildup = Vs.finalise acc.diff_per_buildup;
      diff_per_commit = Vs.finalise acc.diff_per_commit;
    }

  let create header block_count value_of_bag =
    let acc0 = create_acc header block_count value_of_bag in
    Utils.Parallel_folders.folder acc0 accumulate finalise
end

(** Accumulator for the [store] field of [t]. *)
module Store_watched_nodes_folder = struct
  type acc_per_node = {
    value : Vs.acc;
    diff_per_block : Vs.acc;
    prev_value : float;
  }

  type acc = acc_per_node list

  let create_acc block_count =
    let acc0_per_node =
      let value = create_vs_exact block_count in
      let value = Vs.accumulate value [] in
      let diff_per_block = create_vs_smooth block_count in
      let diff_per_block = Vs.accumulate diff_per_block [] in
      { value; diff_per_block; prev_value = Float.nan }
    in
    List.map (Fun.const acc0_per_node) Def.watched_nodes

  let accumulate acc row =
    match row with
    | `Commit c ->
        let accumulate_per_node v acc =
          let v = float_of_int v in
          let diff_block = v -. acc.prev_value in
          let value = Vs.accumulate acc.value [ v ] in
          let diff_per_block =
            Vs.accumulate acc.diff_per_block [ diff_block ]
          in
          { value; diff_per_block; prev_value = v }
        in
        List.map2 accumulate_per_node c.Def.store_after.watched_nodes_length acc
    | _ -> acc

  let finalise acc : Watched_node.map =
    List.map2
      (fun k acc ->
        let v =
          {
            Watched_node.Val.value = Vs.finalise acc.value;
            diff_per_block = Vs.finalise acc.diff_per_block;
          }
        in
        (k, v))
      Def.watched_nodes acc
    |> List.to_seq
    |> Watched_node.Map.of_seq

  let create block_count =
    let acc0 = create_acc block_count in
    Utils.Parallel_folders.folder acc0 accumulate finalise
end

(** Build a resampled curve of [gc.top_heap_words] *)
let major_heap_top_bytes_folder header block_count =
  let ws = header.Def.word_size / 8 |> float_of_int in
  let len0 = block_count + 1 in
  let len1 = Conf.curves_sample_count in
  let v0 = float_of_int header.Def.initial_stats.gc.top_heap_words *. ws in
  let acc0 = Utils.Resample.create_acc `Next_neighbor ~len0 ~len1 ~v00:v0 in
  let accumulate acc = function
    | `Commit c ->
        Utils.Resample.accumulate acc
          (float_of_int c.Def.after.gc.top_heap_words *. ws)
    | _ -> acc
  in
  let finalise = Utils.Resample.finalise in
  Utils.Parallel_folders.folder acc0 accumulate finalise

(** Build a resampled curve of timestamps. *)
let elapsed_wall_over_blocks_folder header block_count =
  let open Def in
  let len0 = block_count + 1 in
  let len1 = Conf.curves_sample_count in
  let v0 = header.initial_stats.timestamp_wall in
  let acc0 = Utils.Resample.create_acc `Interpolate ~len0 ~len1 ~v00:v0 in
  let accumulate acc = function
    | `Commit c -> Utils.Resample.accumulate acc c.after.timestamp_wall
    | _ -> acc
  in
  let finalise acc =
    Utils.Resample.finalise acc |> List.map (fun t -> t -. v0)
  in
  Utils.Parallel_folders.folder acc0 accumulate finalise

(** Build a resampled curve of timestamps. *)
let elapsed_cpu_over_blocks_folder header block_count =
  let open Def in
  let len0 = block_count + 1 in
  let len1 = Conf.curves_sample_count in
  let v0 = header.initial_stats.timestamp_cpu in
  let acc0 = Utils.Resample.create_acc `Interpolate ~len0 ~len1 ~v00:v0 in
  let accumulate acc = function
    | `Commit c -> Utils.Resample.accumulate acc c.after.timestamp_cpu
    | _ -> acc
  in
  let finalise acc =
    Utils.Resample.finalise acc |> List.map (fun t -> t -. v0)
  in
  Utils.Parallel_folders.folder acc0 accumulate finalise

(** Build a list of all the merge durations. *)
let merge_durations_folder =
  let acc0 = [] in
  let accumulate l = function
    | `Commit c ->
        let l = List.rev_append c.Def.before.index.new_merge_durations l in
        let l = List.rev_append c.Def.after.index.new_merge_durations l in
        l
    | _ -> l
  in
  let finalise = List.rev in
  Utils.Parallel_folders.folder acc0 accumulate finalise

(** Substract the first and the last timestamps and count the number of ops. *)
let misc_stats_folder header =
  let open Def in
  let acc0 = (0., 0., 0) in
  let accumulate (t, t', count) = function
    | `Commit c -> (c.after.timestamp_wall, c.after.timestamp_cpu, count + 1)
    | _ -> (t, t', count + 1)
  in
  let finalise (t, t', count) =
    ( t -. header.initial_stats.timestamp_wall,
      t' -. header.initial_stats.timestamp_cpu,
      count )
  in
  Utils.Parallel_folders.folder acc0 accumulate finalise

(* Section 3/4 - Converter from stat_strace to summary *)

(** Fold over [row_seq] and produce the summary.

    {3 Parallel Folders}

    Almost all entries in [t] require to independently fold over the rows of the
    stat trace, but we want:

    - not to fully load the trace in memory,
    - not to reread the trace from disk once for each entry,
    - this current file to be verbose and simple,
    - to have fun with GADTs and avoid mutability.

    All the boilerplate is hidden behind [Utils.Parallel_folders], a
    datastructure that holds all folder functions, takes care of feeding the
    rows to those folders, and preseves the types.

    In the code below, [pf0] is the initial parallel folder, before the first
    accumulation. Each [|+ ...] statement declares a [acc, accumulate, finalise]
    triplet, i.e. a folder.

    [val acc : acc] is the initial empty accumulation of a folder.

    [val accumulate : acc -> row -> acc] needs to be folded over all rows of the
    stat trace. Calling [Parallel_folders.accumulate pf row] will feed [row] to
    every folders.

    [val finalise : acc -> v] has to be applied on the final [acc] of a folder
    in order to produce the final value of that folder - which value is meant to
    be stored in [Trace_stat_summary.t]. Calling [Parallel_folders.finalise pf]
    will finalise all folders and pass their result to [construct]. *)
let summarise' header block_count (row_seq : Def.row Seq.t) =
  let ws = header.Def.word_size / 8 |> float_of_int in
  let lbs_folder_of_bag_getter value_of_bag =
    Linear_bag_stat_folder.create header block_count value_of_bag
  in

  let construct (elapsed_wall, elapsed_cpu, op_count) elapsed_wall_over_blocks
      elapsed_cpu_over_blocks ops finds cache_misses appended_hashes
      appended_offsets contents_hash contents_find contents_add node_hash
      node_mem node_add node_find node_val_v node_val_find node_val_list
      bytes_read nb_reads bytes_written nb_writes nb_merge merge_durations
      minor_words promoted_words major_words minor_collections major_collections
      compactions major_heap_bytes major_heap_top_bytes index_data index_log
      index_log_async store_dict store_pack watched_nodes =
    {
      summary_hostname = Unix.gethostname ();
      summary_timeofday = Unix.gettimeofday ();
      elapsed_wall;
      elapsed_cpu;
      op_count;
      block_count;
      curves_sample_count = Conf.curves_sample_count;
      moving_average_half_life_ratio = Conf.moving_average_half_life_ratio;
      config = header.config;
      hostname = header.hostname;
      word_size = header.word_size;
      timeofday = header.timeofday;
      timestamp_wall0 = header.initial_stats.timestamp_wall;
      timestamp_cpu0 = header.initial_stats.timestamp_cpu;
      elapsed_wall_over_blocks;
      elapsed_cpu_over_blocks;
      ops;
      pack = { finds; cache_misses; appended_hashes; appended_offsets };
      tree =
        {
          contents_hash;
          contents_find;
          contents_add;
          node_hash;
          node_mem;
          node_add;
          node_find;
          node_val_v;
          node_val_find;
          node_val_list;
        };
      index =
        {
          bytes_read;
          nb_reads;
          bytes_written;
          nb_writes;
          nb_merge;
          merge_durations;
        };
      gc =
        {
          minor_words;
          promoted_words;
          major_words;
          minor_collections;
          major_collections;
          compactions;
          major_heap_bytes;
          major_heap_top_bytes;
        };
      disk = { index_data; index_log; index_log_async; store_dict; store_pack };
      store = { watched_nodes };
    }
  in

  let pf0 =
    let open Utils.Parallel_folders in
    let ofi = float_of_int in
    let ofi64 = Int64.to_float in
    open_ construct
    (* misc folders *)
    |+ misc_stats_folder header
    |+ elapsed_wall_over_blocks_folder header block_count
    |+ elapsed_cpu_over_blocks_folder header block_count
    (* [ops] folder *)
    |+ Ops_folder.create header.initial_stats.timestamp_wall block_count
    (* [pack] folders *)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.finds)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.cache_misses)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.appended_hashes)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.appended_offsets)
    (* [tree] folders *)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.contents_hash)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.contents_find)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.contents_add)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_hash)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_mem)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_add)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_find)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_val_v)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_val_find)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_val_list)
    (* [index] folders *)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.index.bytes_read)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.index.nb_reads)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.index.bytes_written)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.index.nb_writes)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.index.nb_merge)
    |+ merge_durations_folder
    (* [gc] folders *)
    |+ lbs_folder_of_bag_getter (fun bag -> bag.Def.gc.minor_words)
    |+ lbs_folder_of_bag_getter (fun bag -> bag.Def.gc.promoted_words)
    |+ lbs_folder_of_bag_getter (fun bag -> bag.Def.gc.major_words)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.gc.minor_collections)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.gc.major_collections)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.gc.compactions)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi bag.Def.gc.heap_words *. ws)
    |+ major_heap_top_bytes_folder header block_count
    (* [disk] folders *)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi64 bag.Def.disk.index_data)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi64 bag.Def.disk.index_log)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi64 bag.Def.disk.index_log_async)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi64 bag.Def.disk.store_dict)
    |+ lbs_folder_of_bag_getter (fun bag -> ofi64 bag.Def.disk.store_pack)
    (* [store] folder *)
    |+ Store_watched_nodes_folder.create block_count
    |> seal
  in
  Seq.fold_left Utils.Parallel_folders.accumulate pf0 row_seq
  |> Utils.Parallel_folders.finalise

(** Turn a stat trace into a summary.

    The number of blocks to consider may be provided in order to truncate the
    summary. *)
let summarise ?block_count trace_stat_path =
  let block_count =
    match block_count with
    | Some block_count -> block_count
    | None ->
        (* The trace has to be iterated a first time in order to retrieve the
         * number of blocks. This is needed to:
         * - define the moving average momentum,
         * - stop the row sequence immediately after the last commit. *)
        Trace_definitions.Stat_trace.open_reader trace_stat_path
        |> snd
        |> Seq.fold_left
             (fun count op ->
               match op with `Commit _ -> count + 1 | _ -> count)
             0
  in
  if block_count <= 0 then invalid_arg "Can't summarise an empty stat trace";
  let header, row_seq =
    Trace_definitions.Stat_trace.open_reader trace_stat_path
  in
  let row_seq =
    let aux (seq, commit_count) =
      if commit_count >= block_count then None
      else
        match seq () with
        | Seq.Nil ->
            failwith
              "summarise reached the end of the stat trace before \
               'block_count' commits were seen"
        | Seq.Cons ((`Commit _ as op), seq) -> Some (op, (seq, commit_count + 1))
        | Seq.Cons (op, seq) -> Some (op, (seq, commit_count))
    in
    Seq.unfold aux (row_seq, 0)
  in
  summarise' header block_count row_seq

(* Section 4/4 - Conversion from summary to json file *)

let save_to_json v path =
  let j = Fmt.strf "%a\n" (Irmin.Type.pp_json t) v in
  let chan = open_out path in
  output_string chan j;
  Logs.app (fun l -> l "Summary saved to %s" path);
  close_out chan;
  Unix.chmod path 0o444
