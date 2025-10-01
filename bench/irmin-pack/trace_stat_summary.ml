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

(** Conversion of a [Stat_trace] to a summary that is both pretty-printable and
    exportable to JSON.

    The main type [t] here isn't versioned like a [Stat_trace.t] is.

    Computing a summary may take a long time if the input [Stat_trace] is long.
    Count ~1000 commits per second.

    This file is NOT meant to be used from Tezos, as opposed to some other
    "trace_*" files. *)

module Def = Trace_definitions.Stat_trace
module Conf = Trace_stat_summary_conf
module Utils = Trace_stat_summary_utils
module Vs = Utils.Variable_summary
module Seq = Trace_common.Seq

(* Section 1/4 - Type of a summary. *)

type curve = Utils.curve [@@deriving repr]

(** A stat trace can be chunked into {e blocks}. A {e blocks} is made of 2
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
    Tezos blocks up to the Edo protocol. *)
module Span = struct
  module Key = struct
    type atom_seen =
      [ `Add | `Remove | `Find | `Mem | `Mem_tree | `Checkout | `Copy | `Commit ]
    [@@deriving repr, enum]
    (** The unitary operations played. We recorded the length of all of these.
    *)

    type atom = [ atom_seen | `Unseen ]
    (** [atom_seen] plus the time between operations. The sum of these is the
        total time. *)

    type phase = [ `Buildup | `Commit ]
    (** The two major phases. The sum of these is the total time *)

    type t =
      [ `Add
      | `Remove
      | `Find
      | `Mem
      | `Mem_tree
      | `Checkout
      | `Copy
      | `Commit
      | `Unseen
      | `Buildup
      | `Block ]
    [@@deriving repr, enum]
    (** All spans.

        Redefined (i.e. no inheritance) for derivers. *)

    let all_atoms_seen : atom_seen list =
      List.init (max_atom_seen + 1) (fun i -> atom_seen_of_enum i |> Option.get)

    let all : t list = List.init (max + 1) (fun i -> of_enum i |> Option.get)

    let to_string : [< t ] -> string =
     fun v ->
      match String.split_on_char '"' (Irmin.Type.to_string t v) with
      | [ ""; s; "" ] -> s |> String.lowercase_ascii
      | _ -> failwith "Could not encode span name to json"

    let of_string : string -> (t, [ `Msg of string ]) result =
     fun s ->
      let s = "\"" ^ String.capitalize_ascii s ^ "\"" in
      match Irmin.Type.of_string t s with Ok v -> Ok v | Error _ as e -> e
  end

  module Val = struct
    type t = {
      count : Vs.t;
      cumu_count : Vs.t;
      duration : Vs.t;
      duration_log_scale : Vs.t;
      cumu_duration : Vs.t;
    }
    [@@deriving repr]
    (** The [count] variable is the number of occurences of a span per block and
        [cumu_count] is the cumulative from the beginning.

        The [duration] variable is the length of a span occurence and
        [cumu_duration] is the cumulative from the beginning. The x axis for the
        [evolution] is the number of blocks.

        The [count] for [commit] and [unseen] is trivialy equal to 1. The same
        is almost true for [checkout] too. *)
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
      | _ -> failwith "Could not encode node name to json"

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

type bag_stat = {
  value_before_commit : Vs.t;
  value_after_commit : Vs.t;
  diff_per_block : Vs.t;
  diff_per_buildup : Vs.t;
  diff_per_commit : Vs.t;
}
[@@deriving repr]
(** Summary of an entry contained in [Def.bag_of_stat].

    Properties of such a variables:

    - Is sampled before each commit operation.
    - Is sampled after each commit operation.
    - Is sampled in header.
    - Most of these entries are expected to grow linearly, it implies that no
      smoothing is necessary for the downsampled curve in these cases, and that
      the histogram is best viewed on a linear scale - as opposed to a log
      scale. The other entries are summarised using
      [~is_linearly_increasing:false].

    The [value_after_commit] is initially fed with the value in the header (i.e.
    the value recorded just before the start of the play). *)

type finds = {
  total : bag_stat;
  from_staging : bag_stat;
  from_lru : bag_stat;
  from_pack_direct : bag_stat;
  from_pack_indexed : bag_stat;
  missing : bag_stat;
  cache_miss : bag_stat;
}
[@@deriving repr]

type pack = {
  finds : finds;
  appended_hashes : bag_stat;
  appended_offsets : bag_stat;
  inode_add : bag_stat;
  inode_remove : bag_stat;
  inode_of_seq : bag_stat;
  inode_of_raw : bag_stat;
  inode_rec_add : bag_stat;
  inode_rec_remove : bag_stat;
  inode_to_binv : bag_stat;
  inode_decode_bin : bag_stat;
  inode_encode_bin : bag_stat;
}
[@@deriving repr]

type tree = {
  contents_hash : bag_stat;
  contents_find : bag_stat;
  contents_add : bag_stat;
  node_hash : bag_stat;
  node_mem : bag_stat;
  node_add : bag_stat;
  node_find : bag_stat;
  node_val_v : bag_stat;
  node_val_find : bag_stat;
  node_val_list : bag_stat;
}
[@@deriving repr]

type index = {
  bytes_read : bag_stat;
  nb_reads : bag_stat;
  bytes_written : bag_stat;
  nb_writes : bag_stat;
  bytes_both : bag_stat;
  nb_both : bag_stat;
  nb_merge : bag_stat;
  cumu_data_bytes : bag_stat;
  merge_durations : float list;
}
[@@deriving repr]

type gc = {
  minor_words : bag_stat;
  promoted_words : bag_stat;
  major_words : bag_stat;
  minor_collections : bag_stat;
  major_collections : bag_stat;
  compactions : bag_stat;
  major_heap_bytes : bag_stat;
  major_heap_top_bytes : curve;
}
[@@deriving repr]

type disk = {
  index_data : bag_stat;
  index_log : bag_stat;
  index_log_async : bag_stat;
  store_dict : bag_stat;
  store_pack : bag_stat;
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
  span : Span.map;
  block_count : int;
  cpu_usage : Vs.t;
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

(** Accumulator for the [span] field of [t]. *)
module Span_folder = struct
  type span_acc = {
    sum_count : int;
    sum_duration : float;
    count : Vs.acc;
    cumu_count : Vs.acc;
    duration : Vs.acc;
    duration_log_scale : Vs.acc;
    cumu_duration : Vs.acc;
  }

  type acc = {
    per_span : span_acc Span.Map.t;
    seen_atoms_durations_in_block : float list Span.Map.t;
    timestamp_before : float;
  }

  let create timestamp_before block_count =
    let seen_atoms_durations_in_block0 =
      List.map
        (fun atom_seen -> (atom_seen, []))
        (Span.Key.all_atoms_seen :> Span.Key.t list)
      |> List.to_seq
      |> Span.Map.of_seq
    in
    let acc0 =
      let acc0_per_span =
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
          sum_count = 0;
          sum_duration = 0.;
          count;
          cumu_count;
          duration;
          duration_log_scale;
          cumu_duration;
        }
      in
      let per_span =
        List.map (fun span -> (span, acc0_per_span)) Span.Key.all
        |> List.to_seq
        |> Span.Map.of_seq
      in
      {
        per_span;
        seen_atoms_durations_in_block = seen_atoms_durations_in_block0;
        timestamp_before;
      }
    in

    let accumulate acc row =
      let on_atom_seen_duration32 acc (span : Span.Key.atom_seen) (d : int32) =
        let d = Int32.float_of_bits d in
        let span = (span :> Span.Key.t) in
        let seen_atoms_durations_in_block =
          let m = acc.seen_atoms_durations_in_block in
          let l = d :: Span.Map.find span m in
          Span.Map.add span l m
        in
        { acc with seen_atoms_durations_in_block }
      in
      let on_durations (span : Span.Key.t) (new_durations : float list) acc =
        let acc' = Span.Map.find span acc.per_span in
        let new_count = List.length new_durations in
        let sum_count = acc'.sum_count + new_count in
        let sum_duration =
          acc'.sum_duration +. List.fold_left ( +. ) 0. new_durations
        in
        let count = Vs.accumulate acc'.count [ float_of_int new_count ] in
        let cumu_count =
          Vs.accumulate acc'.cumu_count [ float_of_int sum_count ]
        in
        let duration = Vs.accumulate acc'.duration new_durations in
        let duration_log_scale =
          Vs.accumulate acc'.duration_log_scale new_durations
        in
        let cumu_duration = Vs.accumulate acc'.cumu_duration [ sum_duration ] in
        let acc' =
          {
            sum_count;
            sum_duration;
            count;
            cumu_count;
            duration;
            duration_log_scale;
            cumu_duration;
          }
        in
        { acc with per_span = Span.Map.add span acc' acc.per_span }
      in
      let on_commit (c : Def.commit) acc =
        let list_one span =
          Span.Map.find span acc.seen_atoms_durations_in_block
        in
        let sum_one span = List.fold_left ( +. ) 0. (list_one span) in
        let sum_several spans =
          let spans = (spans :> Span.Key.t list) in
          List.fold_left (fun cumu span -> cumu +. sum_one span) 0. spans
        in
        let total_duration = c.after.timestamp_wall -. acc.timestamp_before in
        let acc =
          acc
          |> on_durations `Add (list_one `Add)
          |> on_durations `Remove (list_one `Remove)
          |> on_durations `Find (list_one `Find)
          |> on_durations `Mem (list_one `Mem)
          |> on_durations `Mem_tree (list_one `Mem_tree)
          |> on_durations `Checkout (list_one `Checkout)
          |> on_durations `Copy (list_one `Copy)
          |> on_durations `Commit (list_one `Commit)
          |> on_durations `Unseen
               [ total_duration -. sum_several Span.Key.all_atoms_seen ]
          |> on_durations `Buildup [ total_duration -. sum_one `Commit ]
          |> on_durations `Block [ total_duration ]
        in
        {
          acc with
          seen_atoms_durations_in_block = seen_atoms_durations_in_block0;
          timestamp_before = c.after.timestamp_wall;
        }
      in
      match row with
      | `Add d -> on_atom_seen_duration32 acc `Add d
      | `Remove d -> on_atom_seen_duration32 acc `Remove d
      | `Find d -> on_atom_seen_duration32 acc `Find d
      | `Mem d -> on_atom_seen_duration32 acc `Mem d
      | `Mem_tree d -> on_atom_seen_duration32 acc `Mem_tree d
      | `Checkout d -> on_atom_seen_duration32 acc `Checkout d
      | `Copy d -> on_atom_seen_duration32 acc `Copy d
      | `Commit c ->
          on_atom_seen_duration32 acc `Commit c.Def.duration |> on_commit c
    in

    let finalise { per_span; _ } =
      Span.Map.map
        (fun acc ->
          {
            Span.Val.count = Vs.finalise acc.count;
            cumu_count = Vs.finalise acc.cumu_count;
            duration = Vs.finalise acc.duration;
            duration_log_scale = Vs.finalise acc.duration_log_scale;
            cumu_duration = Vs.finalise acc.cumu_duration;
          })
        per_span
    in

    Utils.Parallel_folders.folder acc0 accumulate finalise
end

(** Summary computation for statistics recorded in [Def.bag_of_stat]. *)
module Bag_stat_folder = struct
  type acc = {
    value_before_commit : Vs.acc;
    value_after_commit : Vs.acc;
    diff_per_block : Vs.acc;
    diff_per_buildup : Vs.acc;
    diff_per_commit : Vs.acc;
    prev_value : float;
    (* constants *)
    value_of_bag : Def.bag_of_stats -> float;
    should_cumulate_value : bool;
  }

  let create_acc ?(is_linearly_increasing = true)
      ?(should_cumulate_value = false) header block_count value_of_bag =
    let value_in_header = value_of_bag header.Def.initial_stats in
    let f =
      if is_linearly_increasing then create_vs_exact else create_vs_smooth
    in
    let value_before_commit = f block_count in
    let value_before_commit = Vs.accumulate value_before_commit [] in
    let value_after_commit = f block_count in
    let value_after_commit =
      Vs.accumulate value_after_commit [ value_in_header ]
    in
    let diff_per_block = create_vs_smooth block_count in
    let diff_per_block = Vs.accumulate diff_per_block [] in
    let diff_per_buildup = create_vs_smooth block_count in
    let diff_per_buildup = Vs.accumulate diff_per_buildup [] in
    let diff_per_commit = create_vs_smooth block_count in
    let diff_per_commit = Vs.accumulate diff_per_commit [] in
    {
      value_before_commit;
      value_after_commit;
      diff_per_block;
      diff_per_buildup;
      diff_per_commit;
      prev_value = value_in_header;
      value_of_bag;
      should_cumulate_value;
    }

  let accumulate acc row =
    match row with
    | `Commit c ->
        let va = acc.value_of_bag c.Def.before in
        let vb = acc.value_of_bag c.Def.after in
        let va, vb =
          if acc.should_cumulate_value then
            (acc.prev_value +. va, acc.prev_value +. va +. vb)
          else (va, vb)
        in
        let diff_block = vb -. acc.prev_value in
        let diff_buildup = va -. acc.prev_value in
        let diff_commit = vb -. va in
        let value_before_commit =
          Vs.accumulate acc.value_before_commit [ va ]
        in
        let value_after_commit = Vs.accumulate acc.value_after_commit [ vb ] in
        let diff_per_block = Vs.accumulate acc.diff_per_block [ diff_block ] in
        let diff_per_buildup =
          Vs.accumulate acc.diff_per_buildup [ diff_buildup ]
        in
        let diff_per_commit =
          Vs.accumulate acc.diff_per_commit [ diff_commit ]
        in
        {
          acc with
          value_before_commit;
          value_after_commit;
          diff_per_block;
          diff_per_buildup;
          diff_per_commit;
          prev_value = vb;
        }
    | _ -> acc

  let finalise acc : bag_stat =
    {
      value_before_commit = Vs.finalise acc.value_before_commit;
      value_after_commit = Vs.finalise acc.value_after_commit;
      diff_per_block = Vs.finalise acc.diff_per_block;
      diff_per_buildup = Vs.finalise acc.diff_per_buildup;
      diff_per_commit = Vs.finalise acc.diff_per_commit;
    }

  let create ?should_cumulate_value ?is_linearly_increasing header block_count
      value_of_bag =
    let acc0 =
      create_acc ?should_cumulate_value ?is_linearly_increasing header
        block_count value_of_bag
    in
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

let cpu_usage_folder header block_count =
  let acc0 =
    let vs = create_vs_smooth block_count in
    let vs = Vs.accumulate vs [] in
    ( header.Def.initial_stats.timestamp_wall,
      header.Def.initial_stats.timestamp_cpu,
      vs )
  in
  let accumulate ((prev_wall, prev_cpu, vs) as acc) = function
    | `Commit c ->
        let span_wall = c.Def.after.timestamp_wall -. prev_wall in
        let span_cpu = c.Def.after.timestamp_cpu -. prev_cpu in
        ( c.Def.after.timestamp_wall,
          c.Def.after.timestamp_cpu,
          Vs.accumulate vs [ span_cpu /. span_wall ] )
    | _ -> acc
  in
  let finalise (_, _, vs) = Vs.finalise vs in
  Utils.Parallel_folders.folder acc0 accumulate finalise

(** Substract the first and the last timestamps and count the number of span. *)
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
  let bs_folder_of_bag_getter ?should_cumulate_value ?is_linearly_increasing
      value_of_bag =
    Bag_stat_folder.create ?should_cumulate_value ?is_linearly_increasing header
      block_count value_of_bag
  in

  let finds_folder =
    let construct total from_staging from_lru from_pack_direct from_pack_indexed
        missing cache_miss =
      {
        total;
        from_staging;
        from_lru;
        from_pack_direct;
        from_pack_indexed;
        missing;
        cache_miss;
      }
    in
    let acc0 =
      let open Utils.Parallel_folders in
      let ofi = float_of_int in
      open_ construct
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.finds.total)
      |+ bs_folder_of_bag_getter (fun bag ->
             ofi bag.Def.pack.finds.from_staging)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.finds.from_lru)
      |+ bs_folder_of_bag_getter (fun bag ->
             ofi bag.Def.pack.finds.from_pack_direct)
      |+ bs_folder_of_bag_getter (fun bag ->
             ofi bag.Def.pack.finds.from_pack_indexed)
      |+ bs_folder_of_bag_getter (fun bag ->
             let open Def in
             let v = bag.pack.finds in
             v.total
             - v.from_staging
             - v.from_lru
             - v.from_pack_direct
             - v.from_pack_indexed
             |> ofi)
      |+ bs_folder_of_bag_getter (fun bag ->
             let open Def in
             let v = bag.pack.finds in
             v.total - v.from_staging - v.from_lru |> ofi)
      |> seal
    in
    Utils.Parallel_folders.folder acc0 Utils.Parallel_folders.accumulate
      Utils.Parallel_folders.finalise
  in

  let pack_folder =
    let construct finds appended_hashes appended_offsets inode_add inode_remove
        inode_of_seq inode_of_raw inode_rec_add inode_rec_remove inode_to_binv
        inode_decode_bin inode_encode_bin =
      {
        finds;
        appended_hashes;
        appended_offsets;
        inode_add;
        inode_remove;
        inode_of_seq;
        inode_of_raw;
        inode_rec_add;
        inode_rec_remove;
        inode_to_binv;
        inode_decode_bin;
        inode_encode_bin;
      }
    in
    let acc0 =
      let open Utils.Parallel_folders in
      let ofi = float_of_int in
      open_ construct
      |+ finds_folder
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.appended_hashes)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.appended_offsets)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.inode_add)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.inode_remove)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.inode_of_seq)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.inode_of_raw)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.inode_rec_add)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.inode_rec_remove)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.inode_to_binv)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.inode_decode_bin)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.pack.inode_encode_bin)
      |> seal
    in
    Utils.Parallel_folders.folder acc0 Utils.Parallel_folders.accumulate
      Utils.Parallel_folders.finalise
  in

  let tree_folder =
    let construct contents_hash contents_find contents_add node_hash node_mem
        node_add node_find node_val_v node_val_find node_val_list =
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
      }
    in
    let acc0 =
      let open Utils.Parallel_folders in
      let ofi = float_of_int in
      open_ construct
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.contents_hash)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.contents_find)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.contents_add)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_hash)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_mem)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_add)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_find)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_val_v)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_val_find)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.tree.node_val_list)
      |> seal
    in
    Utils.Parallel_folders.folder acc0 Utils.Parallel_folders.accumulate
      Utils.Parallel_folders.finalise
  in

  let index_folder =
    let construct bytes_read nb_reads bytes_written nb_writes bytes_both nb_both
        nb_merge cumu_data_bytes merge_durations =
      {
        bytes_read;
        nb_reads;
        bytes_written;
        nb_writes;
        bytes_both;
        nb_both;
        nb_merge;
        cumu_data_bytes;
        merge_durations;
      }
    in
    let acc0 =
      let open Utils.Parallel_folders in
      let ofi = float_of_int in
      open_ construct
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.index.bytes_read)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.index.nb_reads)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.index.bytes_written)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.index.nb_writes)
      |+ bs_folder_of_bag_getter (fun bag ->
             ofi (bag.Def.index.bytes_read + bag.Def.index.bytes_written))
      |+ bs_folder_of_bag_getter (fun bag ->
             ofi (bag.Def.index.nb_reads + bag.Def.index.nb_writes))
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.index.nb_merge)
      |+ bs_folder_of_bag_getter ~should_cumulate_value:true (fun bag ->
             (* When 1 merge occured, [data_size] bytes were written.

                When 2 merge occured, [data_size * 2 - log_size] bytes were
                written. But here we just count [data_size * 2]. *)
             let merge_count =
               List.length bag.Def.index.new_merge_durations |> Int64.of_int
             in
             let data_size = bag.Def.disk.index_data in
             Int64.to_float (Int64.mul merge_count data_size))
      |+ merge_durations_folder
      |> seal
    in
    Utils.Parallel_folders.folder acc0 Utils.Parallel_folders.accumulate
      Utils.Parallel_folders.finalise
  in

  let gc_folder =
    let construct minor_words promoted_words major_words minor_collections
        major_collections compactions major_heap_bytes major_heap_top_bytes =
      {
        minor_words;
        promoted_words;
        major_words;
        minor_collections;
        major_collections;
        compactions;
        major_heap_bytes;
        major_heap_top_bytes;
      }
    in
    let acc0 =
      let open Utils.Parallel_folders in
      let ofi = float_of_int in
      let ws = header.Def.word_size / 8 |> float_of_int in
      open_ construct
      |+ bs_folder_of_bag_getter (fun bag -> bag.Def.gc.minor_words)
      |+ bs_folder_of_bag_getter (fun bag -> bag.Def.gc.promoted_words)
      |+ bs_folder_of_bag_getter (fun bag -> bag.Def.gc.major_words)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.gc.minor_collections)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.gc.major_collections)
      |+ bs_folder_of_bag_getter (fun bag -> ofi bag.Def.gc.compactions)
      |+ bs_folder_of_bag_getter ~is_linearly_increasing:false (fun bag ->
             ofi bag.Def.gc.heap_words *. ws)
      |+ major_heap_top_bytes_folder header block_count
      |> seal
    in
    Utils.Parallel_folders.folder acc0 Utils.Parallel_folders.accumulate
      Utils.Parallel_folders.finalise
  in

  let disk_folder =
    let construct index_data index_log index_log_async store_dict store_pack =
      { index_data; index_log; index_log_async; store_dict; store_pack }
    in
    let acc0 =
      let open Utils.Parallel_folders in
      let ofi64 = Int64.to_float in
      open_ construct
      |+ bs_folder_of_bag_getter (fun bag -> ofi64 bag.Def.disk.index_data)
      |+ bs_folder_of_bag_getter ~is_linearly_increasing:false (fun bag ->
             ofi64 bag.Def.disk.index_log)
      |+ bs_folder_of_bag_getter ~is_linearly_increasing:false (fun bag ->
             ofi64 bag.Def.disk.index_log_async)
      |+ bs_folder_of_bag_getter (fun bag -> ofi64 bag.Def.disk.store_dict)
      |+ bs_folder_of_bag_getter (fun bag ->
             (* This would not be linearly increasing with irmin layers *)
             ofi64 bag.Def.disk.store_pack)
      |> seal
    in
    Utils.Parallel_folders.folder acc0 Utils.Parallel_folders.accumulate
      Utils.Parallel_folders.finalise
  in

  let construct (elapsed_wall, elapsed_cpu, op_count) elapsed_wall_over_blocks
      elapsed_cpu_over_blocks span cpu_usage_variable pack tree index gc disk
      watched_nodes =
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
      span;
      pack;
      tree;
      cpu_usage = cpu_usage_variable;
      index;
      gc;
      disk;
      store = { watched_nodes };
    }
  in

  let pf0 =
    let open Utils.Parallel_folders in
    open_ construct
    |+ misc_stats_folder header
    |+ elapsed_wall_over_blocks_folder header block_count
    |+ elapsed_cpu_over_blocks_folder header block_count
    |+ Span_folder.create header.initial_stats.timestamp_wall block_count
    |+ cpu_usage_folder header block_count
    |+ pack_folder
    |+ tree_folder
    |+ index_folder
    |+ gc_folder
    |+ disk_folder
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
  let path = Eio.Path.native_exn path in
  let j = Fmt.str "%a\n" (Irmin.Type.pp_json t) v in
  let chan = open_out path in
  output_string chan j;
  [%logs.app "Summary saved to %s" path];
  close_out chan;
  Unix.chmod path 0o444
