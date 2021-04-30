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

(** Pretty printing of one or more summaries.

    This file is NOT meant to be used from Tezos, as opposed to some other
    "trace_*" files.

    This file contains A LOT of uninteresting boilerplate in order to build the
    pretty-printable table. Doing this using multi-level dataframes from pandas
    would make the thing much more simpler. *)

open Trace_stat_summary
module Utils = Trace_stat_summary_utils
module Summary = Trace_stat_summary

module Pb = struct
  include PrintBox

  let () = PrintBox_unicode.setup ()

  (* Some utilities to work with lists instead of array *)

  let transpose_matrix l =
    l
    |> List.map Array.of_list
    |> Array.of_list
    |> PrintBox.transpose
    |> Array.to_list
    |> List.map Array.to_list

  let matrix_to_text m = List.map (List.map PrintBox.text) m
  let align_matrix where = List.map (List.map (PrintBox.align ~h:where ~v:`Top))

  (** Dirty trick to only have vertical bars, and not the horizontal ones *)
  let matrix_with_column_spacers =
    let rec interleave sep = function
      | ([ _ ] | []) as l -> l
      | hd :: tl -> hd :: sep :: interleave sep tl
    in
    List.map (interleave (PrintBox.text " | "))
end

type summary = Summary.t

type where_is_better = [ `A | `L | `H ]
(** Anywhere, Lower, Higher *)

type scalar_type = [ `R | `S | `P ]
(** Real, Seconds, Percent *)

type summary_floor =
  where_is_better * scalar_type * string * (string * curve) list
(** A [summary_floor] contains all the data necessary in order to print a bunch
    of rows, 1 per summary, all displaying the same summary entry. *)

let summary_config_entries =
  [
    `Hostname;
    `Word_size;
    `Timeofday;
    `Inode_config;
    `Store_type;
    `Replay_path_conversion;
  ]

let name_of_summary_config_entry = function
  | `Hostname -> "Hostname"
  | `Word_size -> "Word Size"
  | `Timeofday -> "Start Time"
  | `Inode_config -> "Inode Config"
  | `Store_type -> "Store Type"
  | `Replay_path_conversion -> "Path Conversion"

let cell_of_summary_config (s : summary) = function
  | `Hostname -> s.hostname
  | `Word_size -> Printf.sprintf "%d bits" s.word_size
  | `Timeofday ->
      let open Unix in
      let t = gmtime s.timeofday in
      Printf.sprintf "%04d/%02d/%02d %02d:%02d:%02d (GMT)" (1900 + t.tm_year)
        (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec
  | `Inode_config ->
      let a, b, c = s.config.inode_config in
      Printf.sprintf "mls:%d bf:%d sh:%d" a b c
  | `Store_type -> (
      match s.config.store_type with
      | `Pack -> "pack"
      | `Pack_layered -> "pack-layered")
  | `Replay_path_conversion -> (
      match s.config.setup with
      | `Play _ -> "n/a"
      | `Replay s -> (
          match s.path_conversion with
          | `None -> "none"
          | `V1 -> "v1"
          | `V0_and_v1 -> "v0+v1"
          | `V0 -> "v0"))

let box_of_summaries_config summary_names (summaries : summary list) =
  let row0 =
    if List.length summary_names = 1 then [] else [ "" :: summary_names ]
  in
  let rows =
    List.map
      (fun e ->
        let n = name_of_summary_config_entry e in
        let l = List.map (fun s -> cell_of_summary_config s e) summaries in
        n :: l)
      summary_config_entries
  in
  row0 @ rows |> Pb.matrix_to_text

let sum_curves curves =
  curves
  |> Pb.transpose_matrix
  |> List.map
       (List.fold_left
          (fun acc v -> if Float.is_nan v then acc else acc +. v)
          0.)

let div_curves a b = List.map2 ( /. ) a b
let mul_curves a b = List.map2 ( *. ) a b

let all_9_ops =
  [ `Add; `Remove; `Find; `Mem; `Mem_tree; `Checkout; `Copy; `Commit; `Unseen ]

let all_8_ops =
  [ `Add; `Remove; `Find; `Mem; `Mem_tree; `Checkout; `Copy; `Commit ]

let all_buildup_seen_ops =
  [ `Add; `Remove; `Find; `Mem; `Mem_tree; `Checkout; `Copy ]

let all_buildup_ro_ops = [ `Find; `Mem; `Mem_tree ]
let all_buildup_rw_ops = [ `Add; `Remove; `Copy ]

let floors_of_summaries : string list -> summary list -> summary_floor list =
 fun summary_names summaries ->
  (* Step 1/3 - Prepare the "/data/..." directories floors *)
  let floor_per_node : summary_floor list =
    List.map
      (fun key ->
        let path = List.assoc key Def.path_per_watched_node in
        let name = Printf.sprintf "%s *S" path in
        let curves =
          List.map
            (fun s ->
              (Summary.Watched_node.Map.find key s.store.watched_nodes).value
                .evolution)
            summaries
        in
        let l = List.combine summary_names curves in
        (`A, `R, name, l))
      Def.watched_nodes
  in

  (* Step 2/3 - Prepare the functions to build all the simple floors *)
  let zip : (summary -> curve) -> (string * curve) list =
   fun curve_of_summary ->
    List.map2
      (fun sname s -> (sname, curve_of_summary s))
      summary_names summaries
  in
  let zip_per_block_to_per_sec : (summary -> curve) -> (string * curve) list =
    let sec_per_block =
      List.map
        (fun s ->
          all_9_ops
          |> List.map (fun op ->
                 mul_curves Summary.(Op.Map.find op s.ops).duration.evolution
                   Summary.(Op.Map.find op s.ops).count.evolution)
          |> sum_curves)
        summaries
    in
    fun curve_of_summary ->
      List.map2
        (fun (sname, sec_per_block) s ->
          (sname, div_curves (curve_of_summary s) sec_per_block))
        (List.combine summary_names sec_per_block)
        summaries
  in

  let v : string -> (summary -> Summary.linear_bag_stat) -> summary_floor =
   fun stat_name lbs_of_summary ->
    let curves = zip (fun s -> (lbs_of_summary s).value.evolution) in
    (`L, `R, stat_name, curves)
  in
  let pb : string -> (summary -> Summary.linear_bag_stat) -> summary_floor =
   fun stat_name lbs_of_summary ->
    let curves = zip (fun s -> (lbs_of_summary s).diff_per_block.evolution) in
    (`L, `R, stat_name, curves)
  in
  let ps : string -> (summary -> Summary.linear_bag_stat) -> summary_floor =
   fun stat_name lbs_of_summary ->
    let curves =
      zip_per_block_to_per_sec (fun s ->
          (lbs_of_summary s).diff_per_block.evolution)
    in
    (`L, `R, stat_name, curves)
  in

  let op_count : string -> Op.Key.t -> summary_floor =
   fun name op ->
    let curves =
      zip (fun s -> Summary.(Op.Map.find op s.ops).count.evolution)
    in
    (`L, `R, name, curves)
  in
  let op_duration : string -> Op.Key.t -> summary_floor =
   fun name op ->
    let curves =
      zip (fun s -> Summary.(Op.Map.find op s.ops).duration.evolution)
    in
    (`L, `S, name, curves)
  in
  let op_every_sec : string -> Op.Key.t -> summary_floor =
   fun name op ->
    let curves =
      zip_per_block_to_per_sec (fun s ->
          Summary.(Op.Map.find op s.ops).count.evolution)
    in
    (`L, `R, name, curves)
  in

  let seen_duration =
    zip (fun s ->
        all_buildup_seen_ops
        |> List.map (fun op ->
               mul_curves Summary.(Op.Map.find op s.ops).duration.evolution
                 Summary.(Op.Map.find op s.ops).count.evolution)
        |> sum_curves)
  in
  let all_ops_cumu_count =
    zip (fun s ->
        all_8_ops
        |> List.map (fun op ->
               Summary.(Op.Map.find op s.ops).cumu_count.evolution)
        |> sum_curves)
  in
  let ro_ops_per_sec =
    zip_per_block_to_per_sec (fun s ->
        all_buildup_ro_ops
        |> List.map (fun op -> Summary.(Op.Map.find op s.ops).count.evolution)
        |> sum_curves)
  in
  let rw_ops_per_sec =
    zip_per_block_to_per_sec (fun s ->
        all_buildup_rw_ops
        |> List.map (fun op -> Summary.(Op.Map.find op s.ops).count.evolution)
        |> sum_curves)
  in
  let sum_op_durations_per_block =
    zip (fun s ->
        all_9_ops
        |> List.map (fun op ->
               mul_curves Summary.(Op.Map.find op s.ops).duration.evolution
                 Summary.(Op.Map.find op s.ops).count.evolution)
        |> sum_curves)
  in

  (* Step 3/3 - Build the final list of floors *)
  [
    (`L, `S, "Wall time elapsed *C", zip (fun s -> s.elapsed_wall_over_blocks));
    (`L, `S, "CPU time elapsed *C", zip (fun s -> s.elapsed_cpu_over_blocks));
    (* ops counts *)
    (`L, `R, "Ops count *C", all_ops_cumu_count);
    (`L, `R, "Read only ops per sec *LA", ro_ops_per_sec);
    (`L, `R, "Read write ops per sec *LA", rw_ops_per_sec);
    (* <op> every sec *)
    op_every_sec "Commit every sec *LA" `Commit;
    op_every_sec "Add every sec *LA" `Add;
    op_every_sec "Remove every sec *LA" `Remove;
    op_every_sec "Find every sec *LA" `Find;
    op_every_sec "Mem every sec *LA" `Mem;
    op_every_sec "Mem_tree every sec *LA" `Mem_tree;
    op_every_sec "Copy every sec *LA" `Copy;
    (* <op> per block *)
    op_count "Add count per block *LA" `Add;
    op_count "Remove count per block *LA" `Remove;
    op_count "Find count per block *LA" `Find;
    op_count "Mem count per block *LA" `Mem;
    op_count "Mem_tree count per block *LA" `Mem_tree;
    op_count "Copy count per block *LA" `Copy;
    (* <phase> duration *)
    (`L, `S, "Block duration *LA", sum_op_durations_per_block);
    (`L, `S, "Buildup seen duration *LA", seen_duration);
    op_duration "Buildup unseen duration *LA" `Unseen;
    op_duration "Commit duration *LA" `Commit;
    (* <op> duration *)
    op_duration "Add duration *LA" `Add;
    op_duration "Remove duration *LA" `Remove;
    op_duration "Find duration *LA" `Find;
    op_duration "Mem duration *LA" `Mem;
    op_duration "Mem_tree duration *LA" `Mem_tree;
    op_duration "Copy duration *LA" `Copy;
    op_duration "Checkout duration *LA" `Checkout;
    (* derived from bag_of_stat *)
    pb "pack.finds per block *LA" (fun s -> s.pack.finds);
    pb "pack.cache_misses per block *LA" (fun s -> s.pack.cache_misses);
    pb "pack.appended_hashes per block *LA" (fun s -> s.pack.appended_hashes);
    pb "pack.appended_offsets per block *LA" (fun s -> s.pack.appended_offsets);
    pb "tree.contents_hash per block *LA" (fun s -> s.tree.contents_hash);
    pb "tree.contents_find per block *LA" (fun s -> s.tree.contents_find);
    pb "tree.contents_add per block *LA" (fun s -> s.tree.contents_add);
    pb "tree.node_hash per block *LA" (fun s -> s.tree.node_hash);
    pb "tree.node_mem per block *LA" (fun s -> s.tree.node_mem);
    pb "tree.node_add per block *LA" (fun s -> s.tree.node_add);
    pb "tree.node_find per block *LA" (fun s -> s.tree.node_find);
    pb "tree.node_val_v per block *LA" (fun s -> s.tree.node_val_v);
    pb "tree.node_val_find per block *LA" (fun s -> s.tree.node_val_find);
    pb "tree.node_val_list per block *LA" (fun s -> s.tree.node_val_list);
    v "index.bytes_read *C" (fun s -> s.index.bytes_read);
    pb "index.bytes_read per block *LA" (fun s -> s.index.bytes_read);
    ps "index.bytes_read per sec *LA" (fun s -> s.index.bytes_read);
    v "index.nb_reads *C" (fun s -> s.index.nb_reads);
    pb "index.nb_reads per block *LA" (fun s -> s.index.nb_reads);
    ps "index.nb_reads per sec *LA" (fun s -> s.index.nb_reads);
    v "index.bytes_written *C" (fun s -> s.index.bytes_written);
    pb "index.bytes_written per block *LA" (fun s -> s.index.bytes_written);
    ps "index.bytes_written per sec *LA" (fun s -> s.index.bytes_written);
    v "index.nb_writes *C" (fun s -> s.index.nb_writes);
    pb "index.nb_writes per block *LA" (fun s -> s.index.nb_writes);
    ps "index.nb_writes per sec *LA" (fun s -> s.index.nb_writes);
    v "index.nb_merge *C" (fun s -> s.index.nb_merge);
    v "gc.minor_words allocated *C" (fun s -> s.gc.minor_words);
    pb "gc.minor_words allocated per block *LA" (fun s -> s.gc.minor_words);
    ps "gc.minor_words allocated per sec *LA" (fun s -> s.gc.minor_words);
    v "gc.promoted_words *C" (fun s -> s.gc.promoted_words);
    v "gc.major_words allocated *C" (fun s -> s.gc.major_words);
    pb "gc.major_words allocated per block *LA" (fun s -> s.gc.major_words);
    ps "gc.major_words allocated per sec *LA" (fun s -> s.gc.major_words);
    v "gc.minor_collections *C" (fun s -> s.gc.minor_collections);
    pb "gc.minor_collections per block *LA" (fun s -> s.gc.minor_collections);
    ps "gc.minor_collections per sec *LA" (fun s -> s.gc.minor_collections);
    v "gc.major_collections *C" (fun s -> s.gc.major_collections);
    pb "gc.major_collections per block *LA" (fun s -> s.gc.major_collections);
    ps "gc.major_collections per sec *LA" (fun s -> s.gc.major_collections);
    v "gc.compactions *C" (fun s -> s.gc.compactions);
    ( `L,
      `R,
      "gc.major heap bytes top *C",
      zip (fun s -> s.gc.major_heap_top_bytes) );
    v "gc.major heap bytes *LA" (fun s -> s.gc.major_heap_bytes);
    v "index_data bytes *S" (fun s -> s.disk.index_data);
    pb "index_data bytes per block *LA" (fun s -> s.disk.index_data);
    ps "index_data bytes per sec *LA" (fun s -> s.disk.index_data);
    v "store_pack bytes *S" (fun s -> s.disk.store_pack);
    pb "store_pack bytes per block *LA" (fun s -> s.disk.store_pack);
    ps "store_pack bytes per sec *LA" (fun s -> s.disk.store_pack);
    v "index_log bytes *S" (fun s -> s.disk.index_log);
    v "index_log_async *S" (fun s -> s.disk.index_log_async);
    v "store_dict bytes *S" (fun s -> s.disk.store_dict);
  ]
  @ floor_per_node

let resample_curves_of_floor sample_count (a, b, c, names_and_curves) =
  let names, curves = List.split names_and_curves in
  let curves =
    List.map
      (fun curve ->
        Utils.Resample.resample_vector `Next_neighbor curve sample_count)
      curves
  in
  (a, b, c, List.combine names curves)

let matrix_of_floor (where_is_better, scalar_type, floor_name, names_and_curves)
    =
  let only_one_summary = List.length names_and_curves = 1 in
  let _, curves = List.split names_and_curves in
  let pp_real = Utils.create_pp_real (List.concat curves) in
  let pp_seconds = Utils.create_pp_seconds (List.concat curves) in
  let curve0 = List.hd curves in
  let box_of_scalar row_idx col_idx (v0, v) =
    let ratio = v /. v0 in
    let show_percent =
      if only_one_summary then
        (* Percents are only needed for comparisons between summaries. *)
        `No
      else if col_idx = 0 then
        (* The first columns is usually full of NaNs, showing percents there
           is a waste of space. *)
        `No
      else if where_is_better = `A then
        (* If none of the scalars can be qualified as "better", no need for
           percents. *)
        `Shadow
      else if Float.is_finite ratio = false then
        (* Nan and infinite percents are ugly. *)
        `Shadow
      else if row_idx = 0 then
        (* The first row of a floor is always 100%, it is prettier without
           displaying it. *)
        `Shadow
      else `Yes
    in
    (* let pp_seconds ppf f =
     *   if Float.is_nan f then Format.fprintf ppf "n/a"
     *   else Mtime.Span.pp_float_s ppf f
     * in *)

    (match (scalar_type, show_percent) with
    | `R, `Yes -> Fmt.str "%a %a" pp_real v Utils.pp_percent ratio
    | `R, `Shadow -> Fmt.str "%a     " pp_real v
    | `R, `No -> Fmt.str "%a" pp_real v
    | `S, `Yes -> Fmt.str "%a %a" pp_seconds v Utils.pp_percent ratio
    | `S, `Shadow -> Fmt.str "%a     " pp_seconds v
    | `S, `No -> Fmt.str "%a" pp_seconds v
    | `P, `Yes -> Fmt.str "%.0f%%  %a" (v *. 100.) Utils.pp_percent ratio
    | `P, `Shadow -> Fmt.str "%.0f%%     " (v *. 100.)
    | `P, `No -> Fmt.str "%.0f%%" (v *. 100.))
    |> Pb.text
    |> Pb.align ~h:`Right ~v:`Top
  in
  let rows =
    List.mapi
      (fun row_idx (summary_name, curve) ->
        let a = Pb.text (if row_idx = 0 then floor_name else "") in
        let b = if only_one_summary then [] else [ Pb.text summary_name ] in
        let c = List.mapi (box_of_scalar row_idx) (List.combine curve0 curve) in
        a :: b @ c)
      names_and_curves
  in
  rows

let unsafe_pp sample_count ppf summary_names (summaries : Summary.t list) =
  let block_count =
    let l = List.map (fun s -> s.block_count) summaries in
    let v = List.hd l in
    if List.exists (fun v' -> v' <> v) l then
      failwith "Can't pp together summaries with a different `block_count`";
    v
  in
  let moving_average_half_life_ratio =
    let l = List.map (fun s -> s.moving_average_half_life_ratio) summaries in
    let v = List.hd l in
    if List.exists (fun v' -> v' <> v) l then
      failwith
        "Can't pp together summaries with a different \
         `moving_average_half_life_ratio`";
    v
  in
  let tbl0 =
    box_of_summaries_config summary_names summaries
    |> Pb.matrix_with_column_spacers
    |> Pb.grid_l ~bars:false
    |> PrintBox_text.to_string
  in
  let header_rows =
    let only_one_summary = List.length summaries = 1 in
    let s = List.hd summaries in
    let played_count_curve =
      List.init Conf.curves_sample_count (fun i ->
          float_of_int i
          /. float_of_int (Conf.curves_sample_count - 1)
          *. float_of_int s.block_count)
    in
    let played_count_curve =
      Utils.Resample.resample_vector `Next_neighbor played_count_curve
        sample_count
      |> Array.of_list
    in
    let header_cells_per_col_idx col_idx =
      let played_count = played_count_curve.(col_idx) in
      let progress_blocks = played_count /. float_of_int s.block_count in
      let h0 =
        if progress_blocks = 0. then "0 (before)"
        else if progress_blocks = 1. then
          Printf.sprintf "%.0f (end)" played_count
        else if Float.is_integer played_count then
          Printf.sprintf "%.0f" played_count
        else Printf.sprintf "%.1f" played_count
      in
      let h1 = Printf.sprintf "%.0f%%" (progress_blocks *. 100.) in
      [ h0; h1 ]
    in
    let col_a =
      [ [ "Block played count *C"; "Blocks progress *C" ] ] |> Pb.matrix_to_text
    in
    let col_b =
      (if only_one_summary then [] else [ [ ""; "" ] ])
      |> Pb.matrix_to_text
      |> Pb.align_matrix `Center
    in
    let cols_c =
      List.init sample_count header_cells_per_col_idx
      |> Pb.matrix_to_text
      |> Pb.align_matrix `Center
    in
    col_a @ col_b @ cols_c |> Pb.transpose_matrix
  in
  let spacer_rows =
    [ List.map (fun _ -> "") (List.hd header_rows) ] |> Pb.matrix_to_text
  in
  let body_rows =
    floors_of_summaries summary_names summaries
    |> List.map (resample_curves_of_floor sample_count)
    |> List.map matrix_of_floor
    |> List.concat
  in
  let tbl1 =
    header_rows @ spacer_rows @ body_rows
    |> Pb.matrix_with_column_spacers
    |> Pb.grid_l ~bars:false
    |> PrintBox_text.to_string
  in
  Format.fprintf ppf
    "%s\n\n\
     %s\n\n\
     Types of curves:\n\
    \  *C: Cumulative. No smoothing.\n\
    \  *LA: Local Average. Smoothed using a weighted sum of the value in the\n\
    \       block and the exponentially decayed values of the previous blocks.\n\
    \       Every %.2f blocks, half of the past is forgotten.\n\
    \  *S: Size. E.g. directory entries, file bytes. No smoothing." tbl0 tbl1
    (moving_average_half_life_ratio *. float_of_int (block_count + 1))

let pp sample_count ppf (summary_names, summaries) =
  if List.length summaries = 0 then ()
  else unsafe_pp sample_count ppf summary_names summaries
