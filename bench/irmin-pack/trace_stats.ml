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

(** [trace_stats.exe].

    This file is NOT meant to be used from Tezos, as opposed to some other
    "trace_*" files. *)

open Irmin_traces
module Def = Trace_definitions.Stat_trace
module Summary = Trace_stat_summary

let is_trace_magic s = Trace_common.Magic.to_string Def.magic = s

let summarise path =
  Summary.(summarise path |> Fmt.pr "%a\n" (Irmin.Type.pp_json t))

let class_of_path p =
  let path = Eio.Path.native_exn p in
  let chan = open_in_bin path in
  if in_channel_length chan < 8 then
    Fmt.invalid_arg "File \"%s\" should be a stat trace or a json." path;
  let magic = really_input_string chan 8 in
  close_in chan;
  if is_trace_magic magic then
    let block_count =
      Def.open_reader p
      |> snd
      |> Seq.fold_left
           (fun count op -> match op with `Commit _ -> count + 1 | _ -> count)
           0
    in
    `Trace block_count
  else
    let chan = open_in_bin path in
    let raw = really_input_string chan (in_channel_length chan) in
    close_in chan;
    match Irmin.Type.of_json_string Summary.t raw with
    | Error (`Msg msg) ->
        Fmt.invalid_arg
          "File \"%s\" should be a stat trace or a json.\nError: %s" path msg
    | Ok s -> `Summary s

let pp name_per_path paths cols_opt =
  let class_per_path = List.map class_of_path paths in
  let block_count =
    (* When pretty printing, all the summaries will have to have the same number of
       blocks, i.e. the smallest of all inputs. *)
    let trace_lengths =
      List.filter_map
        (function `Trace v -> Some v | `Summary _ -> None)
        class_per_path
    in
    let summary_lengths =
      List.filter_map
        (function `Trace _ -> None | `Summary s -> Some s.Summary.block_count)
        class_per_path
    in
    let s = List.length summary_lengths > 0 in
    let t = List.length trace_lengths > 0 in
    let min_trace_length = List.fold_left min max_int trace_lengths in
    let min_summary_length = List.fold_left min max_int summary_lengths in
    let max_summary_length = List.fold_left max 0 summary_lengths in
    if s && min_summary_length <> max_summary_length then
      invalid_arg
        "Can't pretty print 2 summaries with a different number of blocks.";
    if s && t && min_summary_length > min_trace_length then
      invalid_arg
        "Can't pretty print a trace alongside a summary that has a higher \
         block count.";
    min min_trace_length min_summary_length
  in
  let summaries =
    List.map2
      (fun path -> function
        | `Summary s -> s | `Trace _ -> Summary.summarise ~block_count path)
      paths class_per_path
  in
  let col_count =
    match cols_opt with
    | Some v -> v
    | None -> if List.length summaries > 1 then 4 else 5
  in
  Fmt.pr "%a\n" (Trace_stat_summary_pp.pp col_count) (name_per_path, summaries)

let pp paths named_paths cols_opt =
  let name_per_path, paths =
    List.mapi (fun i v -> (string_of_int i, v)) paths @ named_paths
    |> List.split
  in
  if List.length paths = 0 then
    invalid_arg "trace_stats.exe pp: At least one path should be provided";
  pp name_per_path paths cols_opt

let summary_to_cb path =
  let chan = open_in_bin path in
  let raw = really_input_string chan (in_channel_length chan) in
  close_in chan;
  let s =
    match Irmin.Type.of_json_string Summary.t raw with
    | Error (`Msg msg) ->
        Fmt.invalid_arg "File \"%s\" should be a summary json.\nError: %s" path
          msg
    | Ok s -> s
  in
  Trace_stat_summary_cb.(of_summary s |> Fmt.pr "%a\n" (Irmin.Type.pp_json t))

open Cmdliner

let eio_path fs =
  let parse s = Ok Eio.Path.(fs / s) in
  let print = Eio.Path.pp in
  Arg.conv ~docv:"PATH" (parse, print)

let term_summarise fs =
  let stat_trace_file =
    let doc = Arg.info ~docv:"PATH" ~doc:"A stat trace file" [] in
    Arg.(required @@ pos 0 (some (eio_path fs)) None doc)
  in
  Term.(const summarise $ stat_trace_file)

let eio_file fs =
  let parse s =
    let path = Eio.Path.(fs / s) in
    match Eio.Path.kind ~follow:true path with
    | `Regular_file -> Ok path
    | `Not_found -> Error (`Msg (Format.sprintf "no file %s" s))
    | _ -> Error (`Msg (Format.sprintf "%s is a directory" s))
  in
  let print = Eio.Path.pp in
  Arg.conv ~docv:"PATH" (parse, print)

let term_pp fs =
  let arg_indexed_files =
    let open Arg in
    let a = pos_all (eio_file fs) [] (info [] ~docv:"FILE") in
    value a
  in
  let arg_named_files =
    let open Arg in
    let a =
      opt_all
        (pair string (eio_file fs))
        []
        (info [ "f"; "named-file" ]
           ~doc:
             "A comma-separated pair of short name / path to trace or summary. \
              The short name is used to tag the rows inside the pretty printed \
              table.")
    in
    value a
  in
  let arg_columns =
    let open Arg in
    let doc =
      Arg.info ~doc:"Number of sample columns to show." [ "c"; "columns" ]
    in
    let a = opt (some int) None doc in
    value a
  in
  Term.(const pp $ arg_indexed_files $ arg_named_files $ arg_columns)

let term_cb =
  let summary_file =
    let doc = Arg.info ~docv:"PATH" ~doc:"A stat trace summary file" [] in
    Arg.(required @@ pos 0 (some string) None doc)
  in
  Term.(const summary_to_cb $ summary_file)

let () =
  let man = [] in
  let i =
    Cmd.info ~man ~doc:"Processing of stat traces and stat trace summaries."
      "trace_stats"
  in

  let man =
    [
      `P "From stat trace (repr) to summary (json).";
      `S "EXAMPLE";
      `P "trace_stats.exe summarise run0.repr > run0.json";
    ]
  in
  let j = Cmd.info ~man ~doc:"Stat Trace to Summary" "summarise" in

  let man =
    [
      `P
        "Accepts both stat traces (repr) and summaries (json). The file type \
         is automatically infered.";
      `P
        "When a single file is provided, a subset of the summary of that file \
         is computed and shown.";
      `P
        "When multiple files are provided, a subset of the summary of each \
         file is computed and shown in a way that makes comparisons between \
         files easy.";
      `S "EXAMPLES";
      `P "trace_stats.exe pp run0.json";
      `Noblank;
      `P "trace_stats.exe pp run1.repr";
      `Noblank;
      `P "trace_stats.exe pp run0.json run1.repr run3.json";
      `Noblank;
      `P "trace_stats.exe pp -f r0,run0.json -f r1,run1.repr";
    ]
  in
  let k = Cmd.info ~man ~doc:"Comparative Pretty Printing" "pp" in
  let l = Cmd.info ~man ~doc:"Summary JSON to Continous Benchmarks JSON" "cb" in
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Stdlib.exit
  @@ Cmd.eval
  @@ Cmd.group ~default:(term_summarise fs) i
       [ Cmd.v j (term_summarise fs); Cmd.v k (term_pp fs); Cmd.v l term_cb ]
