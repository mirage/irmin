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

open! Import
module Int63 = Optint.Int63
module Io = Irmin_pack_unix.Io.Unix
module Errs = Irmin_pack_unix.Io_errors.Make (Io)
module Sparse_file = Irmin_pack_unix.Sparse_file.Make (Io)

let test_dir = Filename.concat "_build" "test-pack-mapping"

let rec make_string_seq len () =
  if len <= 0 then Seq.Nil
  else
    let quantity = min 8 len in
    Seq.Cons (String.make quantity 'X', make_string_seq (len - quantity))

(** Call the [Mapping_file] routines to process [pairs] *)
let process_on_disk pairs =
  let mapping = Irmin_pack.Layout.V5.mapping ~root:test_dir ~generation:1 in
  Io.unlink mapping |> ignore;
  let data = Irmin_pack.Layout.V5.prefix ~root:test_dir ~generation:1 in
  Io.unlink data |> ignore;
  let sparse = Sparse_file.Ao.create ~mapping ~data |> Errs.raise_if_error in
  List.iter
    (fun (off, len) ->
      Format.printf "%i (+%i) => %i@." off len (off + len);
      let str = make_string_seq len in
      let off = Int63.of_int off in
      Sparse_file.Ao.append_seq_exn sparse ~off str)
    (List.rev pairs);
  let mapping_size = Int63.to_int (Sparse_file.Ao.mapping_size sparse) in
  Sparse_file.Ao.flush sparse |> Errs.raise_if_error;
  Sparse_file.Ao.close sparse |> Errs.raise_if_error;
  let sparse =
    Sparse_file.open_ro ~mapping_size ~mapping ~data |> Errs.raise_if_error
  in
  let l = ref [] in
  let f ~off ~len = l := (Int63.to_int off, len) :: !l in
  Sparse_file.iter sparse f |> Errs.raise_if_error;
  Sparse_file.close sparse |> Errs.raise_if_error;
  !l |> List.rev

(** Emulate the behaviour of the [Mapping_file] routines to process [pairs] *)
let process_in_mem pairs = List.rev pairs

let test input_entries =
  let output_entries = process_on_disk input_entries in
  let input_entries' = process_in_mem input_entries in
  Alcotest.(check (list (pair int int)))
    "Comparison between Mapping_file result and the in-memory equivalent"
    input_entries' output_entries

(** Produce an array of contiguous offset/length pairs starting from offset 0 *)
let produce_suffix_segmentation len seed =
  let rng = Random.State.make [| seed |] in
  let _, elts =
    List.init len Fun.id
    |> List.fold_left
         (fun (totlen, l) _ ->
           let len = Random.State.int rng 10 + 1 in
           (totlen + len, (totlen, len) :: l))
         (0, [])
  in
  List.to_seq elts |> Array.of_seq

(** Randomly produce a subset of the [full_seg] segmentation. *)
let produce_suffix_segmentation_subset full_seg ~seed =
  let rng = Random.State.make [| seed |] in
  List.filter_map (fun (off, len) ->
      if Random.State.bool rng then None
      else
        let len = Random.State.int rng len |> max 1 in
        Some (off, len))
  @@ Array.to_list full_seg

let test ~full_seg_length ~random_test_count =
  (* [mkdir] may fail if the directory exists. The files in it will be
     overwritten at computation time. *)
  Io.mkdir test_dir |> ignore;

  let seg = produce_suffix_segmentation full_seg_length 42 in
  let rec aux i =
    if i >= random_test_count then ()
    else
      let subset = produce_suffix_segmentation_subset seg ~seed:i in
      if subset <> [] then test subset;
      aux (i + 1)
  in
  aux 0;
  Lwt.return_unit

let tests =
  [
    Alcotest_lwt.test_case "test mapping on small inputs" `Quick
      (fun _switch () -> test ~full_seg_length:10 ~random_test_count:1000);
    Alcotest_lwt.test_case "test mapping on large inputs" `Quick
      (fun _switch () -> test ~full_seg_length:10000 ~random_test_count:100);
  ]
