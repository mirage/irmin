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

let test_dir ~fs = Eio.Path.(fs / "_build" / "test-pack-mapping")

let rec make_string_seq len () =
  if len <= 0 then Seq.Nil
  else
    let quantity = min 8 len in
    Seq.Cons (String.make quantity 'X', make_string_seq (len - quantity))

(** Call the [Mapping_file] routines to process [pairs] *)
let process_on_disk ~fs pairs =
  Eio.Switch.run @@ fun sw ->
  let mapping =
    Irmin_pack.Layout.V5.mapping ~root:(test_dir ~fs) ~generation:1
  in
  Io.unlink mapping |> ignore;
  let data = Irmin_pack.Layout.V5.prefix ~root:(test_dir ~fs) ~generation:1 in
  Io.unlink data |> ignore;
  let sparse =
    Sparse_file.Ao.create ~sw ~mapping ~data |> Errs.raise_if_error
  in
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
    Sparse_file.open_ro ~sw ~mapping_size ~mapping ~data |> Errs.raise_if_error
  in
  let l = ref [] in
  let f ~off ~len = l := (Int63.to_int off, len) :: !l in
  Sparse_file.iter sparse f |> Errs.raise_if_error;
  Sparse_file.close sparse |> Errs.raise_if_error;
  !l |> List.rev

(** Emulate the behaviour of the [Mapping_file] routines to process [pairs] *)
let process_in_mem pairs = List.rev pairs

let test ~fs input_entries =
  let output_entries = process_on_disk ~fs input_entries in
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

let test ~fs ~full_seg_length ~random_test_count =
  (* [mkdir] may fail if the directory exists. The files in it will be
     overwritten at computation time. *)
  Io.mkdir (test_dir ~fs) |> ignore;

  let seg = produce_suffix_segmentation full_seg_length 42 in
  let rec aux i =
    if i >= random_test_count then ()
    else
      let subset = produce_suffix_segmentation_subset seg ~seed:i in
      if subset <> [] then test ~fs subset;
      aux (i + 1)
  in
  aux 0

(** Write a mapping file with [entries] [(off, poff, len)], and a data file. *)
let write_mapping ~sw ~fs entries =
  let mapping =
    Irmin_pack.Layout.V5.mapping ~root:(test_dir ~fs) ~generation:1
  in
  let data = Irmin_pack.Layout.V5.prefix ~root:(test_dir ~fs) ~generation:1 in
  let write path contents =
    Io.unlink path |> ignore;
    let io = Io.create ~sw ~path ~overwrite:true |> Errs.raise_if_error in
    Io.write_string io ~off:Int63.zero contents |> Errs.raise_if_error;
    Io.close io |> Errs.raise_if_error
  in
  let buf = Buffer.create 64 in
  List.iter
    (fun (off, poff, len) ->
      List.iter
        (fun x -> Buffer.add_int64_le buf (Int64.of_int x))
        [ off; poff; len ])
    entries;
  write mapping (Buffer.contents buf);
  write data (String.make 100 'x');
  (mapping, data, Buffer.length buf)

let check_corrupted msg = function
  | Error (`Corrupted_mapping_file _) -> ()
  | Error e -> Errs.raise_error e
  | Ok _ -> Alcotest.failf "%s: the corruption is not detected" msg

(** The size of the mapping comes from the control file: it is checked before
    anything is allocated. *)
let test_mapping_size ~fs () =
  Eio.Switch.run @@ fun sw ->
  Io.mkdir (test_dir ~fs) |> ignore;
  let mapping, data, size = write_mapping ~sw ~fs [ (0, 0, 10) ] in
  let open_ro mapping_size =
    Sparse_file.open_ro ~sw ~mapping_size ~mapping ~data
  in
  check_corrupted "larger" (open_ro (size + (24 * 1000)));
  check_corrupted "next multiple of 24" (open_ro (size + 1));
  check_corrupted "negative" (open_ro (-24));
  let sparse = open_ro size |> Errs.raise_if_error in
  Sparse_file.close sparse |> Errs.raise_if_error

(** The entries found in the mapping are checked. *)
let test_mapping_entries ~fs () =
  Eio.Switch.run @@ fun sw ->
  Io.mkdir (test_dir ~fs) |> ignore;
  let check ?(off = 12) msg entries =
    let mapping, data, mapping_size = write_mapping ~sw ~fs entries in
    let sparse =
      Sparse_file.open_ro ~sw ~mapping_size ~mapping ~data
      |> Errs.raise_if_error
    in
    let buf = Bytes.create 1 in
    let r =
      Errs.catch (fun () ->
          Sparse_file.read_exn sparse ~off:(Int63.of_int off) ~len:1 buf)
    in
    Sparse_file.close sparse |> Errs.raise_if_error;
    check_corrupted msg r
  in
  check "overlapping entries" [ (0, 0, 10); (5, 10, 10) ];
  check ~off:11 "empty entry" [ (0, 0, 10); (12, 10, 0); (20, 10, 10) ];
  check "negative poff" [ (10, -10, 10) ];
  (* [5 + max_int - 1] overflows *)
  check "huge previous length" [ (5, 0, max_int); (10, 10, 10) ]

let tests ~fs =
  [
    Alcotest.test_case "test mapping on small inputs" `Quick (fun () ->
        test ~fs ~full_seg_length:10 ~random_test_count:1000);
    Alcotest.test_case "test mapping on large inputs" `Quick (fun () ->
        test ~fs ~full_seg_length:10000 ~random_test_count:100);
    Alcotest.test_case "test mapping size" `Quick (test_mapping_size ~fs);
    Alcotest.test_case "test mapping entries" `Quick (test_mapping_entries ~fs);
  ]
