(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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
open Common

let root fs = Eio.Path.(fs / "_build" / "test-corrupted")

module Conf = Irmin_tezos.Conf

module Store = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)
end

let config ~sw ~fs ?(readonly = false) ?(fresh = true) root =
  Irmin_pack.config ~sw ~fs ~readonly ?index_log_size ~fresh root

let info () = Store.Info.empty

let read_file path =
  let path = Eio.Path.native_exn path in
  let ch = open_in_bin path in
  Fun.protect
    (fun () ->
      let len = in_channel_length ch in
      really_input_string ch len)
    ~finally:(fun () -> close_in ch)

let write_file path contents =
  let path = Eio.Path.native_exn path in
  let ch = open_out_bin path in
  Fun.protect
    (fun () -> output_string ch contents)
    ~finally:(fun () ->
      flush ch;
      close_out ch)

let test_corrupted_control_file ~fs () =
  Eio.Switch.run @@ fun sw ->
  let root = root fs in
  rm_dir root;
  let control_file_path = Eio.Path.(root / "store.control") in
  let repo = Store.Repo.v (config ~sw ~fs ~fresh:true root) in
  let control_file_blob0 = read_file control_file_path in
  let store = Store.main repo in
  let () = Store.set_exn ~info store [ "a" ] "b" in
  let () = Store.Repo.close repo in
  let control_file_blob1 = read_file control_file_path in
  assert (not (String.equal control_file_blob0 control_file_blob1));
  assert (String.length control_file_blob0 = String.length control_file_blob1);
  let split_write_at = 3 * String.length control_file_blob1 / 4 in
  let control_file_mix =
    String.sub control_file_blob1 0 split_write_at
    ^ String.sub control_file_blob0 split_write_at
        (String.length control_file_blob0 - split_write_at)
  in
  assert (String.length control_file_mix = String.length control_file_blob1);
  assert (not (String.equal control_file_blob0 control_file_mix));
  assert (not (String.equal control_file_blob1 control_file_mix));
  write_file control_file_path control_file_mix;
  let error =
    try Ok (Store.Repo.v (config ~sw ~fs ~fresh:false root) : Store.Repo.t)
    with exn -> Error exn
  in
  match error with
  | Error (Irmin_pack_unix.Errors.Pack_error (`Corrupted_control_file s)) ->
      Alcotest.(check string)
        "path is corrupted" s "./_build/test-corrupted/store.control"
  | _ -> Alcotest.fail "unexpected error"

(* [Store.Repo.v] fails with [Decoding_error] when an entry of [file] is
   replaced by [corrupt]. [file] holds one entry after [setup]. *)
let check_decoding_error ~fs ~file ~setup corrupt =
  Eio.Switch.run @@ fun sw ->
  let root = root fs in
  rm_dir root;
  let path = Eio.Path.(root / file) in
  let repo = Store.Repo.v (config ~sw ~fs ~fresh:true root) in
  setup (Store.main repo);
  Store.Repo.close repo;
  write_file path (corrupt (read_file path));
  match Store.Repo.v (config ~sw ~fs ~fresh:false root) with
  | repo ->
      Store.Repo.close repo;
      Alcotest.failf "corrupted %s accepted" file
  | exception Irmin_pack_unix.Errors.Pack_error `Decoding_error -> ()

(* Replace the 4 bytes at [off] of a string by [bytes]. *)
let replace_at off bytes s =
  String.sub s 0 off ^ bytes ^ String.sub s (off + 4) (String.length s - off - 4)

(* The lengths of the entries of the dictionary come from the file: a
   negative or too long one is a decoding error. *)
let test_corrupted_dict ~fs () =
  (* Steps longer than 3 bytes are stored in the dictionary *)
  let setup store = Store.set_exn ~info store [ "abcdef" ] "b" in
  let check = check_decoding_error ~fs ~file:"store.dict" ~setup in
  check (replace_at 0 "\xff\xff\xff\xff");
  check (replace_at 0 "\x7f\xff\xff\xff")

module Dict = Irmin_pack_unix.Dict.Make (Irmin_pack_unix.Io.Unix)

let check_dict_error msg expected r =
  match (r, expected) with
  | Error `Decoding_error, `Decoding_error
  | Error `Inconsistent_store, `Inconsistent_store ->
      ()
  | Error _, _ -> Alcotest.failf "%s: unexpected error" msg
  | Ok (), _ -> Alcotest.failf "%s: accepted" msg

(* A read-only dictionary checks what it reads when it is reloaded. The
   entries read before an error are not read again. *)
let test_corrupted_dict_reload ~fs () =
  Eio.Switch.run @@ fun sw ->
  let root = root fs in
  rm_dir root;
  Eio.Path.mkdir ~perm:0o755 root;
  let path = Eio.Path.(root / "store.dict") in
  let rw = Dict.create_rw ~sw ~overwrite:true ~path |> Result.get_ok in
  let ids = List.filter_map (Dict.index rw) [ "a"; "b"; "c" ] in
  Alcotest.(check (list int)) "ids" [ 0; 1; 2 ] ids;
  Dict.flush rw |> Result.get_ok;
  (* Each entry has a length of 4 bytes, then a string of 1 byte *)
  let entry_size = 5 in
  let end_poff i = Int63.of_int (i * entry_size) in
  let ro =
    Dict.open_ro ~sw ~size:(end_poff 1) ~dead_header_size:0 path
    |> Result.get_ok
  in
  let contents = read_file path in
  write_file path (replace_at (2 * entry_size) "\xff\xff\xff\xff" contents);
  check_dict_error "corrupted entry" `Decoding_error
    (Dict.refresh_end_poff ro (end_poff 3));
  Alcotest.(check (option string)) "entry before" (Some "b") (Dict.find ro 1);
  write_file path contents;
  Dict.refresh_end_poff ro (end_poff 3) |> Result.get_ok;
  Alcotest.(check (list (option string)))
    "entries"
    [ Some "a"; Some "b"; Some "c"; None ]
    (List.map (Dict.find ro) [ 0; 1; 2; 3 ]);
  (* The end offset comes from the control file *)
  check_dict_error "end after the file" `Inconsistent_store
    (Dict.refresh_end_poff ro (end_poff 4));
  check_dict_error "end before the last one" `Inconsistent_store
    (Dict.refresh_end_poff ro (end_poff 2));
  Dict.close ro |> Result.get_ok;
  Dict.close rw |> Result.get_ok

let tests ~fs =
  [
    Alcotest.test_case "Corrupted control file" `Quick
      (test_corrupted_control_file ~fs);
    Alcotest.test_case "Corrupted dictionary" `Quick (test_corrupted_dict ~fs);
    Alcotest.test_case "Corrupted dictionary, reload" `Quick
      (test_corrupted_dict_reload ~fs);
  ]
