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

let root = Filename.concat "_build" "test-corrupted"

module Conf = Irmin_tezos.Conf

module Store = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)
end

let config ?(readonly = false) ?(fresh = true) root =
  Irmin_pack.config ~readonly ?index_log_size ~fresh root

let info () = Store.Info.empty

let read_file path =
  let ch = open_in_bin path in
  Fun.protect
    (fun () ->
      let len = in_channel_length ch in
      really_input_string ch len)
    ~finally:(fun () -> close_in ch)

let write_file path contents =
  let ch = open_out_bin path in
  Fun.protect
    (fun () -> output_string ch contents)
    ~finally:(fun () ->
      flush ch;
      close_out ch)

let test_corrupted_control_file () =
  rm_dir root;
  let control_file_path = Filename.concat root "store.control" in
  let* repo = Store.Repo.v (config ~fresh:true root) in
  let control_file_blob0 = read_file control_file_path in
  let* store = Store.main repo in
  let* () = Store.set_exn ~info store [ "a" ] "b" in
  let* () = Store.Repo.close repo in
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
  let* error =
    Lwt.catch
      (fun () ->
        let+ r = Store.Repo.v (config ~fresh:false root) in
        Ok r)
      (fun exn -> Lwt.return (Error exn))
  in
  (match error with
  | Error (Irmin_pack_unix.Errors.Pack_error (`Corrupted_control_file s)) ->
      Alcotest.(check string)
        "path is corrupted" s "_build/test-corrupted/store.control"
  | _ -> Alcotest.fail "unexpected error");
  Lwt.return_unit

let tests =
  [
    Alcotest_lwt.test_case "Corrupted control file" `Quick (fun _switch ->
        test_corrupted_control_file);
  ]
