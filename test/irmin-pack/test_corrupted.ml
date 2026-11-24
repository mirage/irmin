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

(* [Store.Repo.v] fails with [Decoding_error] when an entry of [file] is
   replaced by [corrupt]. [file] holds one entry after [setup]. *)
let check_decoding_error ~file ~setup corrupt =
  rm_dir root;
  let path = Filename.concat root file in
  let* repo = Store.Repo.v (config ~fresh:true root) in
  let* store = Store.main repo in
  let* () = setup store in
  let* () = Store.Repo.close repo in
  write_file path (corrupt (read_file path));
  Lwt.catch
    (fun () ->
      let* repo = Store.Repo.v (config ~fresh:false root) in
      let+ () = Store.Repo.close repo in
      Alcotest.failf "corrupted %s accepted" file)
    (function
      | Irmin_pack_unix.Errors.Pack_error `Decoding_error -> Lwt.return_unit
      | exn -> Lwt.fail exn)

(* Replace the 4 bytes at [off] of a string by [bytes]. *)
let replace_at off bytes s =
  String.sub s 0 off ^ bytes ^ String.sub s (off + 4) (String.length s - off - 4)

(* The size of the header of the branch store, before the first entry *)
let branches_header_size = 16

(* The length of the key of a branch comes from the file: a negative or too
   long one is a decoding error. *)
let test_corrupted_branches () =
  let setup store = Store.set_exn ~info store [ "a" ] "b" in
  let check = check_decoding_error ~file:"store.branches" ~setup in
  let* () = check (replace_at branches_header_size "\xff\xff\xff\xff") in
  check (replace_at branches_header_size "\x7f\xff\xff\xff")

module Branch_store =
  Irmin_pack_unix.Atomic_write.Make_persistent
    (Irmin_pack_unix.Io.Unix)
    (Irmin.Branch.String)
    (Irmin_pack.Atomic_write.Value.Of_hash (Irmin.Hash.SHA1))

(* A truncated entry at the end of the branch store may be in the middle of
   its write: a reader reads it once it is complete. A writer cannot append
   after it. *)
let test_truncated_branches () =
  rm_dir root;
  Unix.mkdir root 0o755;
  let path = Filename.concat root "store.branches" in
  let hash = Irmin.Hash.SHA1.hash (fun f -> f "v") in
  let hash_t =
    Alcotest.testable
      (Irmin.Type.pp Irmin.Hash.SHA1.t)
      Irmin.Type.(unstage (equal Irmin.Hash.SHA1.t))
  in
  let* rw = Branch_store.v ~fresh:true path in
  let* () = Branch_store.set rw "a" hash in
  let* () = Branch_store.set rw "b" hash in
  let* () = Branch_store.close rw in
  let contents = read_file path in
  let truncated = String.sub contents 0 (String.length contents - 3) in
  write_file path truncated;
  let* ro = Branch_store.v ~readonly:true path in
  let* a = Branch_store.find ro "a" in
  let* b = Branch_store.find ro "b" in
  Alcotest.(check (option hash_t)) "complete entry" (Some hash) a;
  Alcotest.(check (option hash_t)) "truncated entry" None b;
  write_file path contents;
  let* b = Branch_store.find ro "b" in
  Alcotest.(check (option hash_t)) "completed entry" (Some hash) b;
  let* () = Branch_store.close ro in
  write_file path truncated;
  Lwt.catch
    (fun () ->
      let* rw = Branch_store.v path in
      let+ () = Branch_store.close rw in
      Alcotest.fail "truncated entry accepted by a writer")
    (function
      | Irmin_pack_unix.Errors.Pack_error `Decoding_error -> Lwt.return_unit
      | exn -> Lwt.fail exn)

(* A reader which meets a corrupted entry fails at each read. The entries
   read before it are not added again: once the file is repaired, each
   branch is listed once. *)
let test_corrupted_branches_reload () =
  rm_dir root;
  Unix.mkdir root 0o755;
  let path = Filename.concat root "store.branches" in
  let hash = Irmin.Hash.SHA1.hash (fun f -> f "v") in
  let* rw = Branch_store.v ~fresh:true path in
  let* () = Branch_store.set rw "a" hash in
  let only_a = read_file path in
  let* () = Branch_store.set rw "b" hash in
  let* () = Branch_store.set rw "c" hash in
  let* () = Branch_store.close rw in
  let contents = read_file path in
  (* All the entries have the same size *)
  let entry_size = String.length only_a - branches_header_size in
  let corrupted =
    replace_at
      (branches_header_size + (2 * entry_size))
      "\xff\xff\xff\xff" contents
  in
  write_file path only_a;
  let* ro = Branch_store.v ~readonly:true path in
  write_file path corrupted;
  let check_read () =
    Lwt.catch
      (fun () ->
        let+ _ = Branch_store.find ro "c" in
        Alcotest.fail "corrupted entry accepted")
      (function
        | Irmin_pack_unix.Errors.Pack_error `Decoding_error -> Lwt.return_unit
        | exn -> Lwt.fail exn)
  in
  let* () = check_read () in
  let* () = check_read () in
  write_file path contents;
  let* c = Branch_store.find ro "c" in
  Alcotest.(check bool) "repaired entry" true (Option.is_some c);
  let* branches = Branch_store.list ro in
  Alcotest.(check (list string))
    "branches" [ "a"; "b"; "c" ]
    (List.sort String.compare branches);
  Branch_store.close ro

let tests =
  [
    Alcotest_lwt.test_case "Corrupted control file" `Quick (fun _switch ->
        test_corrupted_control_file);
    Alcotest_lwt.test_case "Corrupted branches" `Quick (fun _switch ->
        test_corrupted_branches);
    Alcotest_lwt.test_case "Truncated branches" `Quick (fun _switch ->
        test_truncated_branches);
    Alcotest_lwt.test_case "Corrupted branches, reload" `Quick (fun _switch ->
        test_corrupted_branches_reload);
  ]
