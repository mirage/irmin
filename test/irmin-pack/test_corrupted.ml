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

(* The size of the header of the branch store, before the first entry *)
let branches_header_size = 16

(* The length of the key of a branch comes from the file: a negative or too
   long one is a decoding error. *)
let test_corrupted_branches ~fs () =
  let setup store = Store.set_exn ~info store [ "a" ] "b" in
  let check = check_decoding_error ~fs ~file:"store.branches" ~setup in
  check (replace_at branches_header_size "\xff\xff\xff\xff");
  check (replace_at branches_header_size "\x7f\xff\xff\xff")

module Branch_store =
  Irmin_pack_unix.Atomic_write.Make_persistent
    (Irmin_pack_unix.Io.Unix)
    (Irmin.Branch.String)
    (Irmin_pack.Atomic_write.Value.Of_hash (Irmin.Hash.SHA1))

(* A truncated entry at the end of the branch store may be in the middle of
   its write: a reader reads it once it is complete. A writer cannot append
   after it. *)
let test_truncated_branches ~fs () =
  Eio.Switch.run @@ fun sw ->
  let root = root fs in
  rm_dir root;
  Eio.Path.mkdir ~perm:0o755 root;
  let path = Eio.Path.(root / "store.branches") in
  let hash = Irmin.Hash.SHA1.hash (fun f -> f "v") in
  let hash_t =
    Alcotest.testable
      (Irmin.Type.pp Irmin.Hash.SHA1.t)
      Irmin.Type.(unstage (equal Irmin.Hash.SHA1.t))
  in
  let rw = Branch_store.v ~sw ~fresh:true path in
  Branch_store.set rw "a" hash;
  Branch_store.set rw "b" hash;
  Branch_store.close rw;
  let contents = read_file path in
  let truncated = String.sub contents 0 (String.length contents - 3) in
  write_file path truncated;
  let ro = Branch_store.v ~sw ~readonly:true path in
  Alcotest.(check (option hash_t))
    "complete entry" (Some hash) (Branch_store.find ro "a");
  Alcotest.(check (option hash_t))
    "truncated entry" None (Branch_store.find ro "b");
  write_file path contents;
  Alcotest.(check (option hash_t))
    "completed entry" (Some hash) (Branch_store.find ro "b");
  Branch_store.close ro;
  write_file path truncated;
  match Branch_store.v ~sw path with
  | rw ->
      Branch_store.close rw;
      Alcotest.fail "truncated entry accepted by a writer"
  | exception Irmin_pack_unix.Errors.Pack_error `Decoding_error -> ()

(* A reader which meets a corrupted entry fails at each read, with the same
   error. The entries read before it are not added again: once the file is
   repaired, each branch is listed once. *)
let test_corrupted_branches_reload ~fs () =
  Eio.Switch.run @@ fun sw ->
  let root = root fs in
  rm_dir root;
  Eio.Path.mkdir ~perm:0o755 root;
  let path = Eio.Path.(root / "store.branches") in
  let hash = Irmin.Hash.SHA1.hash (fun f -> f "v") in
  let rw = Branch_store.v ~sw ~fresh:true path in
  Branch_store.set rw "a" hash;
  let only_a = read_file path in
  Branch_store.set rw "b" hash;
  Branch_store.set rw "c" hash;
  Branch_store.close rw;
  let contents = read_file path in
  (* All the entries have the same size *)
  let entry_size = String.length only_a - branches_header_size in
  let corrupted =
    replace_at
      (branches_header_size + (2 * entry_size))
      "\xff\xff\xff\xff" contents
  in
  write_file path only_a;
  let ro = Branch_store.v ~sw ~readonly:true path in
  write_file path corrupted;
  let check_read () =
    match Branch_store.find ro "c" with
    | _ -> Alcotest.fail "corrupted entry accepted"
    | exception Irmin_pack_unix.Errors.Pack_error `Decoding_error -> ()
  in
  check_read ();
  check_read ();
  write_file path contents;
  Alcotest.(check bool)
    "repaired entry" true
    (Option.is_some (Branch_store.find ro "c"));
  Alcotest.(check (list string))
    "branches" [ "a"; "b"; "c" ]
    (List.sort String.compare (Branch_store.list ro));
  Branch_store.close ro

let tests ~fs =
  [
    Alcotest.test_case "Corrupted control file" `Quick
      (test_corrupted_control_file ~fs);
    Alcotest.test_case "Corrupted branches" `Quick (test_corrupted_branches ~fs);
    Alcotest.test_case "Truncated branches" `Quick (test_truncated_branches ~fs);
    Alcotest.test_case "Corrupted branches, reload" `Quick
      (test_corrupted_branches_reload ~fs);
  ]
