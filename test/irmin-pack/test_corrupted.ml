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

let direct_key key =
  match Irmin_pack_unix.Pack_key.inspect key with
  | Direct { offset; hash; length; _ } -> (offset, hash, length)
  | Indexed _ -> Alcotest.fail "not a direct key"

(* A store with one commit: the keys of the commit and of its root node. *)
let store_with_commit ~fs root =
  Eio.Switch.run @@ fun sw ->
  rm_dir root;
  let repo = Store.Repo.v (config ~sw ~fs ~fresh:true root) in
  let store = Store.main repo in
  Store.set_exn ~info store [ "a" ] "b";
  let commit = Store.Head.get store in
  let commit_key = Store.Commit.key commit in
  let node_key =
    match Store.Tree.key (Store.Commit.tree commit) with
    | Some (`Node key) -> key
    | Some (`Contents _) | None -> Alcotest.fail "no node key"
  in
  Store.Repo.close repo;
  (commit_key, node_key)

(* Read the commit of [key] in the store. *)
let read_commit ~fs root key =
  Eio.Switch.run @@ fun sw ->
  let repo = Store.Repo.v (config ~sw ~fs ~fresh:false root) in
  Fun.protect
    (fun () -> Store.Commit.of_key repo key)
    ~finally:(fun () -> Store.Repo.close repo)

let check_raises msg expected f =
  match f () with
  | _ -> Alcotest.failf "%s: no exception" msg
  | exception exn ->
      if not (expected exn) then
        Alcotest.failf "%s: unexpected exception %s" msg
          (Printexc.to_string exn)

let corrupted = function
  | Irmin_pack_unix.Pack_store.Corrupted_store _ -> true
  | _ -> false

(* The bytes of an entry are not trusted: a wrong kind or a wrong length is
   reported as a corruption, and no memory is allocated from the length. *)
let test_corrupted_entry ~fs () =
  let root = root fs in
  let commit_key, node_key = store_with_commit ~fs root in
  let offset, hash, length = direct_key commit_key in
  let node_offset, _, node_length = direct_key node_key in
  let key ~offset ~length =
    Irmin_pack_unix.Pack_key.v_direct ~offset ~length hash
  in
  check_raises "negative length" corrupted (fun () ->
      read_commit ~fs root (key ~offset ~length:(-1)));
  check_raises "length after the end"
    (function Irmin_pack_unix.Pack_store.Invalid_read _ -> true | _ -> false)
    (fun () -> read_commit ~fs root (key ~offset ~length:max_int));
  check_raises "entry of another kind" corrupted (fun () ->
      read_commit ~fs root (key ~offset:node_offset ~length:node_length));
  (* The kind of the commit is not a kind any more *)
  let suffix = Eio.Path.(root / "store.0.suffix") in
  let contents = Bytes.of_string (read_file suffix) in
  Bytes.set contents (Int63.to_int offset + Store.Hash.hash_size) 'X';
  write_file suffix (Bytes.to_string contents);
  check_raises "unknown kind" corrupted (fun () ->
      read_commit ~fs root (key ~offset ~length))

let tests ~fs =
  [
    Alcotest.test_case "Corrupted control file" `Quick
      (test_corrupted_control_file ~fs);
    Alcotest.test_case "Corrupted entry" `Quick (test_corrupted_entry ~fs);
  ]
