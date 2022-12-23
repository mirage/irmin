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
open Common

let root_export = Filename.concat "_build" "test-snapshot-export"
let root_import = Filename.concat "_build" "test-snapshot-import"
let src = Logs.Src.create "tests.snapshot" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

module S = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)
end

let check_key = Alcotest.check_repr Key.t

let config ?(readonly = false) ?(fresh = true) ~indexing_strategy root =
  Irmin_pack.config ~readonly ?index_log_size ~fresh ~indexing_strategy root

let info = S.Info.empty

let read_string rbuf ~len =
  let buf, ofs = !rbuf in
  if String.length buf - ofs < len then failwith "small buffer"
  else
    let res = String.sub buf ofs len in
    rbuf := (buf, ofs + len);
    res

let set_int8 buf i =
  let b = Bytes.create 1 in
  Bytes.set_int8 b 0 i;
  Buffer.add_bytes buf b

let get_int8 rbuf =
  let string = read_string ~len:1 rbuf in
  Bytes.get_int8 (Bytes.of_string string) 0

let read_mbytes rbuf b =
  let string = read_string rbuf ~len:(Bytes.length b) in
  Bytes.blit_string string 0 b 0 (Bytes.length b)

let encode_bin_snapshot = Irmin.Type.(unstage (encode_bin S.Snapshot.t))
let decode_bin_snapshot = Irmin.Type.(unstage (decode_bin S.Snapshot.t))

let encode_with_size buf snapshot_inode =
  let size = ref 0 in
  let tmp = Buffer.create 0 in
  encode_bin_snapshot snapshot_inode (fun x ->
      size := !size + String.length x;
      Buffer.add_string tmp x);
  set_int8 buf !size;
  Buffer.add_buffer buf tmp;
  Lwt.return_unit

let decode_with_size rbuf =
  let size = get_int8 rbuf in
  let b = Bytes.create size in
  let () = read_mbytes rbuf b in
  let b = Bytes.to_string b in
  decode_bin_snapshot b (ref 0)

let restore repo ?on_disk buf =
  let on_disk = (on_disk :> [ `Path of string | `Reuse ] option) in
  let snapshot = S.Snapshot.Import.v ?on_disk repo in
  let total = String.length buf in
  let total_visited = ref 0 in
  let rbuf = ref (buf, 0) in
  let rec aux last_key =
    let _, read = !rbuf in
    if read < total then (
      incr total_visited;
      let elt = decode_with_size rbuf in
      let* key = S.Snapshot.Import.save_elt snapshot elt in
      aux (Some key))
    else Lwt.return (!total_visited, last_key)
  in
  let* result = aux None in
  S.Snapshot.Import.close snapshot repo;
  Lwt.return result

let test ~repo_export ~repo_import ?on_disk tree expected_visited =
  let* commit = S.Commit.v repo_export ~parents:[] ~info tree in
  let tree = S.Commit.tree commit in
  let root_key = S.Tree.key tree |> Option.get in
  let buf = Buffer.create 0 in
  let* total_visited =
    S.Snapshot.export ?on_disk repo_export (encode_with_size buf) ~root_key
  in
  Alcotest.(check int)
    "total visited during export" expected_visited total_visited;
  let* total_visited, key =
    Buffer.contents buf |> restore repo_import ?on_disk
  in
  Alcotest.(check int)
    "total visited during import" expected_visited total_visited;
  let () =
    match (root_key, key) with
    | _, None -> Alcotest.fail "No key imported"
    | `Node key, Some key' -> check_key "snapshot key" key key'
    | `Contents _, _ -> Alcotest.fail "Root key should not be contents"
  in
  Lwt.return_unit

let tree2 () =
  let t = S.Tree.singleton [ "a" ] "x" in
  let* t = S.Tree.add t [ "b" ] "y" in
  let* t = S.Tree.add t [ "c" ] "y" in
  S.Tree.add t [ "d" ] "y"

let test_in_memory ~indexing_strategy () =
  rm_dir root_export;
  rm_dir root_import;
  let* repo_export =
    S.Repo.v (config ~readonly:false ~fresh:true ~indexing_strategy root_export)
  in
  let* repo_import =
    S.Repo.v (config ~readonly:false ~fresh:true ~indexing_strategy root_import)
  in
  let test = test ~repo_export ~repo_import in
  let tree1 = S.Tree.singleton [ "a" ] "x" in
  let* () = test tree1 2 in
  let* tree2 = tree2 () in
  let* () = test tree2 3 in
  let* () = S.Repo.close repo_export in
  S.Repo.close repo_import

let test_in_memory_minimal =
  test_in_memory ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal

let test_in_memory_always =
  test_in_memory ~indexing_strategy:Irmin_pack.Indexing_strategy.always

let test_on_disk ~indexing_strategy () =
  rm_dir root_export;
  rm_dir root_import;
  let index_on_disk = Filename.concat root_import "index_on_disk" in
  let* repo_export =
    S.Repo.v (config ~readonly:false ~fresh:true ~indexing_strategy root_export)
  in
  let* repo_import =
    S.Repo.v (config ~readonly:false ~fresh:true ~indexing_strategy root_import)
  in
  let test = test ~repo_export ~repo_import in
  let* tree2 = tree2 () in
  let* () = test ~on_disk:(`Path index_on_disk) tree2 3 in
  let* () = S.Repo.close repo_export in
  S.Repo.close repo_import

let test_on_disk_minimal =
  test_on_disk ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal

let test_on_disk_always =
  test_on_disk ~indexing_strategy:Irmin_pack.Indexing_strategy.always

let start_gc repo commit =
  let commit_key = S.Commit.key commit in
  let* launched = S.Gc.start_exn ~unlink:false repo commit_key in
  assert launched;
  Lwt.return_unit

let finalise_gc repo =
  let* result = S.Gc.finalise_exn ~wait:true repo in
  match result with
  | `Idle | `Running -> Alcotest.fail "expected finalised gc"
  | `Finalised _ -> Lwt.return_unit

let test_gc ~repo_export ~repo_import ?on_disk expected_visited =
  (* create the store *)
  let* tree1 =
    let t = S.Tree.singleton [ "b"; "a" ] "x0" in
    S.Tree.add t [ "a"; "b" ] "x1"
  in
  let* c1 = S.Commit.v repo_export ~parents:[] ~info tree1 in
  let k1 = S.Commit.key c1 in
  let* tree2 = S.Tree.add tree1 [ "a"; "c" ] "x2" in
  let* _ = S.Commit.v repo_export ~parents:[ k1 ] ~info tree2 in
  let* tree3 =
    let* t = S.Tree.remove tree1 [ "a"; "b" ] in
    S.Tree.add t [ "a"; "d" ] "x3"
  in
  let* c3 = S.Commit.v repo_export ~parents:[ k1 ] ~info tree3 in
  (* call gc on last commit *)
  let* () = start_gc repo_export c3 in
  let* () = finalise_gc repo_export in
  let tree = S.Commit.tree c3 in
  let root_key = S.Tree.key tree |> Option.get in
  let buf = Buffer.create 0 in
  let* total_visited =
    S.Snapshot.export ?on_disk repo_export (encode_with_size buf) ~root_key
  in
  Alcotest.(check int)
    "total visited during export" expected_visited total_visited;
  let* total_visited, key =
    Buffer.contents buf |> restore repo_import ?on_disk
  in
  Alcotest.(check int)
    "total visited during import" expected_visited total_visited;
  let () =
    match (root_key, key) with
    | _, None -> Alcotest.fail "No key imported"
    | `Node key, Some key' -> check_key "snapshot key" key key'
    | `Contents _, _ -> Alcotest.fail "Root key should not be contents"
  in
  Lwt.return_unit

let indexing_strategy = Irmin_pack.Indexing_strategy.minimal

let test_gced_store_in_memory () =
  rm_dir root_export;
  rm_dir root_import;
  let* repo_export =
    S.Repo.v (config ~readonly:false ~fresh:true ~indexing_strategy root_export)
  in
  let* repo_import =
    S.Repo.v (config ~readonly:false ~fresh:true ~indexing_strategy root_import)
  in
  let* () = test_gc ~repo_export ~repo_import 5 in
  let* () = S.Repo.close repo_export in
  S.Repo.close repo_import

let test_gced_store_on_disk () =
  rm_dir root_export;
  rm_dir root_import;
  let index_on_disk = Filename.concat root_import "index_on_disk" in
  let* repo_export =
    S.Repo.v (config ~readonly:false ~fresh:true ~indexing_strategy root_export)
  in
  let* repo_import =
    S.Repo.v (config ~readonly:false ~fresh:true ~indexing_strategy root_import)
  in
  let* () =
    test_gc ~repo_export ~repo_import ~on_disk:(`Path index_on_disk) 5
  in
  let* () = S.Repo.close repo_export in
  S.Repo.close repo_import

let test_export_import_reexport () =
  rm_dir root_export;
  rm_dir root_import;
  (* export a snapshot. *)
  let* repo_export =
    S.Repo.v (config ~readonly:false ~fresh:true ~indexing_strategy root_export)
  in
  let tree = S.Tree.singleton [ "a" ] "y" in
  let* parent_commit = S.Commit.v repo_export ~parents:[] ~info tree in
  let parent_key =
    Irmin_pack_unix.Pack_key.v_indexed (S.Commit.hash parent_commit)
  in
  let tree = S.Tree.singleton [ "a" ] "x" in
  let* _ = S.Commit.v repo_export ~parents:[ parent_key ] ~info tree in
  let root_key = S.Tree.key tree |> Option.get in
  let buf = Buffer.create 0 in
  let* _ = S.Snapshot.export repo_export (encode_with_size buf) ~root_key in
  let* () = S.Repo.close repo_export in
  (* buf contains the snapshot, we can rm root_export and import the snapshot in
     a new store, with the key parent of type Indexed. *)
  rm_dir root_export;
  let* repo_import =
    S.Repo.v (config ~readonly:false ~fresh:true ~indexing_strategy root_import)
  in
  let* _, key = Buffer.contents buf |> restore repo_import in
  let key = Option.get key in
  let* tree = S.Tree.of_key repo_import (`Node key) in
  let tree = Option.get tree in
  let* commit = S.Commit.v repo_import ~info ~parents:[ parent_key ] tree in
  let commit_key = S.Commit.key commit in
  let commit_hash = S.Commit.hash commit in
  (* export the gc-based snapshot in a clean root_export. *)
  let* () = S.create_one_commit_store repo_import commit_key root_export in
  let* () = S.Repo.close repo_import in
  (* open the new store and check that everything is readable. *)
  let* repo_export =
    S.Repo.v
      (config ~readonly:false ~fresh:false ~indexing_strategy root_export)
  in
  let* commit = S.Commit.of_hash repo_export commit_hash in
  let commit = Option.get commit in
  let tree = S.Commit.tree commit in
  let* got = S.Tree.find tree [ "a" ] in
  Alcotest.(check (option string)) "find blob" (Some "x") got;
  S.Repo.close repo_export

let tests =
  let tc name f = Alcotest_lwt.test_case name `Quick (fun _switch () -> f ()) in
  [
    tc "in memory minimal" test_in_memory_minimal;
    tc "in memory always" test_in_memory_always;
    tc "on disk minimal" test_on_disk_minimal;
    tc "on disk always" test_on_disk_always;
    tc "gced store, in memory" test_gced_store_in_memory;
    tc "gced store, on disk" test_gced_store_on_disk;
    tc "import old snapshot, export gc based snapshot "
      test_export_import_reexport;
  ]
