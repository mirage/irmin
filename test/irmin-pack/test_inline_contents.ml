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

(** Tests for inline contents feature.

    These tests verify that the data stored is correct regardless of whether
    inlining is enabled or disabled. *)

open! Import
open Common

let root_no_inline ~fs = Eio.Path.(fs / "_build" / "test-inline-no")
let root_with_inline ~fs = Eio.Path.(fs / "_build" / "test-inline-yes")
let root_equiv_no ~fs = Eio.Path.(fs / "_build" / "test-inline-equiv-no")
let root_equiv_yes ~fs = Eio.Path.(fs / "_build" / "test-inline-equiv-yes")
let src = Logs.Src.create "tests.inline_contents" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

module S = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)
end

let config ~sw ~fs ?(readonly = false) ?(fresh = true)
    ?inline_contents_max_bytes ~inline_contents root =
  Irmin_pack.config ~sw ~fs ~readonly ~fresh
    ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal ~inline_contents
    ?inline_contents_max_bytes root

let info = S.Info.empty

(** Test that data can be stored and retrieved correctly with inlining disabled
*)
let test_without_inlining ~fs () =
  let root = root_no_inline ~fs in
  rm_dir root;
  Eio.Switch.run @@ fun sw ->
  let repo =
    S.Repo.v
      (config ~sw ~fs ~readonly:false ~fresh:true ~inline_contents:false root)
  in
  (* Create a tree with small and large contents *)
  let tree = S.Tree.empty () in
  let tree = S.Tree.add tree [ "small" ] "abc" in
  (* Small content, < 16 bytes *)
  let tree = S.Tree.add tree [ "large" ] (String.make 100 'x') in
  (* Large content *)
  (* Create a commit *)
  let commit = S.Commit.v repo ~parents:[] ~info tree in
  let hash = S.Commit.hash commit in
  (* Read back and verify *)
  let commit' = S.Commit.of_hash repo hash |> Option.get in
  let tree' = S.Commit.tree commit' in
  let small = S.Tree.find tree' [ "small" ] in
  let large = S.Tree.find tree' [ "large" ] in
  Alcotest.(check (option string)) "small content" (Some "abc") small;
  Alcotest.(check (option string))
    "large content"
    (Some (String.make 100 'x'))
    large;
  S.Repo.close repo

(** Test that data can be stored and retrieved correctly with inlining enabled
*)
let test_with_inlining ~fs () =
  let root = root_with_inline ~fs in
  rm_dir root;
  Eio.Switch.run @@ fun sw ->
  let repo =
    S.Repo.v
      (config ~sw ~fs ~readonly:false ~fresh:true ~inline_contents:true root)
  in
  (* Create a tree with small and large contents *)
  let tree = S.Tree.empty () in
  let tree = S.Tree.add tree [ "small" ] "abc" in
  (* Small content, < 16 bytes *)
  let tree = S.Tree.add tree [ "large" ] (String.make 100 'x') in
  (* Large content *)
  (* Create a commit *)
  let commit = S.Commit.v repo ~parents:[] ~info tree in
  let hash = S.Commit.hash commit in
  (* Read back and verify *)
  let commit' = S.Commit.of_hash repo hash |> Option.get in
  let tree' = S.Commit.tree commit' in
  let small = S.Tree.find tree' [ "small" ] in
  let large = S.Tree.find tree' [ "large" ] in
  Alcotest.(check (option string)) "small content" (Some "abc") small;
  Alcotest.(check (option string))
    "large content"
    (Some (String.make 100 'x'))
    large;
  S.Repo.close repo

(** Test that the same data produces the same content hash regardless of
    inlining *)
let test_content_equivalence ~fs () =
  let root_no_inline = root_equiv_no ~fs in
  let root_inline = root_equiv_yes ~fs in
  rm_dir root_no_inline;
  rm_dir root_inline;
  Eio.Switch.run @@ fun sw ->
  (* Create store without inlining *)
  let repo1 =
    S.Repo.v
      (config ~sw ~fs ~readonly:false ~fresh:true ~inline_contents:false
         root_no_inline)
  in
  let tree1 = S.Tree.empty () in
  let tree1 = S.Tree.add tree1 [ "a" ] "small" in
  let tree1 = S.Tree.add tree1 [ "b" ] "also_small" in
  let tree1 = S.Tree.add tree1 [ "c"; "d" ] "nested" in
  let commit1 = S.Commit.v repo1 ~parents:[] ~info tree1 in
  S.Repo.close repo1;
  (* Create store with inlining *)
  let repo2 =
    S.Repo.v
      (config ~sw ~fs ~readonly:false ~fresh:true ~inline_contents:true
         root_inline)
  in
  let tree2 = S.Tree.empty () in
  let tree2 = S.Tree.add tree2 [ "a" ] "small" in
  let tree2 = S.Tree.add tree2 [ "b" ] "also_small" in
  let tree2 = S.Tree.add tree2 [ "c"; "d" ] "nested" in
  let commit2 = S.Commit.v repo2 ~parents:[] ~info tree2 in
  S.Repo.close repo2;
  (* Verify data is the same when read back *)
  let repo1 =
    S.Repo.v
      (config ~sw ~fs ~readonly:true ~fresh:false ~inline_contents:false
         root_no_inline)
  in
  let repo2 =
    S.Repo.v
      (config ~sw ~fs ~readonly:true ~fresh:false ~inline_contents:true
         root_inline)
  in
  let tree1' =
    S.Commit.of_hash repo1 (S.Commit.hash commit1)
    |> Option.get
    |> S.Commit.tree
  in
  let tree2' =
    S.Commit.of_hash repo2 (S.Commit.hash commit2)
    |> Option.get
    |> S.Commit.tree
  in
  (* Verify contents are identical *)
  Alcotest.(check (option string))
    "a"
    (S.Tree.find tree1' [ "a" ])
    (S.Tree.find tree2' [ "a" ]);
  Alcotest.(check (option string))
    "b"
    (S.Tree.find tree1' [ "b" ])
    (S.Tree.find tree2' [ "b" ]);
  Alcotest.(check (option string))
    "c/d"
    (S.Tree.find tree1' [ "c"; "d" ])
    (S.Tree.find tree2' [ "c"; "d" ]);
  S.Repo.close repo1;
  S.Repo.close repo2

(** Test that verifies small content is actually inlined in the node structure
*)
let test_inlining_structure ~fs () =
  let root = Eio.Path.(fs / "_build" / "test-inline-structure") in
  rm_dir root;
  Eio.Switch.run @@ fun sw ->
  let repo =
    S.Repo.v
      (config ~sw ~fs ~readonly:false ~fresh:true ~inline_contents:true root)
  in
  (* Create a tree with small content that should be inlined *)
  let tree = S.Tree.empty () in
  (* Note: inlining threshold is 48 bytes *serialized*, which includes:
     - 1-byte variant tag for the Contents.t encoding
     - 1-byte varint length prefix for the string
     So raw content must be < 46 bytes (45 or less) to be inlined. *)
  let tree = S.Tree.add tree [ "tiny" ] "x" in
  (* 1 byte raw -> 3 bytes serialized -> inlined *)
  let tree = S.Tree.add tree [ "small" ] "hello" in
  (* 5 bytes raw -> 7 bytes serialized -> inlined *)
  let tree =
    S.Tree.add tree [ "medium" ] "0123456789abc0123456789abc0123456789abc012345"
  in
  (* 45 bytes raw -> 47 bytes serialized -> inlined *)
  let tree =
    S.Tree.add tree [ "large" ] "0123456789abc0123456789abc0123456789abc0123456"
  in
  (* 46 bytes raw -> 48 bytes serialized -> NOT inlined *)
  (* Commit to persist the tree *)
  let commit = S.Commit.v repo ~parents:[] ~info tree in
  let _hash = S.Commit.hash commit in
  (* Access the node store to check the structure *)
  let commit' = S.Commit.of_hash repo (S.Commit.hash commit) |> Option.get in
  let tree' = S.Commit.tree commit' in
  (* Get the root node using to_backend_node *)
  let root_node =
    match S.Tree.destruct tree' with
    | `Node n -> S.to_backend_node n
    | `Contents _ -> Alcotest.fail "Expected a node"
  in
  (* Check the node structure using list *)
  let entries = S.Backend.Node.Val.list root_node in
  (* Count inlined vs non-inlined contents *)
  let inlined_count = ref 0 in
  let non_inlined_count = ref 0 in
  List.iter
    (fun (step, value) ->
      match value with
      | `Contents_inlined (bytes, _) ->
          [%log.debug "Inlined content at %s: %S" step bytes];
          incr inlined_count
      | `Contents _ ->
          [%log.debug "Non-inlined content at %s" step];
          incr non_inlined_count
      | `Node _ -> [%log.debug "Node at %s" step])
    entries;
  (* Verify: 3 entries should be inlined (tiny, small, medium), 1 should not (large) *)
  Alcotest.(check int) "inlined count" 3 !inlined_count;
  Alcotest.(check int) "non-inlined count" 1 !non_inlined_count;
  S.Repo.close repo

(** Test inlining with non-default metadata.

    This exercises the [inlined-x-i] / [inlined-x-d] serialization variants in
    inode.ml, which are distinct from the default-metadata variants [inlined-i]
    / [inlined-d]. *)
module Test_metadata = struct
  module Metadata = struct
    type t = [ `Normal | `Exec ]

    let t = Irmin.Type.enum "metadata" [ ("normal", `Normal); ("exec", `Exec) ]
    let default = `Normal
    let merge = Irmin.Merge.default t
  end

  module Schema_with_metadata = struct
    module Metadata = Metadata
    module Contents = Irmin.Contents.String_v2
    module Path = Irmin.Path.String_list
    module Branch = Irmin.Branch.String
    module Hash = Irmin.Hash.SHA1
    module Node = Irmin.Node.Generic_key.Make_v2 (Hash) (Path) (Metadata)
    module Commit = Irmin.Commit.Generic_key.Make_v2 (Hash)
    module Info = Irmin.Info.Default
  end

  module S_meta = struct
    module Maker = Irmin_pack_unix.Maker (Conf)
    include Maker.Make (Schema_with_metadata)
  end

  let test_inlining_with_metadata ~fs () =
    let root = Eio.Path.(fs / "_build" / "test-inline-metadata") in
    rm_dir root;
    Eio.Switch.run @@ fun sw ->
    let repo =
      S_meta.Repo.v
        (config ~sw ~fs ~readonly:false ~fresh:true ~inline_contents:true root)
    in
    let tree = S_meta.Tree.empty () in
    (* Small content with default metadata -> inlined-i/inlined-d *)
    let tree = S_meta.Tree.add tree [ "default" ] "small" in
    (* Small content with non-default metadata -> inlined-x-i/inlined-x-d *)
    let tree = S_meta.Tree.add tree [ "exec" ] ~metadata:`Exec "tiny" in
    (* Large content with non-default metadata -> not inlined *)
    let tree =
      S_meta.Tree.add tree [ "exec_large" ] ~metadata:`Exec
        (String.make 100 'x')
    in
    let commit =
      S_meta.Commit.v repo ~parents:[] ~info:S_meta.Info.empty tree
    in
    let hash = S_meta.Commit.hash commit in
    (* Read back and verify values and metadata *)
    let commit' = S_meta.Commit.of_hash repo hash |> Option.get in
    let tree' = S_meta.Commit.tree commit' in
    Alcotest.(check (option string))
      "default meta content" (Some "small")
      (S_meta.Tree.find tree' [ "default" ]);
    Alcotest.(check (option string))
      "exec meta content" (Some "tiny")
      (S_meta.Tree.find tree' [ "exec" ]);
    Alcotest.(check (option string))
      "exec large content"
      (Some (String.make 100 'x'))
      (S_meta.Tree.find tree' [ "exec_large" ]);
    (* Verify metadata was preserved *)
    let check_meta path expected =
      match S_meta.Tree.find_all tree' path with
      | None -> Alcotest.failf "missing entry at %s" (String.concat "/" path)
      | Some (_, m) ->
          Alcotest.(check string)
            ("metadata at " ^ String.concat "/" path)
            expected
            (Irmin.Type.to_string Metadata.t m)
    in
    check_meta [ "default" ] "\"normal\"";
    check_meta [ "exec" ] "\"exec\"";
    check_meta [ "exec_large" ] "\"exec\"";
    S_meta.Repo.close repo
end

(** Test that zero inline_contents_max_bytes disables inlining *)
let test_zero_inline_threshold ~fs () =
  let root = Eio.Path.(fs / "_build" / "test-inline-zero") in
  rm_dir root;
  Eio.Switch.run @@ fun sw ->
  let repo =
    S.Repo.v
      (config ~sw ~fs ~readonly:false ~fresh:true ~inline_contents:true
         ~inline_contents_max_bytes:0 root)
  in
  let tree = S.Tree.empty () in
  let tree = S.Tree.add tree [ "small" ] "abc" in
  let commit = S.Commit.v repo ~parents:[] ~info tree in
  let hash = S.Commit.hash commit in
  let commit' = S.Commit.of_hash repo hash |> Option.get in
  let tree' = S.Commit.tree commit' in
  let small = S.Tree.find tree' [ "small" ] in
  Alcotest.(check (option string)) "small content" (Some "abc") small;
  S.Repo.close repo

let tests ~fs =
  let tc name f = Alcotest.test_case name `Quick f in
  [
    tc "without inlining" (test_without_inlining ~fs);
    tc "with inlining" (test_with_inlining ~fs);
    tc "content equivalence" (test_content_equivalence ~fs);
    tc "inlining structure" (test_inlining_structure ~fs);
    tc "inlining with non-default metadata"
      (Test_metadata.test_inlining_with_metadata ~fs);
    tc "zero inline threshold disables inlining"
      (test_zero_inline_threshold ~fs);
  ]
