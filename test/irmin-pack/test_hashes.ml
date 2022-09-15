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

let root = Filename.concat "_build" "test-irmin-tezos"

let conf =
  Irmin_pack.config ~readonly:false ~fresh:true ~index_log_size:1000 root

let zero = Bytes.make 10 '0'

let hash_zero =
  "d81e60258ecc8bd7064c8703888aececfc54e29ff94f7a2d9a84667a500548e1"

let bindings steps = List.map (fun x -> ([ x ], zero)) steps

let check_string ~msg ~expected ~got =
  let got = Hex.of_string got |> Hex.show in
  Alcotest.(check string) (Fmt.str "%s" msg) expected got

let check_iter iter_type (iter : 'a -> (string -> unit) -> unit) v checks =
  let counter = ref 0 in
  iter v (fun x ->
      match List.nth_opt checks !counter with
      | None -> Alcotest.failf "No more calls to %s left" iter_type
      | Some (msg, expected) ->
          let msg = Fmt.str "Check %s:%s" iter_type msg in
          check_string ~msg ~expected ~got:x;
          incr counter);
  if !counter <> List.length checks then
    Alcotest.failf "More calls to %s expected" iter_type

module Test
    (Conf : Irmin_pack.Conf.S)
    (Schema : Irmin.Schema.Extended
                with type Contents.t = bytes
                 and type Metadata.t = unit
                 and type Path.t = string list
                 and type Path.step = string
                 and type Branch.t = string
                 and module Info = Irmin.Info.Default) =
struct
  module Store = struct
    module Maker = Irmin_pack_unix.Maker (Conf)
    include Maker.Make (Schema)
  end

  include Store

  let build_tree steps =
    let bindings = bindings steps in
    let tree = Tree.empty () in
    let+ tree =
      Lwt_list.fold_left_s (fun tree (k, v) -> Tree.add tree k v) tree bindings
    in
    tree

  let persist_tree tree =
    let* repo = Repo.v conf in
    let* init_commit =
      Commit.v ~parents:[] ~info:Info.empty repo
        (Tree.singleton [ "singleton-step" ] (Bytes.of_string "singleton-val"))
    in
    let h = Commit.hash init_commit in
    let info = Info.v ~author:"Tezos" 0L in
    let* commit =
      Commit.v ~parents:[ Irmin_pack_unix.Pack_key.v_indexed h ] ~info repo tree
    in
    let tree = Commit.tree commit in
    Lwt.return (repo, tree, commit)

  let check_hardcoded_hash msg expected got =
    let got = (Irmin.Type.to_string Store.Hash.t) got in
    Alcotest.(check string)
      (Fmt.str "Check hardcoded hash: %s" msg)
      expected got
end

module Test_tezos_conf = struct
  module Store = Test (Irmin_tezos.Conf) (Irmin_tezos.Schema)
  module Contents = Store.Backend.Contents
  module Node = Store.Backend.Node
  module Commit = Store.Backend.Commit

  let hash_root_small_tree =
    "83722c2791a1c47dada4718656a20a2f3a063ae9945b475e67bbb6ef29d88ca4"

  let contents_hash () =
    let h0 = Contents.Hash.hash zero in
    let encode_bin_hash = Irmin.Type.(unstage (encode_bin Contents.Hash.t)) in
    encode_bin_hash h0 (fun x ->
        check_string ~msg:"Check encode_bin: h0" ~expected:hash_zero ~got:x);
    let encode_bin_val = Irmin.Type.(unstage (encode_bin Contents.Val.t)) in
    let checks =
      [ ("header of zero", "0a"); ("zero", "30303030303030303030") ]
    in
    check_iter "encode_bin" encode_bin_val zero checks;
    let pre_hash_val = Irmin.Type.(unstage (pre_hash Contents.Val.t)) in
    let checks =
      [
        ("header of zero", "000000000000000a"); ("zero", "30303030303030303030");
      ]
    in
    check_iter "pre_hash" pre_hash_val zero checks;
    Store.check_hardcoded_hash "contents hash"
      "CoWHVKM5r2eiHQxhicqakkr5FwJfabahGBwCCWzRPCNPs79CoZty" h0;
    Lwt.return_unit

  let some_steps = [ "00"; "01" ]

  let checks_bindings_pre_hash steps =
    let nb_steps = Fmt.str "%016x" (List.length steps) in
    let checks =
      List.fold_left
        (fun acc s ->
          let hex = Hex.of_string s |> Hex.show in
          let check_step =
            [
              ("node type is contents", "ff00000000000000");
              ("len of step ", "02");
              (s, hex);
              ("len of contents hash", "0000000000000020");
              ("hash of contents", hash_zero);
            ]
            |> List.rev
          in
          check_step @ acc)
        [] steps
      |> List.rev
    in
    ("len of values", nb_steps) :: checks

  let inode_values_hash () =
    let* tree = Store.build_tree some_steps in
    let* repo, tree, _ = Store.persist_tree tree in
    let* root_node =
      match Store.Tree.destruct tree with
      | `Contents _ -> Alcotest.fail "Expected root to be node"
      | `Node x -> Store.to_backend_node x
    in
    let h = Node.Hash.hash root_node in
    let encode_bin_hash = Irmin.Type.(unstage (encode_bin Node.Hash.t)) in
    encode_bin_hash h (fun x ->
        check_string ~msg:"Check encode_bin: node hash"
          ~expected:hash_root_small_tree ~got:x);
    let pre_hash_val = Irmin.Type.(unstage (pre_hash Node.Val.t)) in
    let checks = checks_bindings_pre_hash some_steps in
    check_iter "pre_hash" pre_hash_val root_node checks;
    Store.check_hardcoded_hash "node hash"
      "CoVeCU4o3dqmfdwqt2vh8LDz9X6qGbTUyLhgVvFReyzAvTf92AKx" h;
    let* () = Store.Repo.close repo in
    Lwt.return_unit

  let commit_hash () =
    let* tree = Store.build_tree some_steps in
    let* repo, _, commit = Store.persist_tree tree in
    let commit_val = Store.to_backend_commit commit in
    let h = Commit.Hash.hash commit_val in
    let encode_bin_hash = Irmin.Type.(unstage (encode_bin Commit.Hash.t)) in
    encode_bin_hash h (fun x ->
        check_string ~msg:"commit hash"
          ~expected:
            "c20860adda3c3d40d8d03fab22b07e889979cdac880d979711aa852a0896ae30"
          ~got:x);
    let checks =
      [
        ("hash of root node", hash_root_small_tree);
        ("len of parents", "01");
        ( "parent hash",
          "634d894802f9032ef48bbe1253563dbeb2aad7dc684da83bdea5692fde2185ae" );
        ("date", "0000000000000000");
        ("len of author", "05");
        ("author", "54657a6f73");
        ("len of message", "00");
        ("message", "");
      ]
    in
    let encode_bin_val = Irmin.Type.(unstage (encode_bin Commit.Val.t)) in
    check_iter "encode_bin" encode_bin_val commit_val checks;
    let checks =
      [
        ("len of node hash", "0000000000000020");
        ("hash of root node", hash_root_small_tree);
        ("len of parents", "0000000000000001");
        ("len of parent hash", "0000000000000020");
        ( "parent hash",
          "634d894802f9032ef48bbe1253563dbeb2aad7dc684da83bdea5692fde2185ae" );
        ("date", "0000000000000000");
        ("len of author", "0000000000000005");
        ("author", "54657a6f73");
        ("len of message", "0000000000000000");
        ("message", "");
      ]
    in
    let pre_hash_val = Irmin.Type.(unstage (pre_hash Commit.Val.t)) in
    check_iter "pre_hash" pre_hash_val commit_val checks;
    Store.check_hardcoded_hash "commit hash"
      "CoW7mALEs2vue5cfTMdJfSAjNmjmALYS1YyqSsYr9siLcNEcrvAm" h;
    let* () = Store.Repo.close repo in
    Lwt.return_unit
end

module Test_small_conf = struct
  module Conf = struct
    let entries = 2
    let stable_hash = 3
    let contents_length_header = Some `Varint
    let inode_child_order = `Seeded_hash
    let forbid_empty_dir_persistence = true
  end

  module Store = Test (Conf) (Irmin_tezos.Schema)
  module Node = Store.Backend.Node

  let many_steps = [ "00"; "01"; "02"; "03"; "04"; "05" ]

  let checks =
    [
      ("inode tree", "01");
      ("depth", "00");
      ("len of tree", "06");
      ("d", "02");
      ("e", "00");
      ("g", "aa670a7e66b80a4d5f0e2e35b0c7fc4fa8d3e2d62a8b90eb2ff1d184dde9d0fa");
      ("b1", "01");
      ( "hash ",
        "821707c86f7030b1102397feb88d454076ec64744dfd9811b8254bd61d396cfe" );
    ]

  let inode_tree_hash () =
    let* tree = Store.build_tree many_steps in
    let* repo, tree, _ = Store.persist_tree tree in
    let* root_node =
      match Store.Tree.destruct tree with
      | `Contents _ -> Alcotest.fail "Expected root to be node"
      | `Node x -> Store.to_backend_node x
    in
    let h = Node.Hash.hash root_node in
    let pre_hash_hash = Irmin.Type.(unstage (pre_hash Node.Hash.t)) in
    pre_hash_hash h (fun x ->
        check_string ~msg:"node hash"
          ~expected:
            "e670a325ac78b2b6949b8f9fa448b17aa708ef39eb29c9e364be473f988329ea"
          ~got:x);
    let pre_hash_val = Irmin.Type.(unstage (pre_hash Node.Val.t)) in
    check_iter "pre_hash" pre_hash_val root_node checks;
    Store.check_hardcoded_hash "node hash"
      "CoWPo8s8h81q8skRqfPLTAJvq4ioFKS6rQhdRcY5nd6HQz2upwp4" h;
    let* () = Store.Repo.close repo in
    Lwt.return_unit
end

module Test_V1 = struct
  module Schema = struct
    include Irmin_tezos.Schema

    module Commit
        (Node_key : Irmin.Key.S with type hash = Hash.t)
        (Commit_key : Irmin.Key.S with type hash = Hash.t) =
    struct
      module M = Irmin.Commit.Generic_key.Make (Hash) (Node_key) (Commit_key)
      module Commit = Irmin.Commit.V1.Make (Hash) (M)
      include Commit
    end
  end

  module Store = Test (Conf) (Schema)
  module Commit = Store.Backend.Commit

  let many_steps = [ "00"; "01"; "02"; "03"; "04"; "05" ]

  let commit_hash () =
    let* tree = Store.build_tree many_steps in
    let* repo, _, commit = Store.persist_tree tree in
    let commit_val = Store.to_backend_commit commit in
    let checks =
      [
        ("len of node hash", "0000000000000020");
        ( "hash of root node",
          "3ab1c8feb08812cd1ffd8ec1ca4f861a578b700fa7dd9daab4c63d4e86638f99" );
        ("len of parents", "0000000000000001");
        ("len of parent hash", "0000000000000020");
        ( "parent hash",
          "634d894802f9032ef48bbe1253563dbeb2aad7dc684da83bdea5692fde2185ae" );
        ("date", "0000000000000000");
        ("len of author", "0000000000000005");
        ("author", "54657a6f73");
        ("len of message", "0000000000000000");
        ("message", "");
      ]
    in
    let encode_bin_val = Irmin.Type.(unstage (encode_bin Commit.Val.t)) in
    check_iter "encode_bin" encode_bin_val commit_val checks;
    let* () = Store.Repo.close repo in
    Lwt.return_unit
end

let tests =
  let tc name f = Alcotest_lwt.test_case name `Quick (fun _switch -> f) in
  [
    tc "contents hash" Test_tezos_conf.contents_hash;
    tc "inode_values hash" Test_tezos_conf.inode_values_hash;
    tc "inode_tree hash" Test_small_conf.inode_tree_hash;
    tc "commit hash" Test_tezos_conf.commit_hash;
    tc "V1 commit hash" Test_V1.commit_hash;
  ]
