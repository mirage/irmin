(*
 * Copyright (c) 2018-2023 Tarides <contact@tarides.com>
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

let root = Filename.concat "_build" "test-multicore"
let src = Logs.Src.create "tests.multicore" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

module Store = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)

  let config ?(readonly = false) ?(fresh = true) root =
    Irmin_pack.config ~readonly ?index_log_size ~fresh root
end

module Tree = Store.Tree

let info () = Store.Info.empty

type shape = [ `Contents of string | `Node of (string * shape) list ]

let shape0 : shape =
  `Node
    [
      ("a", `Contents "a");
      ("b", `Contents "b");
      ("c", `Node [ ("d", `Contents "cd"); ("e", `Contents "ce") ]);
      ("f", `Node [ ("g", `Node [ ("h", `Contents "fgh") ]) ]);
      ("i", `Contents "i");
    ]

let shape1 : shape =
  `Node
    [
      ("a", `Contents "a");
      ("b", `Contents "b");
      ( "c",
        `Node
          [
            ("d", `Contents "cd");
            ("e", `Contents "ce");
            ("c_new", `Node [ ("c_new_new", `Contents "c_new_new") ]);
          ] );
      ("f", `Node [ ("g", `Node [ ("h", `Contents "fgh") ]) ]);
      ("i", `Contents "i");
      ("new", `Contents "new");
      ( "new_new",
        `Node [ ("a", `Contents "new_new_a"); ("b", `Contents "new_new_b") ] );
    ]

let shape2 : shape =
  `Node
    [
      ("a", `Contents "a");
      ("c", `Node [ ("e", `Contents "ce") ]);
      ( "f",
        `Node
          [
            ("g", `Node [ ("h", `Contents "updated") ]);
            ("fresh", `Contents "added");
          ] );
      ("i", `Contents "i");
    ]

let rec flatten_shape acc path : shape -> _ = function
  | `Contents c -> (List.rev path, c) :: acc
  | `Node children ->
      List.fold_left
        (fun acc (name, child) -> flatten_shape acc (name :: path) child)
        acc children

let flatten_shape shape = flatten_shape [] [] shape

let make_tree shape =
  List.fold_left
    (fun tree (k, v) -> Tree.add tree k v)
    (Tree.empty ()) (flatten_shape shape)

let rec list_shape acc path : shape -> _ = function
  | `Contents _c -> (List.rev path, []) :: acc
  | `Node children ->
      let l = List.map (fun (name, child) -> (name, make_tree child)) children in
      let acc = (List.rev path, l) :: acc in
      List.fold_left
        (fun acc (name, child) -> list_shape acc (name :: path) child)
        acc children

let list_shape shape = list_shape [] [] shape

let make_store shape =
  let repo = Store.Repo.v (Store.config ~fresh:true root) in
  let main = Store.main repo in
  let tree = make_tree shape in
  let () = Store.set_tree_exn ~info main [] tree in
  Store.Repo.close repo

let domains_run d_mgr fns =
  let count = Atomic.make (List.length fns) in
  let fibers =
    List.map
      (fun fn () ->
        Eio.Domain_manager.run d_mgr (fun () ->
            Atomic.decr count;
            while Atomic.get count > 0 do
              Domain.cpu_relax ()
            done;
            fn ()))
      fns
  in
  Eio.Fiber.all fibers

let domains_spawn d_mgr ?(nb = 2) fn =
  domains_run d_mgr @@ List.init nb (fun _ -> fn)

let find_all tree paths =
  List.iter
    (fun (path, expected) ->
      match Store.Tree.find tree path with
      | None -> assert false
      | Some value -> assert (expected = value))
    paths

let test_find d_mgr =
  Logs.set_level None;
  make_store shape0;
  let repo = Store.Repo.v (Store.config ~readonly:true ~fresh:false root) in
  let tree = Store.main repo |> Store.Head.get |> Store.Commit.tree in
  let paths = flatten_shape shape0 in
  domains_spawn d_mgr (fun () -> find_all tree paths);
  Store.Repo.close repo

let rec expected_lengths acc path : shape -> _ = function
  | `Contents _ -> (List.rev path, None) :: acc
  | `Node children ->
      let acc = (List.rev path, Some (List.length children)) :: acc in
      List.fold_left
        (fun acc (name, child) -> expected_lengths acc (name :: path) child)
        acc children

let expected_lengths shape = expected_lengths [] [] shape

let test_length d_mgr =
  Logs.set_level None;
  make_store shape0;
  let repo = Store.Repo.v (Store.config ~readonly:true ~fresh:false root) in
  let tree = Store.main repo |> Store.Head.get |> Store.Commit.tree in
  let lengths = expected_lengths shape0 in
  let all_length () =
    List.iter
      (fun (path, expected) ->
        let value = Store.Tree.length tree path in
        let value = if value = 0 then None else Some value in
        assert (expected = value))
      lengths
  in
  domains_spawn ~nb:2 d_mgr all_length;
  Store.Repo.close repo

let rec remove_all acc path : shape -> _ = function
  | `Contents _ -> [ `Remove (List.rev path) ]
  | `Node children ->
      List.fold_left
        (fun acc (name, child) -> remove_all acc (name :: path) child)
        acc children

let rec diff_shape acc path (old_shape : shape option) (new_shape : shape) =
  match (old_shape, new_shape) with
  | Some (`Contents old), `Contents new_ when old = new_ -> acc
  | _, `Contents new_ -> `Add (List.rev path, new_) :: acc
  | _, `Node new_children ->
      let old_children =
        match old_shape with
        | None -> []
        | Some (`Node old_children) -> old_children
        | _ -> assert false
      in
      let acc =
        List.fold_left
          (fun acc (old_name, old_child) ->
            match List.assoc_opt old_name new_children with
            | None -> remove_all acc (old_name :: path) old_child
            | Some _ -> acc)
          acc old_children
      in
      List.fold_left
        (fun acc (name, new_child) ->
          let old_child = List.assoc_opt name old_children in
          diff_shape acc (name :: path) old_child new_child)
        acc new_children

let diff_shape old_shape new_shape =
  List.rev @@ diff_shape [] [] (Some old_shape) new_shape

let test_add_remove d_mgr =
  Logs.set_level None;
  make_store shape0;
  let repo = Store.Repo.v (Store.config ~readonly:true ~fresh:false root) in
  let tree = Store.main repo |> Store.Head.get |> Store.Commit.tree in
  let patch = diff_shape shape0 shape1 in
  let after_paths = flatten_shape shape1 in
  let add_all () =
    let tree =
      List.fold_left
        (fun tree -> function
          | `Add (path, contents) -> Tree.add tree path contents
          | `Remove path -> Tree.remove tree path)
        tree patch
    in
    find_all tree after_paths;
    List.iter
      (function
        | `Add (name, _) -> assert (Tree.mem tree name)
        | `Remove name -> assert (not (Tree.mem tree name)))
      patch
  in
  domains_spawn ~nb:2 d_mgr add_all;
  Store.Repo.close repo

let apply_op tree = function
  | `Add (name, contents) -> Tree.add tree name contents
  | `Remove name -> Tree.remove tree name

let check_patch_was_applied patch tree =
  List.iter
    (function
      | `Add (name, contents) ->
          assert (Store.Tree.find tree name = Some contents)
      | `Remove name -> assert (not (Store.Tree.mem tree name)))
    patch

let test_commit d_mgr =
  Logs.set_level None;
  make_store shape0;
  let repo = Store.Repo.v (Store.config ~readonly:false ~fresh:false root) in
  let store = Store.main repo in
  let patch01 = diff_shape shape0 shape1 in
  let patch02 = diff_shape shape0 shape2 in
  let do_commit patch () =
    List.iter
      (fun op ->
        let tree = Store.Head.get store |> Store.Commit.tree in
        let tree = apply_op tree op in
        Store.set_tree_exn ~info store [] tree)
      patch;
    let tree = Store.main repo |> Store.Head.get |> Store.Commit.tree in
    check_patch_was_applied patch tree
  in
  domains_run d_mgr [ do_commit patch01; do_commit patch02 ];
  Store.Repo.close repo

let test_merkle d_mgr =
  Logs.set_level None;
  make_store shape0;
  let repo = Store.Repo.v (Store.config ~readonly:false ~fresh:false root) in
  let tree = Store.main repo |> Store.Head.get |> Store.Commit.tree in
  let hash = Store.Tree.key tree |> Option.get in
  let patch01 = diff_shape shape0 shape1 in
  let patch02 = diff_shape shape0 shape2 in
  let do_proof patch () =
    let fn tree =
      let new_tree = List.fold_left apply_op tree patch in
      (new_tree, ())
    in
    let proof, () = Store.Tree.produce_proof repo hash fn in
    match Store.Tree.verify_proof proof fn with
    | Ok (new_tree, ()) -> check_patch_was_applied patch new_tree
    | Error _ -> assert false
  in
  domains_run d_mgr [ do_proof patch01; do_proof patch02 ];
  Store.Repo.close repo

let test_hash d_mgr =
  Logs.set_level None;
  make_store shape0;
  let repo = Store.Repo.v (Store.config ~readonly:false ~fresh:false root) in
  let tree = Store.main repo |> Store.Head.get |> Store.Commit.tree in
  let patch01 = diff_shape shape0 shape1 in
  let patch12 = diff_shape shape1 shape2 in
  let patch = patch01 @ patch12 in
  let _, trees =
    List.fold_left
      (fun (tree, trees) op ->
        let new_tree = apply_op tree op in
        (new_tree, new_tree :: trees))
      (tree, [ tree ]) patch
  in
  let do_hash result () =
    let hashes = List.map Store.Tree.hash trees in
    Atomic.set result hashes
  in
  let result1 = Atomic.make [] in
  let result2 = Atomic.make [] in
  domains_run d_mgr [ do_hash result1; do_hash result2 ];
  List.iter2
    (fun h1 h2 -> assert (h1 = h2))
    (Atomic.get result1) (Atomic.get result2);
  Store.Repo.close repo

let list_all cache tree paths =
  List.iter
  (fun (path, expected) ->
    let value = Store.Tree.list ~cache tree path in
    assert (List.length expected = List.length value);
    List.iter (fun (s, t) ->
      let t' = List.assoc s value in
      let diffs = Store.Tree.diff t t' in
      assert (diffs = [])
    ) expected)
  paths

let test_list_disk ~cache d_mgr =
  Logs.set_level None;
  make_store shape0;
  let repo = Store.Repo.v (Store.config ~readonly:true ~fresh:false root) in
  let tree = Store.main repo |> Store.Head.get |> Store.Commit.tree in
  let paths = list_shape shape0 in
  domains_spawn d_mgr (fun () -> list_all cache tree paths);
  Store.Repo.close repo

let test_list_mem ~cache d_mgr =
  Logs.set_level None;
  make_store shape0;
  let repo = Store.Repo.v (Store.config ~readonly:true ~fresh:false root) in
  let tree = Store.main repo |> Store.Head.get |> Store.Commit.tree in
  let patch = diff_shape shape0 shape1 in
  let paths = list_shape shape1 in
  let tree = List.fold_left apply_op tree patch in
  domains_spawn d_mgr (fun _ -> list_all cache tree paths);
  Store.Repo.close repo

let test_commit_of_hash d_mgr =
  Logs.set_level None;
  make_store shape0;
  let repo = Store.Repo.v (Store.config ~readonly:false ~fresh:false root) in
  let store = Store.main repo in
  let patch01 = diff_shape shape0 shape1 in
  let patch02 = diff_shape shape0 shape2 in
  let commit1 = Store.Head.get store in
  let hash1 = Store.Commit.hash commit1 in
  let tree1 = Store.Commit.tree commit1 in
  List.iter
  (fun op ->
    let tree = Store.Commit.tree commit1 in
    let tree = apply_op tree op in
    Store.set_tree_exn ~info store [] tree)
  patch01;
  let commit2 = Store.Head.get store in
  let hash2 = Store.Commit.hash commit2 in
  let tree2 = Store.Commit.tree commit2 in
  List.iter
  (fun op ->
    let tree = Store.Commit.tree commit2 in
    let tree = apply_op tree op in
    Store.set_tree_exn ~info store [] tree)
  patch02;
  let commit3 = Store.Head.get store in
  let hash3 = Store.Commit.hash commit3 in
  let tree3 = Store.Commit.tree commit3 in
  let do_commit_of_hash () =
    let t1 = Store.Commit.of_hash repo hash1 |> Option.get |> Store.Commit.tree in
    let diffs = Store.Tree.diff tree1 t1 in
    assert (diffs = []);
    let t2 = Store.Commit.of_hash repo hash2 |> Option.get |> Store.Commit.tree in
    let diffs = Store.Tree.diff tree2 t2 in
    assert (diffs = []);
    let t3 = Store.Commit.of_hash repo hash3 |> Option.get |> Store.Commit.tree in
    let diffs = Store.Tree.diff tree3 t3 in
    assert (diffs = [])
  in
  domains_spawn d_mgr do_commit_of_hash;
  Store.Repo.close repo

let test_commit_parents d_mgr =
  Logs.set_level None;
  make_store shape0;
  let repo = Store.Repo.v (Store.config ~readonly:false ~fresh:false root) in
  let store = Store.main repo in
  let patch01 = diff_shape shape0 shape1 in
  let commit = Store.Head.get store in
  let tree = Store.Commit.tree commit in
  let commits =
    snd @@ List.fold_left_map
    (fun tree op ->
      let tree = apply_op tree op in
      Store.set_tree_exn ~info store [] tree;
      tree, Store.Head.get store) tree patch01
  in
  let do_commit_parents () =
    ignore
      (List.fold_left
        (fun parent commit ->
        let parents = Store.Commit.parents commit in
        assert (parents = [Store.Commit.key parent]);
        commit)
        commit commits)
  in
  domains_spawn d_mgr do_commit_parents;
  Store.Repo.close repo

  let tests d_mgr =
  let tc name fn = Alcotest.test_case name `Quick (fun () -> fn d_mgr) in
  [
    tc "find" test_find;
    tc "length" test_length;
    tc "add / remove" test_add_remove;
    tc "commit" test_commit;
    tc "merkle" test_merkle;
    tc "hash" test_hash;
    tc "list-disk-no-cache" (test_list_disk ~cache:false);
    tc "list-disk-with-cache" (test_list_disk ~cache:true);
    tc "list-mem-no-cache" (test_list_mem ~cache:false);
    tc "list-mem-with-cache" (test_list_mem ~cache:true);
    tc "commit-of-hash" test_commit_of_hash;
    tc "commit-parents" test_commit_parents;
  ]
