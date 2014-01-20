(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open OUnit
open Test_common
open Lwt

type t = {
  name : string;
  init : unit -> unit Lwt.t;
  clean: unit -> unit Lwt.t;
  store: (module Irmin.S);
}

let unit () =
  return_unit

module Make (S: Irmin.S) = struct

  module Common = Make(S)
  open Common
  open S

  let run x test =
    try Lwt_unix.run (x.init () >>= test >>= x.clean)
    with e ->
      Lwt_unix.run (x.clean ());
      raise e

  type e = {
    v1: B.t;
    v2: B.t;
    kv1: K.t Lwt.t Lazy.t;
    kv2: K.t Lwt.t Lazy.t;
    r1: R.t;
    r2: R.t;
  }

  let mk t =
    let v1 = B.of_bytes_exn "foo" in
    let v2 = B.of_bytes_exn "" in
    let kv1 = lazy (S.Internal.add (S.internal t) (IrminValue.Blob v1)) in
    let kv2 = lazy (S.Internal.add (S.internal t) (IrminValue.Blob v2)) in
    let r1 = R.of_bytes "refs/foo" in
    let r2 = R.of_bytes "refs/bar" in
    return { v1; v2; kv1; kv2; r1; r2 }

  let test_blobs x () =
    let test () =
      create () >>= fun t                    ->
      mk t >>= function { v1; v2; kv1; kv2 } ->

      Lazy.force kv1 >>= fun kv1 ->
      Lazy.force kv2 >>= fun kv2 ->

      let v = blob t in
      Blob.add v v1                >>= fun k1'  ->
      assert_key_equal "kv1" kv1 k1';
      Blob.add v v1                >>= fun k1'' ->
      assert_key_equal "kv1" kv1 k1'';
      Blob.add v v2                >>= fun k2'  ->
      assert_key_equal "kv2" kv2 k2';
      Blob.add v v2                >>= fun k2'' ->
      assert_key_equal "kv2" kv2 k2'';
      Blob.read v kv1              >>= fun v1'  ->
      assert_blob_opt_equal "v1" (Some v1) v1';
      Blob.read v kv2              >>= fun v2'  ->
      assert_blob_opt_equal "v2" (Some v2) v2';
      return_unit
    in
    run x test

  let test_trees x () =
    let test () =
      create () >>= fun t          ->
      mk t >>= function { v1; v2 } ->
      let tree = tree t in

      (* Create a node containing t1(v1) *)
      Tree.leaf tree v1           >>= fun k1  ->
      Tree.leaf tree v1           >>= fun k1' ->
      assert_key_equal "k1.1" k1 k1';
      Tree.read_exn tree k1       >>= fun t1  ->
      Tree.add tree t1            >>= fun k1''->
      assert_key_equal "k1.2" k1 k1'';

      (* Create the tree  t2 -b-> t1(v1) *)
      Tree.node tree ["b", t1]     >>= fun k2  ->
      Tree.node tree ["b", t1]     >>= fun k2' ->
      assert_key_equal "k2.1" k2 k2';
      Tree.read_exn tree k2        >>= fun t2  ->
      Tree.add tree t2             >>= fun k2''->
      assert_key_equal "k2.2" k2 k2'';
      Tree.sub_exn tree t2 ["b"]  >>= fun t1' ->
      assert_tree_equal "t1.1" t1 t1';
      Tree.add tree t1'           >>= fun k1''->
      assert_key_equal "k1.3" k1 k1'';

      (* Create the tree t3 -a-> t2 -b-> t1(v1) *)
      Tree.node tree ["a", t2]         >>= fun k3  ->
      Tree.node tree ["a", t2]         >>= fun k3' ->
      assert_key_equal "k3.1" k3 k3';
      Tree.read_exn tree k3            >>= fun t3  ->
      Tree.add tree t3                 >>= fun k3''->
      assert_key_equal "k3.2" k3 k3'';
      Tree.sub_exn tree t3 ["a"]       >>= fun t2' ->
      assert_tree_equal "t2.1" t2 t2';
      Tree.add tree t2'                >>= fun k2''->
      assert_key_equal "k2.3" k2 k2'';
      Tree.sub_exn tree t2' ["b"]      >>= fun t1' ->
      assert_tree_equal "t1.2" t1 t1';
      Tree.sub tree t3 ["a";"b"]       >>= fun t1' ->
      assert_tree_opt_equal "t1.3" (Some t1) t1';

      Tree.find tree t1 []            >>= fun v11 ->
      assert_blob_opt_equal "v1.1" (Some v1) v11;
      Tree.find tree t2 ["b"]         >>= fun v12 ->
      assert_blob_opt_equal "v1.2" (Some v1) v12;
      Tree.find tree t3 ["a";"b"]     >>= fun v13 ->
      assert_blob_opt_equal "v1" (Some v1) v13;

      (* Create the tree t6 -a-> t5 -b-> t1(v1)
                                   \-c-> t4(v2) *)
      Tree.leaf tree v2                   >>= fun k4  ->
      Tree.read_exn tree k4               >>= fun t4  ->
      Tree.node tree [("b",t1); ("c",t4)] >>= fun k5  ->
      Tree.read_exn tree k5               >>= fun t5  ->
      Tree.node tree ["a",t5]             >>= fun k6  ->
      Tree.read_exn tree k6               >>= fun t6  ->
      Tree.update tree t3 ["a";"c"] v2    >>= fun t6' ->
      assert_tree_equal "tree" t6 t6';

      return_unit
    in
    run x test

  let test_commits x () =
    let test () =
      create () >>= fun t      ->
      mk t >>= function { v1 } ->

      let tree = tree t in
      let commit = commit t in

      (* t3 -a-> t2 -b-> t1(v1) *)
      Tree.leaf tree v1           >>= fun k1  ->
      Tree.read_exn tree k1       >>= fun t1  ->
      Tree.node tree ["a", t1]    >>= fun k2  ->
      Tree.read_exn tree k2       >>= fun t2  ->
      Tree.node tree ["b", t2]    >>= fun k3  ->
      Tree.read_exn tree k3       >>= fun t3  ->

      (* r1 : t2 *)
      Commit.commit commit ~tree:t2 ~parents:[] >>= fun kr1 ->
      Commit.commit commit ~tree:t2 ~parents:[] >>= fun kr1'->
      assert_key_equal "kr1" kr1 kr1';
      Commit.read_exn commit kr1 >>= fun r1  ->

      (* r1 -> r2 : t3 *)
      Commit.commit commit ~tree:t3 ~parents:[r1] >>= fun kr2  ->
      Commit.commit commit ~tree:t3 ~parents:[r1] >>= fun kr2' ->
      assert_key_equal "kr2" kr2 kr2';
      Commit.read_exn commit kr2 >>= fun r2   ->

      Commit.list commit kr1 >>= fun kr1s ->
      assert_keys_equal "g1" [kr1] kr1s;

      Commit.list commit kr2 >>= fun kr2s ->
      assert_keys_equal "g2" [kr1; kr2] kr2s;

     return_unit
    in
    run x test

  let test_references x () =
    let test () =
      create () >>= fun t                    ->
      mk t >>= function { kv1; kv2; r1; r2 } ->

      Lazy.force kv1 >>= fun kv1 ->
      Lazy.force kv2 >>= fun kv2 ->

      let reference = reference t in
      Reference.update reference r1 kv1 >>= fun ()  ->
      Reference.read   reference r1     >>= fun k1' ->
      assert_key_opt_equal "r1" (Some kv1) k1';
      Reference.update reference r2 kv2 >>= fun ()  ->
      Reference.read   reference r2     >>= fun k2' ->
      assert_key_opt_equal "r2" (Some kv2) k2';
      Reference.update reference r1 kv2 >>= fun ()   ->
      Reference.read   reference r1     >>= fun k2'' ->
      assert_key_opt_equal "r1-after-update" (Some kv2) k2'';
      Reference.list   reference r1     >>= fun ts ->
      assert_references_equal "list" [r1; r2] ts;
      Reference.remove reference r1     >>= fun () ->
      Reference.read   reference r1     >>= fun empty ->
      assert_key_opt_equal "empty" None empty;
      Reference.list   reference r1     >>= fun r2' ->
      assert_references_equal "all-after-remove" [r2] r2';
      return_unit
    in
    run x test

  let test_stores x () =
    let test () =
      create () >>= fun t          ->
      mk t >>= function { v1; v2 } ->

      update t ["a";"b"] v1 >>= fun ()  ->

      mem t ["a";"b"]       >>= fun b1  ->
      assert_bool_equal "mem1" true b1;
      mem t ["a"]           >>= fun b2  ->
      assert_bool_equal "mem2" false b2;
      read_exn t ["a";"b"]  >>= fun v1' ->
      assert_blob_equal "v1.1" v1 v1';
      snapshot t            >>= fun r1  ->

      update t ["a";"c"] v2 >>= fun ()  ->
      mem t ["a";"b"]       >>= fun b1  ->
      assert_bool_equal "mem3" true b1;
      mem t ["a"]           >>= fun b2  ->
      assert_bool_equal "mem4" false b2;
      read_exn t ["a";"b"]  >>= fun v1' ->
      assert_blob_equal "v1.1" v1 v1';
      mem t ["a";"c"]       >>= fun b1  ->
      assert_bool_equal "mem5" true b1;
      read_exn t ["a";"c"]  >>= fun v2' ->
      assert_blob_equal "v1.1" v2 v2';

      remove t ["a";"b"]    >>= fun ()  ->
      read t ["a";"b"]      >>= fun v1''->
      assert_blob_opt_equal "v1.2" None v1'';
      revert t r1           >>= fun ()  ->
      read t ["a";"b"]      >>= fun v1''->
      assert_blob_opt_equal "v1.3" (Some v1) v1'';
      list t ["a"]          >>= fun ks  ->
      assert_paths_equal "path" [["a";"b"]] ks;
      return_unit
    in
    run x test

  let test_sync x () =
    let test () =
      create () >>= fun t1          ->
      mk t1 >>= function { v1; v2 } ->

      update t1 ["a";"b"] v1 >>= fun () ->
      snapshot t1            >>= fun r1 ->
      update t1 ["a";"c"] v2 >>= fun () ->
      snapshot t1            >>= fun r2 ->
      update t1 ["a";"d"] v1 >>= fun () ->
      snapshot t1            >>= fun r3 ->
      output t1 "full"       >>= fun () ->
      export t1 [r3]         >>= fun partial ->
      export t1 []           >>= fun full    ->

      (* Restart a fresh store and import everything in there. *)
      x.clean ()             >>= fun () ->
      x.init ()              >>= fun () ->
      create ()              >>= fun t2 ->

      import t2 partial      >>= fun () ->
      revert t2 r3           >>= fun () ->
      output t2 "partial"    >>= fun () ->

      mem t2 ["a";"b"]       >>= fun b1 ->
      assert_bool_equal "mem-ab" true b1;

      mem t2 ["a";"c"]       >>= fun b2 ->
      assert_bool_equal "mem-ac" true b2;

      mem t2 ["a";"d"]       >>= fun b3  ->
      assert_bool_equal "mem-ad" true b3;
      read_exn t2 ["a";"d"]  >>= fun v1' ->
      assert_blob_equal "v1" v1' v1;

      catch
        (fun () ->
           revert t2 r2      >>= fun () ->
           OUnit.assert_bool "revert" false;
           return_unit)
        (fun e ->
           import t2 full    >>= fun () ->
           revert t2 r2      >>= fun () ->
           mem t2 ["a";"d"]  >>= fun b4 ->
           assert_bool_equal "mem-ab" false b4;
           return_unit)
    in
    run x test

end

let suite (speed, x) =
  let (module S) = x.store in
  let module T = Make(S) in
  x.name,
  [
    "Basic operations on blobs"       , speed, T.test_blobs     x;
    "Basic operations on trees"       , speed, T.test_trees      x;
    "Basic operations on commits"     , speed, T.test_commits    x;
    "Basic operations on references"  , speed, T.test_references x;
    "High-level store operations"     , speed, T.test_stores     x;
    "High-level store synchronisation", speed, T.test_sync       x;
  ]

let run name tl =
  let tl = List.map suite tl in
  Alcotest.run name tl
