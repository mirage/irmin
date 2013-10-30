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

let debug fmt =
  IrminLog.debug "TEST" fmt

module Make (S: Irmin.S) = struct

  open S

  let cmp_list eq comp l1 l2 =
    cmp_list eq (List.sort comp l1) (List.sort comp l2)

  let mk equal compare pretty =
    let aux cmp printer msg =
      line msg;
      OUnit.assert_equal ~msg ~cmp ~printer in
    aux equal pretty,
    aux (cmp_opt equal) (printer_opt pretty),
    aux (cmp_list equal compare) (printer_list pretty)


  let assert_key_equal, assert_key_opt_equal, assert_keys_equal =
    mk Key.equal Key.compare Key.pretty

  let assert_value_equal, assert_value_opt_equal, assert_values_equal =
    mk Value.equal Value.compare Value.pretty

  let assert_tag_equal, assert_tag_opt_equal, assert_tags_equal =
    mk Tag.equal Tag.compare Tag.pretty

  let assert_tree_equal, assert_tree_opt_equal, assert_trees_equal =
    mk Tree.equal Tree.compare Tree.pretty

  let assert_revision_equal, assert_revision_opt_equal, assert_revisions_equal =
    mk Revision.equal Revision.compare Revision.pretty

  (* XXX: move that into the library ? *)
  let key value =
    let buf = IrminBuffer.create (Value.sizeof value) in
    Value.set buf value;
    Key.of_ba (IrminBuffer.to_ba buf)

  let v1 = Value.of_bytes "foo"
  let v2 = Value.of_bytes ""
  let k1 = key v1
  let k2 = key v2
  let t1 = Tag.of_string "foo"
  let t2 = Tag.of_string "bar"

  let test_values cleanup () =
    let test =
      cleanup ()            >>= fun t    ->
      create ()             >>= fun t    ->
      init t                >>= fun ()   ->
      Value.add t.value v1  >>= fun k1'  ->
      Value.add t.value v1  >>= fun k1'' ->
      assert_key_equal "k1" k1 k1';
      assert_key_equal "k1" k1 k1'';
      Value.add t.value v2  >>= fun k2'  ->
      Value.add t.value v2  >>= fun k2'' ->
      assert_key_equal "k2" k2 k2';
      assert_key_equal "k2" k2 k2'';
      Value.read t.value k1 >>= fun v1'  ->
      assert_value_opt_equal "v1" (Some v1) v1';
      Value.read t.value k2 >>= fun v2'  ->
      assert_value_opt_equal "v2" (Some v2) v2';
      return_unit
    in
    Lwt_unix.run test

  let test_trees cleanup () =
    let test =
      cleanup () >>= fun () ->
      create ()  >>= fun t  ->
      init t     >>= fun () ->
      (* Create a node containing t1(v1) *)
      Tree.tree t.tree ~value:v1 [] >>= fun k1  ->
      Tree.tree t.tree ~value:v1 [] >>= fun k1' ->
      assert_key_equal "k1.1" k1 k1';
      Tree.read_exn t.tree k1       >>= fun t1  ->
      Tree.add t.tree t1            >>= fun k1''->
      assert_key_equal "k1.2" k1 k1'';

      (* Create the tree  t2 -b-> t1(v1) *)
      Tree.tree t.tree ["b", t1]    >>= fun k2  ->
      Tree.tree t.tree ["b", t1]    >>= fun k2' ->
      assert_key_equal "k2.1" k2 k2';
      Tree.read_exn t.tree k2       >>= fun t2  ->
      Tree.add t.tree t2            >>= fun k2''->
      assert_key_equal "k2.2" k2 k2'';
      Tree.sub_exn t.tree t2 ["b"]  >>= fun t1' ->
      assert_tree_equal "t1.1" t1 t1';
      Tree.add t.tree t1'           >>= fun k1''->
      assert_key_equal "k1.3" k1 k1'';

      (* Create the tree t3 -a-> t2 -b-> t1(v1) *)
      Tree.tree t.tree ["a", t2]    >>= fun k3  ->
      Tree.tree t.tree ["a", t2]    >>= fun k3' ->
      assert_key_equal "k3.1" k3 k3';
      Tree.read_exn t.tree k3       >>= fun t3  ->
      Tree.add t.tree t3            >>= fun k3''->
      assert_key_equal "k3.2" k3 k3'';
      Tree.sub_exn t.tree t3 ["a"]  >>= fun t2' ->
      assert_tree_equal "t2.1" t2 t2';
      Tree.add t.tree t2'           >>= fun k2''->
      assert_key_equal "k2.3" k2 k2'';
      Tree.sub_exn t.tree t2' ["b"]     >>= fun t1' ->
      assert_tree_equal "t1.2" t1 t1';
      Tree.sub t.tree t3 ["a";"b"]  >>= fun t1' ->
      assert_tree_opt_equal "t1.3" (Some t1) t1';

      Tree.find t.tree t1 []        >>= fun v11 ->
      assert_value_opt_equal "v1.1" (Some v1) v11;
      Tree.find t.tree t2 ["b"]     >>= fun v12 ->
      assert_value_opt_equal "v1.2" (Some v1) v12;
      Tree.find t.tree t3 ["a";"b"] >>= fun v13 ->
      assert_value_opt_equal "v1" (Some v1) v13;

      return_unit
    in
    Lwt_unix.run test

  let test_revisions cleanup () =
    let test =
      cleanup () >>= fun ()  ->
      create ()  >>= fun t   ->
      init t     >>= fun ()  ->

      (* t3 -a-> t2 -b-> t1(v1) *)
      Tree.tree t.tree ~value:v1 [] >>= fun k1  ->
      Tree.read_exn t.tree k1       >>= fun t1  ->
      Tree.tree t.tree ["a", t1]    >>= fun k2  ->
      Tree.read_exn t.tree k2       >>= fun t2  ->
      Tree.tree t.tree ["b", t2]    >>= fun k3  ->
      Tree.read_exn t.tree k3       >>= fun t3  ->

      (* r1 : t2 *)
      Revision.revision t.revision ~tree:t2 [] >>= fun kr1 ->
      Revision.revision t.revision ~tree:t2 [] >>= fun kr1'->
      assert_key_equal "kr1" kr1 kr1';
      Revision.read_exn t.revision kr1         >>= fun r1  ->

      (* r1 -> r2 : t3 *)
      Revision.revision t.revision ~tree:t3 [r1] >>= fun kr2  ->
      Revision.revision t.revision ~tree:t3 [r1] >>= fun kr2' ->
      assert_key_equal "kr2" kr2 kr2';
      Revision.read_exn t.revision kr2           >>= fun r2   ->

      Revision.cut t.revision [kr1] >>= fun g1 ->
      assert_revisions_equal "g1" [r1] (Revision.Graph.vertex g1);

      Revision.cut t.revision [kr2] >>= fun g2 ->
      assert_revisions_equal "g2" [r1; r2] (Revision.Graph.vertex g2);

     return_unit
    in
    Lwt_unix.run test

  let test_tags cleanup () =
    let test =
      cleanup ()             >>= fun ()  ->
      create ()              >>= fun t   ->
      init t                 >>= fun ()  ->
      Tag.update t.tag t1 k1 >>= fun ()  ->
      Tag.read   t.tag t1    >>= fun k1' ->
      assert_key_opt_equal "t1" (Some k1) k1';
      Tag.update t.tag t2 k2 >>= fun ()  ->
      Tag.read   t.tag t2    >>= fun k2' ->
      assert_key_opt_equal "t2" (Some k2) k2';
      Tag.update t.tag t1 k2 >>= fun ()   ->
      Tag.read   t.tag t1    >>= fun k2'' ->
      assert_key_opt_equal "t1-after-update" (Some k2) k2'';
      Tag.list   t.tag t1    >>= fun ts ->
      assert_tags_equal "list" [t1; t2] ts;
      Tag.remove t.tag t1    >>= fun () ->
      Tag.read   t.tag t1    >>= fun empty ->
      assert_key_opt_equal "empty" None empty;
      Tag.list   t.tag t1    >>= fun t2' ->
      assert_tags_equal "all-after-remove" [t2] t2';
      return_unit
    in
    Lwt_unix.run test

  let test_stores cleanup () =
    let test =
      cleanup ()            >>= fun ()  ->
      create ()             >>= fun t   ->
      init t                >>= fun ()  ->
      update t ["a";"b"] v1 >>= fun ()  ->
      read_exn t ["a";"b"]  >>= fun v1' ->
      assert_value_equal "v1.1" v1 v1';
      snapshot t            >>= fun r1  ->
      remove t ["a";"b"]    >>= fun ()  ->
      read t ["a";"b"]      >>= fun v1''->
      assert_value_opt_equal "v1.2" None v1'';
      revert t r1           >>= fun ()  ->
      read t ["a";"b"]      >>= fun v1''->
      assert_value_opt_equal "v1.3" (Some v1) v1'';
      return_unit
    in
    Lwt_unix.run test

  let suite name cleanup =
    name,
    [
      "Basic operations on values"   , test_values    cleanup;
      "Basic operations on trees"    , test_trees     cleanup;
      "Basic operations on revisions", test_revisions cleanup;
      "Basic operations on tags"     , test_tags      cleanup;
      "High-level store operations"  , test_stores    cleanup;
    ]

end
