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

module Make (S: Irmin.S) = struct

  open S

  let cmp_list eq comp l1 l2 =
    cmp_list eq (List.sort comp l1) (List.sort comp l2)

  let assert_key_equal msg =
    line msg;
    OUnit.assert_equal ~msg ~cmp:Key.equal ~printer:Key.pretty

  let assert_key_opt_equal msg =
    line msg;
    OUnit.assert_equal
      ~msg ~cmp:(cmp_opt Key.equal) ~printer:(printer_opt Key.pretty)

  let assert_keys_equal msg =
    line msg;
    OUnit.assert_equal
      ~msg ~cmp:(cmp_list Key.equal Key.compare) ~printer:(printer_list Key.pretty)

  let assert_value_equal msg =
    line msg;
    OUnit.assert_equal ~msg ~cmp:Value.equal ~printer:Value.pretty

  let assert_value_opt_equal msg =
    line msg;
    OUnit.assert_equal ~msg
      ~cmp:(cmp_opt Value.equal) ~printer:(printer_opt Value.pretty)

  let assert_values_equal msg =
    line msg;
    OUnit.assert_equal ~msg
      ~cmp:(cmp_list Value.equal Value.compare) ~printer:(printer_list Value.pretty)

  let assert_tag_opt_equal msg =
    OUnit.assert_equal ~msg
      ~cmp:(cmp_opt Tag.equal) ~printer:(printer_opt Tag.pretty)

  let assert_tags_equal msg =
    OUnit.assert_equal ~msg
      ~cmp:(cmp_list Tag.equal Tag.compare) ~printer:(printer_list Tag.pretty)

  let v1 = Value.of_bytes "foo"
  let v2 = Value.of_bytes ""
  let k1 = Key.of_bytes (Value.dump v1)
  let k2 = Key.of_bytes (Value.dump v2)
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

  let suite name cleanup =
    name,
    [
      "Basic operations for values", test_values cleanup;
      "Basic operations for tags"  , test_tags   cleanup;
    ]

end
