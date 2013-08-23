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

open IrminLwt
open OUnit
open Test_common

let test_keys () =
  let k1 = Value.key (Value.blob "foo") in
  let k2 = Value.key (Value.blob "") in
  let k1s = Key.Set.singleton k1 in
  let k2s = Key.Set.singleton k2 in
  let module KV = Disk.Key_store in
  let test t =
    lwt () = KV.add t k1 k2s in
    lwt k1s' = KV.succ t k2 in
    lwt k2s' = KV.pred t k1 in
    lwt ks = KV.all t in
    assert_keys_equal "k1" k1s k1s';
    assert_keys_equal "k2" k2s k2s';
    assert_keys_equal "list" ks (Key.Set.union k1s k2s);
    Lwt.return ()
  in
  Lwt_unix.run (with_db test_db test)

let test_values () =
  let v1 = Value.blob "foo" in
  let v2 = Value.blob "" in
  let module DV = Disk.Value_store in
  let test t =
    lwt k1 = DV.write t v1 in
    lwt k1' = DV.write t v1 in
    lwt k2 = DV.write t v2 in
    lwt k2' = DV.write t v2 in
    lwt v1' = DV.read t k1 in
    lwt v2' = DV.read t k2 in
    assert_key_equal "k1" k1 k1';
    assert_key_equal "k2" k2 k2';
    assert_value_opt_equal "v1" (Some v1) v1';
    assert_value_opt_equal "v2" (Some v2) v2';
    Lwt.return ()
  in
  Lwt_unix.run (with_db test_db test)

let test_tags () =
  let k1 = Value.key (Value.blob "foo") in
  let k2 = Value.key (Value.blob "") in
  let t1 = Tag.of_name "foo" in
  let t2 = Tag.of_name "bar" in
  let module KT = Disk.Tag_store in
  let test t =
    lwt () = KT.update t t1 k1 in
    lwt () = KT.update t t2 k2 in
    lwt k1' = KT.read t t1 in
    lwt k2' = KT.read t t2 in
    assert_key_opt_equal "t1" (Some k1) k1';
    assert_key_opt_equal "t2" (Some k2) k2';
    lwt () = KT.update t t1 k2 in
    lwt k2'' = KT.read t t1 in
    assert_key_opt_equal "t1-after-update" (Some k2) k2'';
    lwt set = KT.all t in
    assert_tags_equal "all" set (Tag.Set.of_list [t1; t2]);
    lwt () = KT.remove t t1 in
    lwt none = KT.read t t1 in
    assert_key_opt_equal "remove" None none;
    lwt set = KT.all t in
    assert_tags_equal "all-after-remove" set (Tag.Set.singleton t2);
    Lwt.return ()
  in
  Lwt_unix.run (with_db test_db test)

let suite =
  "DISK" >:::
    [
      "Basic disk operations for values" >:: test_values;
      "Basic disk operations for keys"   >:: test_keys;
      "Basic disk operations for tags"   >:: test_tags;
    ]

let () =
  run_tests suite
