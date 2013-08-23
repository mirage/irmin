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

let test_init () =
  clean test_db;
  IrminQueue.init (`Dir test_db)

let test_add_and_peek () =
  let t = `Dir test_db in
  let v1 = Value.blob "foo" in
  let v2 = Value.blob "" in
  IrminQueue.add t [v1; v2];
  let v1' = IrminQueue.peek t in
  assert_value_equal "v1" v1 v1'

let suite =
  "QUEUE" >:::
    [
      "Init a queue"                           >:: test_init;
      "Add elements to the queue and peek one" >:: test_add_and_peek;
    ]

let () =
  run_tests suite
