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

let cmp_opt fn x y =
  match x, y with
  | Some x, Some y -> fn x y
  | None  , None   -> true
  | Some _, None
  | None  , Some _ -> false

let printer_opt fn = function
  | None   -> "<none>"
  | Some v -> fn v

let rec cmp_list fn x y =
  match x, y with
  | xh::xt, yh::yt -> fn xh yh && cmp_list fn xt yt
  | []    , []     -> true
  | _              -> false

let printer_list fn = function
  | [] -> "[]"
  | l  -> Printf.sprintf "[ %s ]" (String.concat ", " (List.map fn l))

let line msg =
  let line () =
    if IrminMisc.debug_enabled () then Alcotest.line stderr ~color:`Yellow '-' in
  line ();
  IrminMisc.info "ASSERT" "%s" msg;
  line ()

let assert_key_equal msg =
  line msg;
  OUnit.assert_equal ~msg ~cmp:Key.equal ~printer:Key.pretty

let assert_key_opt_equal msg =
  line msg;
  OUnit.assert_equal ~msg ~cmp:(cmp_opt Key.equal) ~printer:(printer_opt Key.pretty)

let assert_keys_equal msg =
  line msg;
  OUnit.assert_equal ~msg ~cmp:Key.Set.equal ~printer:Key.Set.pretty

let assert_keyl_equal msg =
  line msg;
  OUnit.assert_equal ~msg ~cmp:(cmp_list Key.equal) ~printer:(printer_list Key.pretty)

let assert_value_equal msg =
  line msg;
  OUnit.assert_equal ~msg ~cmp:Value.equal ~printer:Value.pretty

let assert_value_opt_equal msg =
  line msg;
  OUnit.assert_equal ~msg ~cmp:(cmp_opt Value.equal) ~printer:(printer_opt Value.pretty)

let assert_valuel_equal msg =
  line msg;
  OUnit.assert_equal ~msg
    ~cmp:(cmp_list Value.equal) ~printer:(printer_list Value.pretty)


let assert_tags_equal msg =
  OUnit.assert_equal ~msg ~cmp:Tag.Set.equal ~printer:Tag.Set.pretty

let test_db = "test-db"

let clean test_db =
  if Sys.file_exists test_db then
    let cmd = Printf.sprintf "rm -rf %s" test_db in
    let _ = Sys.command cmd in
    ()

let with_db test_db fn =
  clean test_db;
  lwt () = Disk.init test_db in
  let t = Disk.create test_db in
  try_lwt fn t
  with e ->
    raise_lwt e
