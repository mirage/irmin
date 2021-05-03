(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

module Config = struct
  let entries = 2
  let stable_hash = 3
end

module Maker = Irmin_layers_pack.Maker (Config)

let test_dir = Filename.concat "_build" "test-db-layers"
let config = Irmin_pack.config ~fresh:true ~lru_size:0 test_dir
let clean () = Lwt.return ()
let store = Irmin_test.store (module Maker) (module Irmin.Metadata.None)

let init () =
  if Sys.file_exists test_dir then (
    let cmd = Printf.sprintf "rm -rf %s" test_dir in
    Fmt.epr "exec: %s\n%!" cmd;
    let _ = Sys.command cmd in
    ());
  Lwt.return_unit

let suite =
  { Irmin_test.name = "LAYERS"; init; clean; config; store; stats = None }

module S =
  Maker.Make (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.BLAKE2B)

module TL = Layered_store.Make_Layered (S)

let misc =
  let t = { suite with store = (module S : Irmin_test.S) } in
  let test f () = Lwt_main.run (init () >|= fun () -> f t ()) in
  [
    ( "misc",
      [
        ("Test commits and graphs", `Quick, test TL.test_graph_and_history);
        ("Update branches after freeze", `Quick, test TL.test_fail_branch);
        ("Test operations on set", `Quick, test TL.test_set);
        ("Test operations on set tree", `Quick, test TL.test_set_tree);
        ("Gc and tree operations", `Quick, test TL.test_gc);
        ( "Merge into deleted branch",
          `Quick,
          test TL.test_merge_into_deleted_branch );
        ( "Merge with deleted branch",
          `Quick,
          test TL.test_merge_with_deleted_branch );
        ("Freeze with squash", `Quick, test TL.test_squash);
        ("Branches with squash", `Quick, test TL.test_branch_squash);
        ("Consecutive freezes", `Quick, test TL.test_consecutive_freeze);
        ("Test find tree after freeze", `Quick, test TL.test_freeze_tree);
        ("Keep max and copy from upper", `Quick, test TL.test_copy_in_upper);
        ("Keep max and heads after max", `Quick, test TL.test_keep_heads);
        ("Test find during freeze", `Quick, test TL.test_find_during_freeze);
        ("Test add during freeze", `Quick, test TL.test_add_during_freeze);
        ("Adds again objects deleted by freeze", `Quick, test TL.test_add_again);
      ] );
  ]
