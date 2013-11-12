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
    if IrminLog.debug_enabled () then Alcotest.line stderr ~color:`Yellow '-' in
  line ();
  IrminLog.info "ASSERT" "%s" msg;
  line ()

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

  let assert_path_equal, assert_path_opt_equal, assert_paths_equal =
    mk (=) compare (String.concat "/")

  (* XXX: move that into the library ? *)
  let key value =
    let buf = IrminBuffer.create (Value.sizeof value) in
    Value.set buf value;
    Key.of_ba (IrminBuffer.to_ba buf)

end
