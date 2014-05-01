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

open Core_kernel.Std

let () =
  Log.set_log_level Log.DEBUG;
  Log.color_on ();
  Log.set_output stderr

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
  | l  -> Printf.sprintf "[ %s ]"
            (String.concat ~sep:", " (List.map ~f:fn l))

let line msg =
  let line () = Alcotest.line stderr ~color:`Yellow '-' in
  line ();
  Log.infof "ASSERT %s" msg;
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

  module K = Internal.Key
  let assert_key_equal, assert_key_opt_equal, assert_keys_equal =
    mk K.equal K.compare K.to_string

  let assert_key_result_equal, assert_key_result_opt_equal, assert_key_results_equal =
    mk
      (IrminMerge.Result.equal K.equal)
      (IrminMerge.Result.compare K.compare)
      (IrminMerge.Result.to_string K.to_string)

  module Contents = Internal.Contents
  module B = Contents.Value
  let assert_contents_equal, assert_contents_opt_equal, assert_contentss_equal =
    mk B.equal B.compare B.to_string

  module R = Reference.Key
  let assert_reference_equal, assert_reference_opt_equal, assert_references_equal =
    mk R.equal R.compare R.to_string

  module Node = Internal.Node
  module T = Node.Value
  let assert_node_equal, assert_node_opt_equal, assert_nodes_equal =
    mk T.equal T.compare T.to_string

  module Commit = Internal.Commit
  module C = Commit.Value
  let assert_commit_equal, assert_commit_opt_equal, assert_commits_equal =
    mk C.equal C.compare C.to_string

  module P = IrminPath
  let assert_path_equal, assert_path_opt_equal, assert_paths_equal =
    mk P.equal P.compare P.to_string
  module V = Internal.Value

  let assert_bool_equal, assert_bool_opt_equal, assert_bools_equal =
    mk (=) compare string_of_bool

  let assert_string_equal, assert_string_opt_equal, assert_strings_equal =
    mk String.equal String.compare (fun x -> x)

  let contents t = Internal.contents (internal t)
  let node t = Internal.node (internal t)
  let commit t = Internal.commit (internal t)

  let assert_succ_equal msg s1 s2 =
    let l1, k1 = List.unzip s1 in
    let l2, k2 = List.unzip s2 in
    assert_strings_equal msg l1 l2;
    assert_nodes_equal msg k1 k2

end

open Lwt

let modules x: (module IrminKey.S) * (module IrminContents.S) * (module IrminReference.S) =
  match x with
  | `String -> (module IrminKey.SHA1), (module IrminContents.String), (module IrminReference.String)
  | `JSON   -> (module IrminKey.SHA1), (module IrminContents.JSON)  , (module IrminReference.String)

type t = {
  name : string;
  kind : [`JSON | `String];
  init : unit -> unit Lwt.t;
  clean: unit -> unit Lwt.t;
  store: (module Irmin.S);
}

let unit () =
  return_unit

let string_of_kind = function
  | `JSON   -> ".JSON"
  | `String -> ""
