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

open Irmin_unix

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
            (String.concat ", " (List.map fn l))

let line msg =
  let line () = Alcotest.line stderr ~color:`Yellow '-' in
  line ();
  Log.infof "ASSERT %s" msg;
  line ()

module Make (S: Irmin.S) = struct

  let cmp_list eq comp l1 l2 =
    cmp_list eq (List.sort comp l1) (List.sort comp l2)

  let mk equal compare pretty =
    let aux cmp printer msg =
      line msg;
      OUnit.assert_equal ~msg ~cmp ~printer in
    aux equal pretty,
    aux (cmp_opt equal) (printer_opt pretty),
    aux (cmp_list equal compare) (printer_list pretty)

  let assert_step_equal, assert_step_opt_equal, assert_steps_equal =
    mk S.Key.Step.equal S.Key.Step.compare S.Key.Step.to_hum

  module KB = S.Private.Contents.Key
  let assert_key_contents_equal, assert_key_contents_opt_equal, assert_keys_contents_equal =
    mk KB.equal KB.compare KB.to_hum

  module KN = S.Private.Node.Key
  let assert_key_node_equal, assert_key_node_opt_equal, assert_keys_node_equal =
    mk KN.equal KN.compare KN.to_hum

  module KC = S.Head
  let assert_key_commit_equal, assert_key_commit_opt_equal, assert_key_commits_equal =
    mk KC.equal KC.compare KC.to_hum

  module RB = Tc.App1(Irmin.Merge.Result)(KB)
  let assert_contents_result_equal, assert_contents_result_opt_equal,
      assert_contents_results_equal =
    mk RB.equal RB.compare (Tc.show (module RB))

  module RN = Tc.App1(Irmin.Merge.Result)(KN)
  let assert_node_result_equal, assert_node_result_opt_equal,
      assert_node_results_equal =
    mk RN.equal RN.compare (Tc.show (module RN))

  module RC = Tc.App1(Irmin.Merge.Result)(KC)
  let assert_commit_result_equal, assert_commit_result_opt_equal,
      assert_commit_results_equal =
    mk RC.equal RC.compare (Tc.show (module RC))

  module Contents = S.Private.Contents
  module B = Contents.Val
  let assert_contents_equal, assert_contents_opt_equal, assert_contentss_equal =
    mk B.equal B.compare (Tc.show (module B))

  module T = S.Tag
  let assert_tag_equal, assert_tag_opt_equal, assert_tags_equal =
    mk T.equal T.compare T.to_hum

  module Node = S.Private.Node
  module N = Node.Val
  let assert_node_equal, assert_node_opt_equal, assert_nodes_equal =
    mk N.equal N.compare (Tc.show (module N))

  module Commit = S.Private.Commit
  module C = Commit.Val
  let assert_commit_equal, assert_commit_opt_equal, assert_commits_equal =
    mk C.equal C.compare (Tc.show (module C))

  module P = S.Key
  let assert_path_equal, assert_path_opt_equal, assert_paths_equal =
    mk P.equal P.compare P.to_hum
  module V = S.Val

  let assert_bool_equal, assert_bool_opt_equal, assert_bools_equal =
    mk (=) compare string_of_bool

  let assert_string_equal, assert_string_opt_equal, assert_strings_equal =
    mk (=) String.compare (fun x -> x)

  let assert_succ_equal msg s1 s2 =
    let unzip l = List.map fst l, List.map snd l in
    let l1, k1 = unzip s1 in
    let l2, k2 = unzip s2 in
    assert_steps_equal msg l1 l2;
    assert_keys_node_equal msg k1 k2

end

open Lwt

module P = Irmin.Path.String
module K = Irmin.Hash.SHA1
module T = Irmin.Tag.String_list
module Store (F: Irmin.S_MAKER)(C: Irmin.Contents.S) =  F(P)(C)(T)(K)

let create: (module Irmin.S_MAKER) -> [`String | `Json] -> (module Irmin.S) =
  fun (module M) c ->
    let (module C: Irmin.Contents.S) = match c with
      | `String -> (module Irmin.Contents.String)
      | `Json   -> (module Irmin.Contents.Json)
    in
    let module X = Store(M)(C) in
    (module X)

type t = {
  name  : string;
  kind  : [`Json | `String];
  init  : unit -> unit Lwt.t;
  clean : unit -> unit Lwt.t;
  config: Irmin.config;
  store : (module Irmin.S);
}

let none () =
  return_unit

let string_of_kind = function
  | `Json   -> "-json"
  | `String -> ""

let mem_store = create (module Irmin_mem.Make)
let irf_store = create (module Irmin_fs.Make)
let http_store = create (module Irmin_http.Make)
let git_store = create (module Irmin_git.FS)
