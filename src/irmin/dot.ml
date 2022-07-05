(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Printf
open Astring

let src = Logs.Src.create "irmin.dot" ~doc:"Irmin dot graph output"

module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig
  type db

  val output_buffer :
    db ->
    ?html:bool ->
    ?depth:int ->
    ?full:bool ->
    date:(int64 -> string) ->
    Buffer.t ->
    unit Lwt.t
end

exception Utf8_failure

let is_valid_utf8 str =
  try
    Uutf.String.fold_utf_8
      (fun _ _ -> function `Malformed _ -> raise Utf8_failure | _ -> ())
      () str;
    true
  with Utf8_failure -> false

module Make (S : Store.Generic_key.S) = struct
  type db = S.t

  module Branch = S.Backend.Branch
  module Contents = S.Backend.Contents
  module Node = S.Backend.Node
  module Commit = S.Backend.Commit
  module Slice = S.Backend.Slice

  module Graph =
    Object_graph.Make (Contents.Hash) (Node.Hash) (Commit.Hash) (Branch.Key)

  module Info = S.Info

  let pp_author = Type.pp Info.author_t
  let pp_message = Type.pp Info.message_t

  let fprintf (t : db) ?depth ?(html = false) ?full ~date name =
    [%log.debug
      "depth=%s html=%b full=%s"
        (match depth with None -> "<none>" | Some d -> string_of_int d)
        html
        (match full with None -> "<none>" | Some b -> string_of_bool b)];
    let* slice = S.Repo.export ?full ?depth (S.repo t) in
    let vertex = Hashtbl.create 102 in
    let add_vertex v l = Hashtbl.add vertex v l in
    let mem_vertex v = Hashtbl.mem vertex v in
    let edges = ref [] in
    let add_edge v1 l v2 =
      if mem_vertex v1 && mem_vertex v2 then edges := (v1, l, v2) :: !edges
    in
    let string_of_hash t k =
      let s = Type.to_string t k in
      if String.length s <= 8 then s else String.with_range s ~len:8
    in
    let string_of_contents s =
      let s =
        if String.length s <= 10 then s else String.with_range s ~len:10
      in
      let s = if is_valid_utf8 s then s else "<blob>" in
      s
    in
    let label_of_node k _ =
      let s =
        (if html then
         sprintf "<div class='node'><div class='sha1'>%s</div></div>"
        else fun x -> x)
          (string_of_hash Node.Hash.t k)
      in
      `Label s
    in
    let label_of_step l =
      let l = Type.to_string S.Path.step_t l in
      let s =
        (if html then sprintf "<div class='path'>%s</div>" else fun x -> x)
          (string_of_contents l)
      in
      `Label s
    in
    let label_of_commit k c =
      let k = string_of_hash Commit.Hash.t k in
      let o = Commit.Val.info c in
      let s =
        if html then
          let message = Fmt.to_to_string pp_message (Info.message o) in
          Fmt.str
            "<div class='commit'>\n\
            \  <div class='sha1'>%s</div>\n\
            \  <div class='author'>%a</div>\n\
            \  <div class='date'>%s</div>\n\
            \  <div class='message'><pre>%s</pre></div>\n\
            \  <div>&nbsp</div>\n\
             </div>"
            k pp_author (Info.author o)
            (date (Info.date o))
            (String.Ascii.escape message)
        else sprintf "%s" k
      in
      `Label s
    in
    let label_of_contents k v =
      let k = string_of_hash Contents.Hash.t k in
      let s =
        if html then
          sprintf
            "<div class='contents'>\n\
            \  <div class='sha1'>%s</div>\n\
            \  <div>&nbsp</div>\n\
             </div>"
            k
        else
          let v = string_of_contents (Type.to_string Contents.Val.t v) in
          sprintf "%s (%s)" k (String.Ascii.escape_string v)
      in
      `Label s
    in
    let label_of_tag t =
      let s =
        if html then
          sprintf "<div class='tag'>%s</div>" (Type.to_string Branch.Key.t t)
        else Type.to_string Branch.Key.t t
      in
      `Label s
    in
    let contents = ref [] in
    let nodes = ref [] in
    let commits = ref [] in
    let* () =
      Slice.iter slice (function
        | `Contents c ->
            contents := c :: !contents;
            Lwt.return_unit
        | `Node n ->
            nodes := n :: !nodes;
            Lwt.return_unit
        | `Commit c ->
            commits := c :: !commits;
            Lwt.return_unit)
    in
    List.iter
      (fun (k, c) ->
        add_vertex (`Contents k) [ `Shape `Box; label_of_contents k c ])
      !contents;
    List.iter
      (fun (k, t) ->
        add_vertex (`Node k) [ `Shape `Box; `Style `Dotted; label_of_node k t ])
      !nodes;
    List.iter
      (fun (k, r) ->
        add_vertex (`Commit k)
          [ `Shape `Box; `Style `Bold; label_of_commit k r ])
      !commits;
    List.iter
      (fun (k, t) ->
        List.iter
          (fun (l, v) ->
            match v with
            | `Contents (v, _meta) ->
                let v = Contents.Key.to_hash v in
                add_edge (`Node k)
                  [ `Style `Dotted; label_of_step l ]
                  (`Contents v)
            | `Node n ->
                let n = Node.Key.to_hash n in
                add_edge (`Node k) [ `Style `Solid; label_of_step l ] (`Node n))
          (Node.Val.list t))
      !nodes;
    List.iter
      (fun (k, r) ->
        List.iter
          (fun c ->
            let c = Commit.Key.to_hash c in
            add_edge (`Commit k) [ `Style `Bold ] (`Commit c))
          (Commit.Val.parents r);
        let node_hash = Commit.Val.node r |> Node.Key.to_hash in
        add_edge (`Commit k) [ `Style `Dashed ] (`Node node_hash))
      !commits;
    let branch_t = S.Backend.Repo.branch_t (S.repo t) in
    let* bs = Branch.list branch_t in
    let+ () =
      Lwt_list.iter_s
        (fun r ->
          Branch.find branch_t r >|= function
          | None -> ()
          | Some k ->
              let k = Commit.Key.to_hash k in
              add_vertex (`Branch r)
                [ `Shape `Plaintext; label_of_tag r; `Style `Filled ];
              add_edge (`Branch r) [ `Style `Bold ] (`Commit k))
        bs
    in
    let vertex = Hashtbl.fold (fun k v acc -> (k, v) :: acc) vertex [] in
    fun ppf -> Graph.output ppf vertex !edges name

  let output_buffer t ?html ?depth ?full ~date buf =
    let+ fprintf = fprintf t ?depth ?full ?html ~date "graph" in
    let ppf = Format.formatter_of_buffer buf in
    fprintf ppf
end
