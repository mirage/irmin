(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix
open Printf
open Astring

let src = Logs.Src.create "irmin.dot" ~doc:"Irmin dot graph output"
module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig
  type db
  val output_buffer:
    db -> ?html:bool -> ?depth:int -> ?full:bool -> date:(int64 -> string) ->
    Buffer.t -> unit Lwt.t
end

exception Utf8_failure
let is_valid_utf8 str =
  try
    Uutf.String.fold_utf_8 (fun _ _ -> function
        | `Malformed _ -> raise (Utf8_failure)
        | _ -> ()
      ) () str;
    true
  with Utf8_failure -> false

module type PP = sig type t val pp: t Fmt.t end

module Make (S: S.STORE) = struct

  type db = S.t

  module Branch = S.Private.Branch
  module Contents = S.Private.Contents
  module Node = S.Private.Node
  module Commit = S.Private.Commit
  module Slice = S.Private.Slice
  module Graph = Object_graph.Make
      (Contents.Key)(Node.Metadata)(Node.Key)(Commit.Key)(Branch.Key)

  let fprintf (t:db) ?depth ?(html=false) ?full ~date name =
    Log.debug (fun f -> f "fprintf depth=%s html=%b full=%s"
      (match depth with None -> "<none>" | Some d -> string_of_int d)
      html
      (match full with None -> "<none>" | Some b -> string_of_bool b));
    S.Repo.export ?full ?depth (S.repo t) >>= fun slice ->
    let vertex = Hashtbl.create 102 in
    let add_vertex v l = Hashtbl.add vertex v l in
    let mem_vertex v = Hashtbl.mem vertex v in
    let edges = ref [] in
    let add_edge v1 l v2 =
      if mem_vertex v1 && mem_vertex v2 then edges := (v1, l, v2) :: !edges
    in
    let string_of_key (type t) (module M: PP with type t = t) (k:t) =
      let s = Fmt.to_to_string M.pp k in
      if String.length s <= 8 then s else String.with_range s ~len:8 in
    let string_of_contents s =
      let s =
        if String.length s <= 10 then s
        else String.with_range s ~len:10 in
      let s =
        if is_valid_utf8 s then s
        else (let `Hex s = Hex.of_string s in s) in
      s in
    let label_of_node k _ =
      let s =
        (if html then
           sprintf "<div class='node'><div class='sha1'>%s</div></div>"
         else
           fun x -> x)
          (string_of_key (module Node.Key) k) in
      `Label s in
    let label_of_step l =
      let l = Fmt.to_to_string S.Key.pp_step l in
      let s =
        (if html then
          sprintf "<div class='path'>%s</div>"
        else
          fun x -> x)
          (string_of_contents l) in
      `Label s in
    let label_of_commit k c =
      let k = string_of_key (module Commit.Key) k in
      let o = Commit.Val.info c in
      let s =
        if html then
          sprintf
            "<div class='commit'>\n\
            \  <div class='sha1'>%s</div>\n\
            \  <div class='author'>%s</div>\n\
            \  <div class='date'>%s</div>\n\
            \  <div class='message'><pre>%s</pre></div>\n\
            \  <div>&nbsp</div>\n\
             </div>"
            k
            (Info.author o)
            (date (Info.date o))
            (String.Ascii.escape (Info.message o))
        else
          sprintf "%s" k
      in
      `Label s in
    let label_of_contents k v =
      let k = string_of_key (module Contents.Key) k in
      let s =
        if html then
          sprintf "<div class='contents'>\n\
                  \  <div class='sha1'>%s</div>\n\
                  \  <div>&nbsp</div>\n\
                   </div>" k
        else
           let v = string_of_contents (Fmt.to_to_string Contents.Val.pp v) in
           sprintf "%s (%s)" k (String.Ascii.escape_string v) in
      `Label s in
    let label_of_tag t =
      let s =
        if html then
          sprintf "<div class='tag'>%s</div>" (Fmt.to_to_string Branch.Key.pp t)
        else
          Fmt.to_to_string Branch.Key.pp t
      in
      `Label s
    in
    let contents = ref [] in
    let nodes = ref [] in
    let commits = ref [] in
    Slice.iter slice (function
        | `Contents c -> contents := c :: !contents; Lwt.return_unit
        | `Node n     -> nodes := n :: !nodes; Lwt.return_unit
        | `Commit c   -> commits := c :: !commits; Lwt.return_unit
      ) >>= fun () ->
    List.iter (fun (k, c) ->
        add_vertex (`Contents k) [`Shape `Box; label_of_contents k c]
      ) !contents;
    List.iter (fun (k, t) ->
        add_vertex (`Node k) [`Shape `Box; `Style `Dotted; label_of_node k t]
      ) !nodes;
    List.iter (fun (k, r) ->
        add_vertex (`Commit k) [`Shape `Box; `Style `Bold; label_of_commit k r]
      ) !commits;
    List.iter (fun (k, t) ->
        List.iter (fun (l, v) -> match v with
            | `Contents (v, _meta) ->
              add_edge (`Node k) [`Style `Dotted; label_of_step l] (`Contents v)
            | `Node n ->
              add_edge (`Node k) [`Style `Solid; label_of_step l] (`Node n)
          ) (Node.Val.list t);
      ) !nodes;
    List.iter (fun (k, r) ->
        List.iter (fun c ->
            add_edge (`Commit k) [`Style `Bold] (`Commit c)
          ) (Commit.Val.parents r);
        add_edge (`Commit k) [`Style `Dashed] (`Node (Commit.Val.node r));
      ) !commits;
    let branch_t = S.Private.Repo.branch_t (S.repo t) in
    Branch.list branch_t >>= fun bs ->
    Lwt_list.iter_s (fun r ->
        Branch.find branch_t r >|= function
        | None   -> ()
        | Some k ->
          add_vertex (`Branch r) [`Shape `Plaintext; label_of_tag r; `Style `Filled];
          add_edge (`Branch r) [`Style `Bold] (`Commit k)
      ) bs >|= fun () ->
    let map = function
      | `Contents c -> `Contents (c, Node.Metadata.default)
      | `Commit _ | `Node _ | `Branch _ as k -> k
    in
    let vertex = Hashtbl.fold (fun k v acc -> (map k, v) :: acc) vertex [] in
    let edges = List.map (fun (k, l, v) -> map k, l, map v) !edges in
    fun ppf -> Graph.output ppf vertex edges name

  let output_buffer t ?html ?depth ?full ~date buf =
    fprintf t ?depth ?full ?html ~date "graph" >|= fun fprintf ->
    let ppf = Format.formatter_of_buffer buf in
    fprintf ppf

end
