(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Printf

module Log = Log.Make(struct let section ="DOT" end)

module type S = sig
  type db
  val output_buffer:
    db -> ?html:bool -> ?depth:int -> ?full:bool -> date:(int64 -> string) ->
    Buffer.t -> unit Lwt.t
end

module Make (S: Ir_s.STORE) = struct

  type db = S.t

  module Tag = S.Private.Tag
  module Contents = S.Private.Contents
  module Node = S.Private.Node
  module Commit = S.Private.Commit
  module Slice = S.Private.Slice
  module Graph = Ir_graph.Make(Contents.Key)(Node.Key)(Commit.Key)(Tag.Key)

  let fprintf (t:db) ?depth ?(html=false) ?full ~date name =
    Log.debugf "fprintf depth=%s html=%b full=%s"
      (match depth with None -> "<none>" | Some d -> string_of_int d)
      html
      (match full with None -> "<none>" | Some b -> string_of_bool b);
    S.export ?full ?depth t >>= fun slice ->
    let vertex = ref [] in
    let add_vertex v l =
      vertex := (v, l) :: !vertex in
    let edges = ref [] in
    let add_edge v1 l v2 =
      edges := (v1, l, v2) :: !edges in
    let string_of_key m k =
      let s = Tc.show m k in
      if String.length s <= 8 then s else String.sub s 0 8 in
    let string_of_contents s =
      let s =
        if String.length s <= 10 then s
        else String.sub s 0 10 in
      let s =
        if Ir_misc.is_valid_utf8 s then s
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
      let l = S.Key.Step.to_hum l in
      let s =
        (if html then
          sprintf "<div class='path'>%s</div>"
        else
          fun x -> x)
          (string_of_contents l) in
      `Label s in
    let label_of_commit k c =
      let k = string_of_key (module Commit.Key) k in
      let o = Commit.Val.task c in
      let s =
        (if html then
          sprintf
            "<div class='commit'>\n\
            \  <div class='sha1'>%s</div>\n\
            \  <div class='author'>%s</div>\n\
            \  <div class='date'>%s</div>\n\
            \  <div class='message'>%s</div>\n\
             </div>"
        else
          sprintf "%s %s %s %s")
          k
          (Ir_task.owner o)
          (date (Ir_task.date o))
          (String.concat "\n" (Ir_task.messages o))
      in
      `Label s in
    let label_of_contents k v =
      let k = string_of_key (module Contents.Key) k in
      let s =
        if html then
          sprintf "<div class='contents'>\n\
                  \  <div class='sha1'>%s</div>\n\
                  \  <div class='blob'><pre>%s</pre></div>\n\
                   </div>"
            k (Ezjsonm.to_string (S.Val.to_json v))
        else
           let v = string_of_contents (Tc.show (module S.Val) v) in
           sprintf "%s (%s)" k (String.escaped v) in
      `Label s in
    let label_of_tag t =
      let s =
        if html then
          sprintf "<div class='tag'>%s</div>"
            (Ezjsonm.to_string (Tag.Key.to_json t))
        else
          Tc.show (module Tag.Key) t
      in
      `Label s in
    Slice.iter_contents slice (fun (k, b) ->
        add_vertex (`Contents k) [`Shape `Box; label_of_contents k b];
        return_unit
      ) >>= fun () ->
    Slice.iter_nodes slice (fun (k, t) ->
        add_vertex (`Node k) [`Shape `Box; `Style `Dotted; label_of_node k t];
        Node.Val.iter_contents t (fun l v ->
            add_edge (`Node k) [`Style `Dotted; label_of_step l] (`Contents v)
          );
        Node.Val.iter_succ t (fun l n ->
            add_edge (`Node k) [`Style `Solid; label_of_step l] (`Node n)
          );
        return_unit
      ) >>= fun () ->
    Slice.iter_commits slice (fun (k, r) ->
        add_vertex (`Commit k) [`Shape `Box; `Style `Bold; label_of_commit k r];
        List.iter (fun c ->
            add_edge (`Commit k) [`Style `Bold] (`Commit c)
          ) (Commit.Val.parents r);
        match Commit.Val.node r with
        | None      -> return_unit
        | Some node ->
          add_edge (`Commit k) [`Style `Dashed] (`Node node);
          return_unit
      ) >>= fun () ->
    Slice.iter_tags slice (fun (r,k) ->
        add_vertex (`Tag r) [`Shape `Plaintext; label_of_tag r; `Style `Filled];
        add_edge (`Tag r) [`Style `Bold] (`Commit k);
        return_unit
      ) >>= fun () ->
    return (fun ppf -> Graph.output ppf !vertex !edges name)

  let output_buffer t ?html ?depth ?full ~date buf =
    fprintf t ?depth ?full ?html ~date "graph" >>= fun fprintf ->
    let ppf = Format.formatter_of_buffer buf in
    fprintf ppf;
    return_unit

end
