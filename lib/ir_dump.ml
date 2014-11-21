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

module Log = Log.Make(struct let section ="DUMP" end)

module type OF_STORE = sig
  type db
  type origin
  val output_buffer: db -> origin -> ?html:bool -> ?depth:int -> ?full:bool ->
    Buffer.t -> unit Lwt.t
end

module Make (S: Ir_bc.STORE_EXT) = struct

  type db = S.t
  type origin = S.origin

  module T = S.Tag
  module B = S.Block
  module StepMap = Ir_misc.Map(B.Node.Step)

  let fprintf t origin ?depth ?(html=false) ?full name =
    Log.debugf "fprintf depth=%s html=%b full=%s"
      (match depth with None -> "<none>" | Some d -> string_of_int d)
      html
      (match full with None -> "<none>" | Some b -> string_of_bool b);
    S.export ?full ?depth t origin >>= fun slice ->
    let contents = S.slice_contents slice in
    let nodes = S.slice_nodes slice in
    let commits = S.slice_commits slice in
    let tags = S.slice_tags slice in
    let exists k l = List.exists (fun (kk,_) -> kk=k) l in
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
          (string_of_key (module B.Node.Key) k) in
      `Label s in
    let label_of_path l =
      let l = Tc.write_string (module B.Step) l in
      let s =
        (if html then
          sprintf "<div class='path'>%s</div>"
        else
          fun x -> x)
          (string_of_contents l) in
      `Label s in
    let label_of_commit k c =
      let k = string_of_key (module B.Commit.Key) k in
      let o = B.Commit.Val.origin c in
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
          sprintf "%s | %s | %s | %s")
          k
          (B.Origin.id o)
          (B.Origin.pretty_date o)
          (B.Origin.message o)
      in
      `Label s in
    let label_of_contents k v =
      let k = string_of_key (module B.Contents.Key) k in
      let s =
        if html then
          sprintf "<div class='contents'>\n\
                  \  <div class='sha1'>%s</div>\n\
                  \  <div class='blob'><pre>%s</pre></div>\n\
                   </div>"
            k (Ezjsonm.to_string (B.Contents.Val.to_json v))
        else
           let v = string_of_contents (Tc.show (module B.Contents.Val) v) in
           sprintf "%s | %s" k (String.escaped v) in
      `Label s in
    let label_of_tag t =
      let s =
        if html then
          sprintf "<div class='tag'>%s</div>"
            (Ezjsonm.to_string (T.Key.to_json t))
        else
          Tc.show (module T.Key) t
      in
      `Label s in
    List.iter (fun (k, b) ->
        add_vertex (`Contents k) [`Shape `Record; label_of_contents k b];
      ) contents;
    List.iter (fun (k, t) ->
        add_vertex (`Node k) [`Shape `Box; `Style `Dotted; label_of_node k t];
        begin match B.Node.Val.contents t with
          | None    -> ()
          | Some v  ->
            if exists v contents then
              add_edge (`Node k) [`Style `Dotted] (`Contents v)
        end;
        StepMap.iter (fun l n ->
            if exists n nodes then
              add_edge (`Node k) [`Style `Solid; label_of_path l] (`Node n)
          ) (B.Node.Val.succ t)
      ) nodes;
    List.iter (fun (k, r) ->
        add_vertex (`Commit k) [`Shape `Box; `Style `Bold; label_of_commit k r];
        List.iter (fun c ->
            if exists c commits then
              add_edge (`Commit k) [`Style `Bold] (`Commit c)
          ) (B.Commit.Val.parents r);
        match B.Commit.Val.node r with
        | None      -> ()
        | Some node ->
          if exists node nodes then
            add_edge (`Commit k) [`Style `Dashed] (`Node node)
      ) commits;
    List.iter (fun (r,k) ->
        add_vertex (`Tag r) [`Shape `Plaintext; label_of_tag r; `Style `Filled];
        if exists k commits then add_edge (`Tag r) [`Style `Bold] (`Commit k);
      ) tags;
    return (fun ppf -> S.Graph.output ppf !vertex !edges name)

  let output_buffer t origin ?html ?depth ?full buf =
    fprintf t origin ?depth ?full ?html "graph" >>= fun fprintf ->
    let ppf = Format.formatter_of_buffer buf in
    fprintf ppf;
    return_unit

end
