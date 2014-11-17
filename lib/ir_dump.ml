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

module type S = sig
  type t
  val output_buffer: t -> ?depth:int -> ?full:bool -> Buffer.t -> unit Lwt.t
end

module Make (B: Ir_block.STORE) (T: Ir_tag.STORE with type value = B.key) = struct

  module S = Ir_bc.Make(B)(T)

  type t = S.t

  let get_contents t ?(full=false) = function
    | None  ->
      T.dump (S.tag_t t)           >>= fun tags     ->
      B.Commit.dump (S.commit_t t) >>= fun commits  ->
      if not full then
        return ([], [], commits, tags)
      else
        B.Contents.dump (S.contents_t t) >>= fun contents ->
        B.Node.dump (S.node_t t)         >>= fun nodes    ->
        return (contents, nodes, commits, tags)
    | Some depth ->
      Log.debugf "get_contents depth=%d full=%b" depth full;
      T.dump (S.tag_t t) >>= fun tags ->
      let max = List.map (fun (_,k) -> `Commit k) tags in
      let pred = function
        | `Commit k ->
          begin B.Commit.read (S.commit_t t) k >>= function
            | None   -> return_nil
            | Some c -> return (Ir_graph.of_commits c.Ir_commit.parents)
          end
        | _ -> return_nil in
      S.Graph.closure ~depth ~pred max >>= fun g ->
      let keys = Ir_graph.to_commits (S.Graph.vertex g) in
      Lwt_list.map_p (fun k ->
          B.Commit.read_exn (S.commit_t t) k >>= fun c ->
          return (k, c)
        ) keys
      >>= fun commits ->
      if not full then
        return ([], [], commits, tags)
      else
        let root_nodes = Ir_misc.list_filter_map (fun (_,c) -> c.Ir_commit.node) commits in
        B.Node.list (S.node_t t) root_nodes >>= fun nodes ->
        Lwt_list.map_p (fun k ->
            B.Node.read (S.node_t t) k >>= function
            | None   -> return_none
            | Some v -> return (Some (k, v))
          ) nodes >>= fun nodes ->
        let nodes = Ir_misc.list_filter_map (fun x -> x) nodes in
        let root_contents = Ir_misc.list_filter_map (fun (_,n) -> n.Ir_node.contents) nodes in
        B.Contents.list (S.contents_t t) root_contents >>= fun contents ->
        Lwt_list.map_p (fun k ->
            B.Contents.read (S.contents_t t) k >>= function
            | None   -> return_none
            | Some v -> return (Some (k, v))
          ) contents >>= fun contents ->
        let contents = Ir_misc.list_filter_map (fun x -> x) contents in
        return (contents, nodes, commits, tags)

  let fprintf t ?depth ?(html=false) ?full name =
    Log.debugf "fprintf depth=%s html=%b full=%s"
      (match depth with None -> "<none>" | Some d -> string_of_int d)
      html
      (match full with None -> "<none>" | Some b -> string_of_bool b);
    get_contents t ?full depth >>= fun (contents, nodes, commits, tags) ->
    let exists k l = List.exists (fun (kk,_) -> kk=k) l in
    let vertex = ref [] in
    let add_vertex v l =
      vertex := (v, l) :: !vertex in
    let edges = ref [] in
    let add_edge v1 l v2 =
      edges := (v1, l, v2) :: !edges in
    let string_of_key k =
      let s = Tc.show (module B.Key) k in
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
          (string_of_key k) in
      `Label s in
    let label_of_path l =
      let s =
        (if html then
          sprintf "<div class='path'>%s</div>"
        else
          fun x -> x)
          (string_of_contents l) in
      `Label s in
    let label_of_commit k c =
      let k = string_of_key k in
      let o = c.Ir_commit.origin in
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
          (B.Origin.(string_of_date (date o)))
          (B.Origin.message o) in
      `Label s in
    let label_of_contents k v =
      let k = string_of_key k in
      let s =
        if html then
          sprintf "<div class='contents'>\n\
                  \  <div class='sha1'>%s</div>\n\
                  \  <div class='blob'><pre>%s</pre></div>\n\
                   </div>"
            k (Ezjsonm.to_string (B.Contents.Value.to_json v))
        else
           let v = string_of_contents (Tc.show (module B.Contents.Value) v) in
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
    let leafs = List.map (fun (k,_) -> k, Ir_node.leaf k) contents in
    let nodes = leafs @ nodes in
    List.iter (fun (k, b) ->
        add_vertex (`Contents k) [`Shape `Record; label_of_contents k b];
      ) contents;
    List.iter (fun (k, t) ->
        add_vertex (`Node k) [`Shape `Box; `Style `Dotted; label_of_node k t];
        begin match t.Ir_node.contents with
          | None    -> ()
          | Some v  ->
            if exists v contents then
              add_edge (`Node k) [`Style `Dotted] (`Contents v)
        end;
        Ir_misc.StringMap.iter (fun l n ->
            if exists n nodes then
              add_edge (`Node k) [`Style `Solid; label_of_path l] (`Node n)
          ) t.Ir_node.succ
      ) nodes;
    List.iter (fun (k, r) ->
        add_vertex (`Commit k) [`Shape `Box; `Style `Bold; label_of_commit k r];
        List.iter (fun c ->
            if exists c commits then
              add_edge (`Commit k) [`Style `Bold] (`Commit c)
          ) r.Ir_commit.parents;
        match r.Ir_commit.node with
        | None      -> ()
        | Some node ->
          if exists node nodes then
            add_edge (`Commit k) [`Style `Dashed] (`Node node)
      ) commits;
    List.iter (fun (r,k) ->
        add_vertex (`Tag r) [`Shape `Plaintext; label_of_tag r; `Style `Filled];
        if exists k commits then
          add_edge (`Tag r) [`Style `Bold] (`Commit k);
        if exists k nodes then
          add_edge (`Tag r) [`Style `Bold] (`Node k);
      ) tags;
    return (fun ppf -> S.Graph.output ppf !vertex !edges name)

  let output_buffer t ?depth ?full buf =
    fprintf t ?depth ?full ~html:true "graph" >>= fun fprintf ->
    let ppf = Format.formatter_of_buffer buf in
    fprintf ppf;
    return_unit

end
