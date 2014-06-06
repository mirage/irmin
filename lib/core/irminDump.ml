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

open Core_kernel.Std
open Lwt
open IrminMerge.OP

module Log = Log.Make(struct let section ="DUMP" end)

module type S = sig
  type t
  val output_file: t -> ?depth:int -> string ->  unit Lwt.t
  val output_buffer: t -> ?depth:int -> Buffer.t -> unit Lwt.t
end

module Make (Store: IrminBranch.STORE) = struct

  module K = Store.Block.Key
  module C = Store.Value
  module T = Store.Tag.Key

  module Tag = Store.Tag
  module Block = Store.Block
  module Commit = Block.Commit
  module Node = Block.Node
  module Contents = Block.Contents

  type t = Store.t

  let get_contents t = function
    | None  ->
      Contents.dump (Store.contents_t t) >>= fun contents ->
      Node.dump (Store.node_t t)         >>= fun nodes    ->
      Commit.dump (Store.commit_t t)     >>= fun commits  ->
      Tag.dump (Store.tag_t t)           >>= fun tags     ->
      return (contents, nodes, commits, tags)
    | Some depth ->
      Tag.dump (Store.tag_t t) >>= fun tags ->
      let max = List.map ~f:(fun (_,k) -> `Commit k) tags in
      let pred = function
        | `Commit k ->
          begin Commit.read (Store.commit_t t) k >>= function
            | None   -> return_nil
            | Some c -> return (IrminGraph.of_commits c.IrminCommit.parents)
          end
        | _ -> return_nil in
      Store.Graph.closure ~depth ~pred max >>= fun g ->
      let keys = IrminGraph.to_commits (Store.Graph.vertex g) in
      Lwt_list.map_p (fun k ->
          Commit.read_exn (Store.commit_t t) k >>= fun c ->
          return (k, c)
        ) keys
      >>= fun commits ->
      let root_nodes = List.filter_map ~f:(fun (_,c) -> c.IrminCommit.node) commits in
      Node.list (Store.node_t t) root_nodes >>= fun nodes ->
      Lwt_list.map_p (fun k ->
          Node.read (Store.node_t t) k >>= function
          | None   -> return_none
          | Some v -> return (Some (k, v))
        ) nodes >>= fun nodes ->
      let nodes = List.filter_map ~f:(fun x -> x) nodes in
      let root_contents = List.filter_map ~f:(fun (_,n) -> n.IrminNode.contents) nodes in
      Contents.list (Store.contents_t t) root_contents >>= fun contents ->
      Lwt_list.map_p (fun k ->
          Contents.read (Store.contents_t t) k >>= function
          | None   -> return_none
          | Some v -> return (Some (k, v))
        ) contents >>= fun contents ->
      let contents = List.filter_map ~f:(fun x -> x) contents in
      return (contents, nodes, commits, tags)

  let fprintf t ?depth ?(html=false) name =
    get_contents t depth >>= fun (contents, nodes, commits, tags) ->
    let vertex = ref [] in
    let add_vertex v l =
      vertex := (v, l) :: !vertex in
    let edges = ref [] in
    let add_edge v1 l v2 =
      edges := (v1, l, v2) :: !edges in
    let string_of_key k =
      let s = K.to_string k in
      if Int.(String.length s <= 8) then s else String.sub s 0 8 in
    let string_of_contents s =
      let s =
        if Int.(String.length s <= 10) then s
        else String.sub s 0 10 in
      let s =
        if IrminMisc.is_valid_utf8 s then s
        else IrminMisc.hex_encode s in
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
      let o = c.IrminCommit.origin in
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
          (IrminOrigin.id o)
          (IrminOrigin.(string_of_date (date o)))
          (IrminOrigin.message o) in
      `Label s in
    let label_of_contents k v =
      let k = string_of_key k in
      let s =
        if html then
          sprintf "<div class='contents'>\n\
                  \  <div class='sha1'>%s</div>\n\
                  \  <div class='blob'><pre>%s</pre></div>\n\
                   </div>"
            k (C.to_string v)
        else
           let v = string_of_contents (C.to_string v) in
           sprintf "%s | %s" k (String.escaped v) in
      `Label s in
    let label_of_tag t =
      let s =
        (if html then
           sprintf "<div class='tag'>%s</div>"
         else
           fun x -> x)
          (T.to_string t) in
      `Label s in
    let leafs = List.map ~f:(fun (k,_) ->
        (k, IrminNode.leaf k)
      ) contents in
    let nodes = leafs @ nodes in
    List.iter ~f:(fun (k, b) ->
        add_vertex (`Contents k) [`Shape `Record; label_of_contents k b];
      ) contents;
    List.iter ~f:(fun (k, t) ->
        add_vertex (`Node k) [`Shape `Box; `Style `Dotted; label_of_node k t];
        begin match t.IrminNode.contents with
          | None    -> ()
          | Some v  -> add_edge (`Node k) [`Style `Dotted] (`Contents v)
        end;
        String.Map.iter ~f:(fun ~key:l ~data:c ->
            add_edge (`Node k) [`Style `Solid; label_of_path l] (`Node c)
          ) t.IrminNode.succ
      ) nodes;
    List.iter ~f:(fun (k, r) ->
        add_vertex (`Commit k) [`Shape `Box; `Style `Bold; label_of_commit k r];
        List.iter ~f:(fun p ->
            add_edge (`Commit k) [`Style `Bold] (`Commit p)
          ) r.IrminCommit.parents;
        match r.IrminCommit.node with
        | None      -> ()
        | Some node -> add_edge (`Commit k) [`Style `Dashed] (`Node node)
      ) commits;
    List.iter ~f:(fun (r,k) ->
        add_vertex (`Tag r) [`Shape `Plaintext; label_of_tag r; `Style `Filled];
        let exists l = List.exists ~f:(fun (kk,_) -> K.(kk=k)) l in
        if exists commits then
          add_edge (`Tag r) [`Style `Bold] (`Commit k);
        if exists nodes then
          add_edge (`Tag r) [`Style `Bold] (`Node k);
      ) tags;
    return (fun ppf -> Store.Graph.output ppf !vertex !edges name)

  let output_file t ?depth name =
    Log.debugf "output %s" name;
    fprintf t ?depth name ~html:false >>= fun fprintf ->
    let oc = Out_channel.create (name ^ ".dot") in
    fprintf (Format.formatter_of_out_channel oc);
    Out_channel.close oc;
    let cmd = Printf.sprintf "dot -Tpng %s.dot -o%s.png" name name in
    (* XXX: this is not Xen-friendly *)
    let i = Sys.command cmd in
    if Int.(i <> 0) then Log.errorf "The %s.dot is corrupted" name;
    return_unit

  let output_buffer t ?depth buf =
    fprintf t ?depth "graph" ~html:true >>= fun fprintf ->
    let ppf = Format.formatter_of_buffer buf in
    fprintf ppf;
    return_unit

end
