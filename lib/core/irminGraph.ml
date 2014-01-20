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

module type S = sig
  include Graph.Sig.I
  include Graph.Oper.S with type g := t
  module Topological: sig
    val fold: (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  end
  val vertex: t -> vertex list
  val edges: t -> (vertex * vertex) list
  val closure: (vertex -> vertex list Lwt.t)
    -> min:vertex list
    -> max:vertex list
    -> t Lwt.t
  val output:
    Format.formatter ->
    (vertex * Graph.Graphviz.DotAttributes.vertex list) list ->
    (vertex * Graph.Graphviz.DotAttributes.edge list * vertex) list ->
    string -> unit
  val min: t -> vertex list
  val max: t -> vertex list
  type dump = vertex list * (vertex * vertex) list
  val export: t -> dump
  val import: dump -> t
  module Dump: Identifiable.S with type t = dump
end

module Make (K: IrminKey.S) = struct

  open Lwt

  module L = Log.Make(struct let section = "GRAPH" end)

  module G = Graph.Imperative.Digraph.ConcreteBidirectional(K)
  module GO = Graph.Oper.I(G)
  module Topological = Graph.Topological.Make(G)
  include G
  include GO
  type dump = V.t list * (V.t * V.t) list

  let vertex g =
    G.fold_vertex (fun k set -> k :: set) g []

  let edges g =
    G.fold_edges (fun k1 k2 list -> (k1,k2) :: list) g []

  let closure pred ~min ~max =
    let g = G.create ~size:1024 () in
    let marks = K.Table.create () in
    let mark key = Hashtbl.add_exn marks key true in
    let has_mark key = Hashtbl.mem marks key in
    List.iter ~f:mark min;
    List.iter ~f:(G.add_vertex g) min;
    let rec add key =
      if has_mark key then Lwt.return ()
      else (
        mark key;
        L.debugf "ADD %s" (K.to_string key);
        if not (G.mem_vertex g key) then G.add_vertex g key;
        pred key >>= fun keys ->
        List.iter ~f:(fun k -> G.add_edge g k key) keys;
        Lwt_list.iter_p add keys
      ) in
    Lwt_list.iter_p add max >>= fun () ->
    Lwt.return g

  let min g =
    G.fold_vertex (fun v acc ->
        if G.in_degree g v = 0 then v :: acc
        else acc
      ) g []

  let max g =
    G.fold_vertex (fun v acc ->
        if G.out_degree g v = 0 then v :: acc
        else acc
      ) g []

  let vertex_attributes = ref (fun _ -> [])
  let edge_attributes = ref (fun _ -> [])
  let graph_name = ref None
  module Dot = Graph.Graphviz.Dot(struct
      include G
      let edge_attributes k = !edge_attributes k
      let default_edge_attributes _ = []
      let vertex_name k =
        Printf.sprintf "%S" (K.to_string k)
      let vertex_attributes k = !vertex_attributes k
      let default_vertex_attributes _ = []
      let get_subgraph _ = None
      let graph_attributes _ =
        match !graph_name with
        | None   -> []
        | Some n -> [`Label n]
    end)

  module Dump = struct
    module M = struct
      type nonrec t = K.t list * (K.t * K.t) list
      with bin_io, compare, sexp
      let hash (t : t) = Hashtbl.hash t
      include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
      let module_name = "Graph"
    end
    include M
    include Identifiable.Make (M)
  end

  let export t =
    vertex t, edges t

  let import (vs, es) =
    let g = G.create ~size:(List.length vs) () in
    List.iter ~f:(G.add_vertex g) vs;
    List.iter ~f:(fun (v1, v2) -> G.add_edge g v1 v2) es;
    g

  let output ppf vertex edges name =
    let g = G.create ~size:(List.length vertex) () in
    List.iter ~f:(fun (v,_) -> G.add_vertex g v) vertex;
    List.iter ~f:(fun (v1,_,v2) -> G.add_edge g v1 v2) edges;
    let eattrs (v1, v2) =
      try
        let l = List.filter ~f:(fun (x,_,y) -> x=v1 && y=v2) edges in
        let l = List.fold_left ~f:(fun acc (_,l,_) -> l @ acc) ~init:[] l in
        let labels, others =
          List.partition_map ~f:(function `Label l -> `Fst l | x -> `Snd x) l in
        match labels with
        | []  -> others
        | [l] -> `Label l :: others
        | _   -> `Label (String.concat ~sep:"," labels) :: others
      with Not_found -> [] in
    let vattrs v =
      try List.Assoc.find_exn vertex v
      with Not_found -> [] in
    vertex_attributes := vattrs;
    edge_attributes := eattrs;
    graph_name := Some name;
    Dot.fprint_graph ppf g

end
