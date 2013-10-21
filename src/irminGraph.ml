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

module type S = sig
  module Vertex: IrminBase.S
  include Graph.Sig.I with type V.t = Vertex.t
  include Graph.Oper.S with type g := t
  module Topological: sig
    val fold: (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  end
  val vertex: t -> vertex list
  val edges: t -> (vertex * vertex) list
  val make: vertex list
    -> ?sources:vertex list
    -> ?sinks:vertex list
    -> (vertex -> vertex list Lwt.t)
    -> t Lwt.t
  val output: t ->
    ?labels:(vertex * string) list ->
    ?overlay:(vertex * vertex) list ->
    string -> unit
  val min: t -> vertex list
  val max: t -> vertex list
  include IrminBase.S with type t := t
end

module Make (B: IrminBase.S) = struct

  let debug fmt = IrminLog.debug "GRAPH" fmt

  module Vertex = B
  module G = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)
  module GO = Graph.Oper.I(G)
  module Topological = Graph.Topological.Make(G)
  include G
  include GO

  let vertex g =
    G.fold_vertex (fun k set -> k :: set) g []

  let edges g =
    G.fold_edges (fun k1 k2 list -> (k1,k2) :: list) g []

  let make keys ?sources ?sinks pred =
    let keys = match sinks with
      | None      -> keys
      | Some keys -> keys in
    let g = G.create ~size:(List.length keys) () in
    let marks = Hashtbl.create 1024 in
    let mark key = Hashtbl.add marks key true in
    let has_mark key = Hashtbl.mem marks key in
    let () = match sources with
      | None      -> ()
      | Some keys ->
        List.iter mark keys;
        List.iter (G.add_vertex g) keys in
    let rec add key =
      if has_mark key then Lwt.return ()
      else (
        mark key;
        debug "ADD %s" (Vertex.pretty key);
        if not (G.mem_vertex g key) then G.add_vertex g key;
        lwt keys = pred key in
        List.iter (fun k -> G.add_edge g k key) keys;
        Lwt_list.iter_s add keys
      ) in
    lwt () = Lwt_list.iter_s add keys in
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
      let vertex_name k = Vertex.pretty k
      let vertex_attributes k = !vertex_attributes k
      let default_vertex_attributes _ = []
      let get_subgraph _ = None
      let graph_attributes _ =
        match !graph_name with
        | None   -> []
        | Some n -> [`Label n]
    end)

  let output g ?(labels=[]) ?(overlay=[]) name =
    if IrminLog.debug_enabled () then
      let g = G.copy g in
      List.iter (fun (v1,v2) -> G.add_edge g v1 v2) overlay;
      let eattrs e =
        if List.mem e overlay then [`Label "overlay"] else [] in
      let vattrs k =
        let tags = List.fold_left
            (fun accu (ki,ti) -> if k=ki then ti::accu else accu)
            [] labels in
        List.map (fun t -> `Label t) tags in
      vertex_attributes := vattrs;
      edge_attributes := eattrs;
      graph_name := Some name;
      Printf.eprintf "%!";
      Dot.output_graph stderr g;
      Printf.eprintf "\n\n%!"

  let mk vertex edges =
    let g = G.create ~size:(List.length vertex) () in
    List.iter (G.add_vertex g) vertex;
    List.iter (fun (v1, v2) -> G.add_edge g v1 v2) edges;
    g

  module XVertex = struct
    include IrminBase.List(Vertex)
    let name = "vertex"
  end
  module XEdges  = struct
    include IrminBase.List(IrminBase.Pair(Vertex)(Vertex))
    let name = "edges"
  end
  module XGraph = struct
    include IrminBase.Pair(XVertex)(XEdges)
    let name = "graph"
  end

  let name = XGraph.name

  let decompose t =
    let vertex = vertex t in
    let edges = edges t in
    vertex, edges

  let sizeof t =
    let g = decompose t in
    XGraph.sizeof g

  let set buf t =
    let g = decompose t in
    XGraph.set buf g

  let get buf =
    let vertex, edges = XGraph.get buf in
    mk vertex edges

  let pretty t =
    let vertex = Topological.fold (fun v acc -> v::acc) t [] in
    Printf.sprintf "%s" (String.concat "-" (List.map B.pretty vertex))

  let dump t =
    let g = decompose t in
    XGraph.dump g

  let of_json j =
    let vertex, edges = XGraph.of_json j in
    mk vertex edges

  let to_json t =
    let g = decompose t in
    XGraph.to_json g

  let hash = Hashtbl.hash

  let compare t1 t2 =
    let g1 = decompose t1 in
    let g2 = decompose t2 in
    XGraph.compare g1 g2

  let equal t1 t2 =
    compare t1 t2 = 0

end
