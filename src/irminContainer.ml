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

open IrminTypes

module Set (B: BASE) = struct

  module Set = Set.Make(B)

  include Set

  let debug fmt = IrminMisc.debug "SET" fmt

  module L = IrminIO.List(B)

  let of_list l =
    List.fold_left (fun acc elt -> Set.add elt acc) Set.empty l

  let to_list s =
    Set.elements s

  let hash = Hashtbl.hash

  let compare t1 t2 =
    L.compare (to_list t1) (to_list t2)

  let equal t1 t2 =
    compare t1 t2 = 0

  let pretty s =
    if Set.is_empty s then "{}"
    else
      "{ "^ String.concat ", " (List.map B.pretty (to_list s)) ^ " }"

  let to_json t =
    L.to_json (to_list t)

  let of_json j =
    of_list (L.of_json j)

  let sizeof t =
    debug "sizeof";
    L.sizeof (to_list t)

  let get buf =
    let l = L.get buf in
    of_list l

  let set buf t =
    L.set buf (to_list t)

end

module Graph (B: BASESET) = struct

  let debug fmt = IrminMisc.debug "GRAPH" fmt

  module Vertex = B
  module G = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)
  module GO = Graph.Oper.I(G)
  module Topological = Graph.Topological.Make(G)
  include G
  include GO

  let vertex g =
    G.fold_vertex (fun k set -> Vertex.Set.add k set) g Vertex.Set.empty

  let edges g =
    G.fold_edges (fun k1 k2 list -> (k1,k2) :: list) g []

  let make keys ?sources ?sinks pred =
    let keys = match sinks with
      | None      -> keys
      | Some keys -> keys in
    let g = G.create ~size:(B.Set.cardinal keys) () in
    let marks = Hashtbl.create 1024 in
    let mark key = Hashtbl.add marks key true in
    let has_mark key = Hashtbl.mem marks key in
    let () = match sources with
      | None      -> ()
      | Some keys ->
        Vertex.Set.iter mark keys;
        Vertex.Set.iter (G.add_vertex g) keys in
    let rec add key =
      if has_mark key then Lwt.return ()
      else (
        mark key;
        debug "ADD %s" (Vertex.pretty key);
        if not (G.mem_vertex g key) then G.add_vertex g key;
        lwt keys = pred key in
        List.iter (fun k -> G.add_edge g k key) (Vertex.Set.to_list keys);
        Lwt_list.iter_s add (Vertex.Set.to_list keys)
      ) in
    lwt () = Lwt_list.iter_s add (Vertex.Set.to_list keys) in
    Lwt.return g

  let attributes = ref (fun _ -> [])
  let graph_name = ref None
  module Dot = Graph.Graphviz.Dot(struct
      include G
      let edge_attributes _ = []
      let default_edge_attributes _ = []
      let vertex_name k = Vertex.pretty k
      let vertex_attributes k = !attributes k
      let default_vertex_attributes _ = []
      let get_subgraph _ = None
      let graph_attributes _ =
        match !graph_name with
        | None   -> []
        | Some n -> [`Label n]
    end)

  let dump g tags name =
    if IrminMisc.debug_enabled () then
      let attrs k =
        let tags = List.fold_left
            (fun accu (ki,ti) -> if k=ki then ti::accu else accu)
            [] tags in
        List.map (fun t -> `Label t) tags in
      attributes := attrs;
      graph_name := Some name;
      Printf.eprintf "%!";
      Dot.output_graph stderr g;
      Printf.eprintf "\n\n%!"

  let mk vertex edges =
    let g = G.create ~size:(Vertex.Set.cardinal vertex) () in
    Vertex.Set.iter (G.add_vertex g) vertex;
    List.iter (fun (v1, v2) -> G.add_edge g v1 v2) edges;
    g

  module VVL = IrminIO.List(IrminIO.Pair(Vertex)(Vertex))

  let sizeof t =
    let vertex = vertex t in
    let edges = edges t in
    Vertex.Set.sizeof vertex + VVL.sizeof edges

  let set buf t =
    let vertex = vertex t in
    let edges = edges t in
    Vertex.Set.set buf vertex;
    VVL.set buf edges

  let get buf =
    let vertex = Vertex.Set.get buf in
    let edges = VVL.get buf in
    mk vertex edges

  let pretty t =
    let vertex = Topological.fold (fun v acc -> v::acc) t [] in
    Printf.sprintf "%s" (String.concat "-" (List.map B.pretty vertex))

  let of_json _ =
    failwith "TODO"

  let to_json _ =
    failwith "TODO"

  let hash = Hashtbl.hash

  let equal = (=)

  let compare = compare

end
