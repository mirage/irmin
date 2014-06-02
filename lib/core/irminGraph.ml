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

module Log = Log.Make(struct let section = "GRAPH" end)

module type S = sig
  include Graph.Sig.I
  include Graph.Oper.S with type g := t
  module Topological: sig
    val fold: (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  end
  val vertex: t -> vertex list
  val edges: t -> (vertex * vertex) list
  val closure:
    ?depth:int
    -> ?min:vertex list
    -> pred:(vertex -> vertex list Lwt.t)
    -> vertex list
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
  module Dump: IrminIdent.S with type t = dump
end

type ('a, 'b) vertex =
  [ `Contents of 'a
  | `Node of 'a
  | `Commit of 'a
  | `Tag of 'b ]
with bin_io, compare, sexp

let of_tags     = List.map ~f:(fun k -> `Tag k)
let of_contents = List.map ~f:(fun k -> `Contents k)
let of_nodes    = List.map ~f:(fun k -> `Node k)
let of_commits  = List.map ~f:(fun k -> `Commit k)

let to_tags     = List.filter_map ~f:(function `Tag k -> Some k | _ -> None)
let to_contents = List.filter_map ~f:(function `Contents k -> Some k | _ -> None)
let to_nodes    = List.filter_map ~f:(function `Node k -> Some k | _ -> None)
let to_commits  = List.filter_map ~f:(function `Commit k -> Some k | _ -> None)
let to_keys     = List.filter_map ~f:(function `Commit k
                                             | `Node k
                                             | `Contents k -> Some k
                                             | `Tag _      -> None)

module Make (K: IrminKey.S) (R: IrminTag.S) = struct

  open Lwt

  module K = struct
    module M = IrminIdent.Make(struct
        type t = (K.t, R.t) vertex
        with bin_io, compare, sexp
      end)
    include M
    let to_string = function
      | `Contents x -> "B" ^ K.to_string x
      | `Node     x -> "N" ^ K.to_string x
      | `Commit   x -> "C" ^ K.to_string x
      | `Tag      x -> "R" ^ R.to_string x
  end

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

  let closure ?(depth=Int.max_value) ?(min=[]) ~pred max =
    Log.debugf "closure depth=%d (%d elements)" depth (List.length max);
    let g = G.create ~size:1024 () in
    let marks = K.Table.create () in
    let mark key level = Hashtbl.add_exn marks key level in
    let has_mark key = Hashtbl.mem marks key in
    List.iter ~f:(fun k -> mark k Int.max_value) min;
    List.iter ~f:(G.add_vertex g) max;
    let todo = Queue.create () in
    List.iter ~f:(fun k -> Queue.enqueue todo (k,0)) max;
    let rec add () =
      match Queue.dequeue todo with
      | None              -> return_unit
      | Some (key, level) ->
        if Int.(level >= depth) then add ()
        else if has_mark key then add ()
        else (
          mark key level;
          Log.debugf "ADD %s %d" (K.to_string key) level;
          if not (G.mem_vertex g key) then G.add_vertex g key;
          pred key >>= fun keys ->
          List.iter ~f:(fun k -> G.add_edge g k key) keys;
          List.iter ~f:(fun k -> Queue.enqueue todo (k, level+1)) keys;
          add ()
      ) in
    add () >>= fun () ->
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

  module Dump = IrminIdent.Make(struct
      type nonrec t = K.t list * (K.t * K.t) list
      with bin_io, compare, sexp
    end)

  let export t =
    vertex t, edges t

  let import (vs, es) =
    let g = G.create ~size:(List.length vs) () in
    List.iter ~f:(G.add_vertex g) vs;
    List.iter ~f:(fun (v1, v2) -> G.add_edge g v1 v2) es;
    g

  let output ppf vertex edges name =
    Log.debugf "output %s" name;
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
