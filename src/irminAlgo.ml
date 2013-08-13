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

module Make (K: IrminAPI.KEY) (T: IrminAPI.TAG) = struct

  (** Type of keys *)
  type key = K.t

  (** Graph of keys *)
  type graph = key list * (key * key) list

  (** Type of remote tags *)
  type tag = T.t

  (** Type of channel *)
  type channel = unit

  module Vertex = struct
    type t = key
    let compare k1 k2 = String.compare (K.to_string k1) (K.to_string k2)
    let hash k = Hashtbl.hash (K.to_string k)
    let equal k1 k2 = String.compare (K.to_string k1) (K.to_string k2) = 0
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)

  module Graph = struct

    include G

    let vertex g =
      G.fold_vertex (fun v acc -> v :: acc) g []

    let edges g =
      G.fold_edges (fun v1 v2 acc -> (v1, v2) :: acc) g []

    let to_string g =
      let buf = Buffer.create 124 in
      Printf.bprintf buf "digraph {\n";
      G.iter_vertex (fun v ->
          Printf.bprintf buf "%S;\n" (K.to_string v)
        ) g;
      G.iter_edges (fun v1 v2 ->
          Printf.bprintf buf "%S -> %S;\n" (K.to_string v1) (K.to_string v2)
        ) g;
      Printf.bprintf buf "}\n";
      Buffer.contents buf

    let of_graph (vertex, edges) =
      let g = G.create ~size:(List.length vertex) () in
      List.iter (G.add_vertex g) vertex;
      List.iter (fun (k1,k2) -> G.add_edge g k1 k2) edges

  end

  let pull_keys () _ =
    failwith "TODO"

  let pull_tags () =
    failwith "TODO"

  let push_keys () _ =
    failwith "TODO"

  let push_tags () _ =
    failwith "TODO"

  let watch () _ =
    failwith "TODO"

end
