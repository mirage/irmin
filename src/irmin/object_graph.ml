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

open! Import
include Object_graph_intf

let src = Logs.Src.create "irmin.graph" ~doc:"Irmin graph support"

module Log = (val Logs.src_log src : Logs.LOG)

let list_partition_map f t =
  let rec aux fst snd = function
    | [] -> (List.rev fst, List.rev snd)
    | h :: t -> (
        match f h with
        | `Fst x -> aux (x :: fst) snd t
        | `Snd x -> aux fst (x :: snd) t)
  in
  aux [] [] t

module Make (X : IO.S) (Hash : HASH) (Branch : Type.S) = struct
  module IO_list = IO.List (X)
  module IO = X
  open IO.Syntax

  module X = struct
    type t =
      [ `Contents of Hash.t
      | `Node of Hash.t
      | `Commit of Hash.t
      | `Branch of Branch.t ]
    [@@deriving irmin]

    let equal = Type.(unstage (equal t))
    let compare = Type.(unstage (compare t))
    let hash_branch = Type.(unstage (short_hash Branch.t))

    (* we are using cryptographic hashes here, so the first bytes
       are good enough to be used as short hashes. *)
    let hash (t : t) : int =
      match t with
      | `Contents c -> Hash.short_hash c
      | `Node n -> Hash.short_hash n
      | `Commit c -> Hash.short_hash c
      | `Branch b -> hash_branch b
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectional (X)
  module GO = Graph.Oper.I (G)
  module Topological = Graph.Topological.Make (G)

  module Table : sig
    type t

    val create : int option -> t
    val add : t -> X.t -> int -> unit
    val mem : t -> X.t -> bool
  end = struct
    module Lru = Lru.Make (X)
    module Tbl = Hashtbl.Make (X)

    type t = L of int Lru.t | T of int Tbl.t

    let create = function
      | None -> T (Tbl.create 1024)
      | Some n -> L (Lru.create n)

    let add t k v = match t with L t -> Lru.add t k v | T t -> Tbl.add t k v
    let mem t k = match t with L t -> Lru.mem t k | T t -> Tbl.mem t k
  end

  module Set = Set.Make (X)
  include G
  include GO

  type dump = vertex list * (vertex * vertex) list

  (* XXX: for the binary format, we can use offsets in the vertex list
     to save space. *)
  module Dump = struct
    type t = X.t list * (X.t * X.t) list [@@deriving irmin]
  end

  let vertex g = G.fold_vertex (fun k set -> k :: set) g []
  let edges g = G.fold_edges (fun k1 k2 list -> (k1, k2) :: list) g []
  let pp_vertices = Fmt.Dump.list (Type.pp X.t)
  let pp_depth ppf d = if d <> max_int then Fmt.pf ppf "depth=%d,@ " d

  type action = Visit of (X.t * int) | Treat of X.t

  let iter ?cache_size ?(depth = max_int) ~pred ~min ~max ~node ?edge ~skip ~rev
      () =
    Log.debug (fun f ->
        f "@[<2>iter:@ %arev=%b,@ min=%a,@ max=%a@, cache=%a@]" pp_depth depth
          rev pp_vertices min pp_vertices max
          Fmt.(Dump.option int)
          cache_size);
    let marks = Table.create cache_size in
    let mark key level = Table.add marks key level in
    let todo = Stack.create () in
    (* if a branch is in [min], add the commit it is pointing to too. *)
    let* min =
      IO_list.fold_left_s
        (fun acc -> function
          | `Branch _ as x ->
              let+ c = pred x in
              x :: c @ acc
          | x -> IO.return (x :: acc))
        [] min
    in
    let min = Set.of_list min in
    let has_mark key = Table.mem marks key in
    List.iter (fun k -> Stack.push (Visit (k, 0)) todo) max;
    let treat key =
      Log.debug (fun f -> f "TREAT %a" Type.(pp X.t) key);
      let* () = node key in
      if not (Set.mem key min) then
        (* the edge function is optional to prevent an unnecessary computation
           of the preds .*)
        match edge with
        | None -> IO.return ()
        | Some edge ->
            let* keys = pred key in
            IO_list.iter_p (fun k -> edge key k) keys
      else IO.return ()
    in
    let visit_predecessors ~filter_history key level =
      let+ keys = pred key in
      (*if a commit is in [min] cut the history but still visit
        its nodes. *)
      List.iter
        (function
          | `Commit _ when filter_history -> ()
          | k -> Stack.push (Visit (k, level + 1)) todo)
        keys
    in
    let visit key level =
      if level >= depth then IO.return ()
      else if has_mark key then IO.return ()
      else
        let* s = skip key in
        if s then IO.return ()
        else
          let+ () =
            Log.debug (fun f -> f "VISIT %a %d" Type.(pp X.t) key level);
            mark key level;
            if rev then Stack.push (Treat key) todo;
            match key with
            | `Commit _ ->
                visit_predecessors ~filter_history:(Set.mem key min) key level
            | _ ->
                if Set.mem key min then IO.return ()
                else visit_predecessors ~filter_history:false key level
          in
          if not rev then Stack.push (Treat key) todo
    in
    let rec pop () =
      match Stack.pop todo with
      | exception Stack.Empty -> IO.return ()
      | Treat key ->
          let* k = treat key in
          pop k
      | Visit (key, level) ->
          let* k = visit key level in
          pop k
    in
    pop ()

  let closure ?(depth = max_int) ~pred ~min ~max () =
    let g = G.create ~size:1024 () in
    List.iter (G.add_vertex g) max;
    let node key =
      if not (G.mem_vertex g key) then G.add_vertex g key else ();
      IO.return ()
    in
    let edge node pred =
      G.add_edge g pred node;
      IO.return ()
    in
    let skip _ = IO.return false in
    let+ () = iter ~depth ~pred ~min ~max ~node ~edge ~skip ~rev:false () in
    g

  let min g =
    G.fold_vertex
      (fun v acc -> if G.in_degree g v = 0 then v :: acc else acc)
      g []

  let max g =
    G.fold_vertex
      (fun v acc -> if G.out_degree g v = 0 then v :: acc else acc)
      g []

  let vertex_attributes = ref (fun _ -> [])
  let edge_attributes = ref (fun _ -> [])
  let graph_name = ref None

  module Dot = Graph.Graphviz.Dot (struct
    include G

    let edge_attributes k = !edge_attributes k
    let default_edge_attributes _ = []

    let vertex_name k =
      let str t v = "\"" ^ Type.to_string t v ^ "\"" in
      match k with
      | `Node n -> str Hash.t n
      | `Commit c -> str Hash.t c
      | `Contents c -> str Hash.t c
      | `Branch b -> str Branch.t b

    let vertex_attributes k = !vertex_attributes k
    let default_vertex_attributes _ = []
    let get_subgraph _ = None

    let graph_attributes _ =
      match !graph_name with None -> [] | Some n -> [ `Label n ]
  end)

  let export t = (vertex t, edges t)

  let import (vs, es) =
    let g = G.create ~size:(List.length vs) () in
    List.iter (G.add_vertex g) vs;
    List.iter (fun (v1, v2) -> G.add_edge g v1 v2) es;
    g

  let output ppf vertex edges name =
    Log.debug (fun f -> f "output %s" name);
    let g = G.create ~size:(List.length vertex) () in
    List.iter (fun (v, _) -> G.add_vertex g v) vertex;
    List.iter (fun (v1, _, v2) -> G.add_edge g v1 v2) edges;
    let eattrs (v1, v2) =
      try
        let l = List.filter (fun (x, _, y) -> x = v1 && y = v2) edges in
        let l = List.fold_left (fun acc (_, l, _) -> l @ acc) [] l in
        let labels, others =
          list_partition_map (function `Label l -> `Fst l | x -> `Snd x) l
        in
        match labels with
        | [] -> others
        | [ l ] -> `Label l :: others
        | _ -> `Label (String.concat "," labels) :: others
      with Not_found -> []
    in
    let vattrs v = try List.assoc v vertex with Not_found -> [] in
    vertex_attributes := vattrs;
    edge_attributes := eattrs;
    graph_name := Some name;
    Dot.fprint_graph ppf g
end
