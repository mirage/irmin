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

open Lwt.Infix

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

module type S = sig
  include Graph.Sig.I

  include Graph.Oper.S with type g := t

  module Topological : sig
    val fold : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  end

  val vertex : t -> vertex list

  val edges : t -> (vertex * vertex) list

  val closure :
    ?depth:int ->
    pred:(vertex -> vertex list Lwt.t) ->
    min:vertex list ->
    max:vertex list ->
    unit ->
    t Lwt.t

  val iter_track_visited :
    ?depth:int ->
    pred:(vertex -> vertex list Lwt.t) ->
    min:vertex list ->
    max:vertex list ->
    node:(vertex -> unit Lwt.t) ->
    ?edge:(vertex -> vertex -> unit Lwt.t) ->
    skip:(vertex -> bool Lwt.t) ->
    rev:bool ->
    unit ->
    unit Lwt.t

  val iter :
    ?depth:int ->
    pred:(vertex -> vertex list Lwt.t) ->
    min:vertex list ->
    max:vertex list ->
    node:(vertex -> unit Lwt.t) ->
    ?edge:(vertex -> vertex -> unit Lwt.t) ->
    skip:(vertex -> bool Lwt.t) ->
    rev:bool ->
    unit ->
    unit Lwt.t

  val output :
    Format.formatter ->
    (vertex * Graph.Graphviz.DotAttributes.vertex list) list ->
    (vertex * Graph.Graphviz.DotAttributes.edge list * vertex) list ->
    string ->
    unit

  val min : t -> vertex list

  val max : t -> vertex list

  type dump = vertex list * (vertex * vertex) list

  val export : t -> dump

  val import : dump -> t

  module Dump : Type.S with type t = dump
end

module Make (Hash : Type.S) (Branch : Type.S) = struct
  module X = struct
    type t =
      [ `Contents of Hash.t
      | `Node of Hash.t
      | `Commit of Hash.t
      | `Branch of Branch.t ]
    [@@deriving irmin]

    let equal = Type.(unstage (equal t))

    let compare = Type.(unstage (compare t))

    let short_hash = Type.(unstage (short_hash Hash.t))

    let hash_branch = Type.(unstage (short_hash Branch.t))

    (* we are using cryptographic hashes here, so the first bytes
       are good enough to be used as short hashes. *)
    let hash (t : t) : int =
      match t with
      | `Contents c -> short_hash c
      | `Node n -> short_hash n
      | `Commit c -> short_hash c
      | `Branch b -> hash_branch b
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectional (X)
  module GO = Graph.Oper.I (G)
  module Topological = Graph.Topological.Make (G)
  module Table = Hashtbl.Make (X)
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

  let iter ?(depth = max_int) ~pred ~min ~max ~node ?edge ~skip ~rev () =
    Log.debug (fun f ->
        f "@[<2>iter:@ %arev=%b,@ min=%a,@ max=%a@]" pp_depth depth rev
          pp_vertices min pp_vertices max);
    let todo = Stack.create () in
    (* if a branch is in [min], add the commit it is pointing to too. *)
    Lwt_list.fold_left_s
      (fun acc -> function
        | `Branch _ as x -> pred x >|= fun c -> (x :: c) @ acc
        | x -> Lwt.return (x :: acc))
      [] min
    >>= fun min ->
    let treat key =
      Log.debug (fun f -> f "TREAT %a" Type.(pp X.t) key);
      node key >>= fun () ->
      if not (List.mem key min) then
        (* the edge function is optional to prevent an unnecessary computation
           of the preds .*)
        match edge with
        | None -> Lwt.return_unit
        | Some edge ->
            pred key >>= fun keys -> Lwt_list.iter_p (fun k -> edge key k) keys
      else Lwt.return_unit
    in
    let rec visit parent = function
      | [] -> (
          (if rev then treat parent else Lwt.return_unit) >>= fun () ->
          match Stack.pop todo with
          | exception Stack.Empty -> Lwt.return_unit
          | parent, children -> ( visit parent children [@tail]))
      | (key, level) :: tl -> (
          skip key >>= function
          | true -> ( visit parent tl [@tail])
          | false ->
              if level >= depth then Lwt.return_unit
              else (
                Log.debug (fun f -> f "VISIT %a" Type.(pp X.t) key);
                if List.mem key min then
                  treat key >>= fun () -> (visit parent tl [@tail])
                else
                  (if not rev then treat key else Lwt.return_unit) >>= fun () ->
                  Stack.push (parent, tl) todo;
                  let prepare_pred ~filter_history () =
                    pred key >|= fun keys ->
                    (*if a commit is in [min] cut the history but still visit
                       its nodes. *)
                    List.filter
                      (function
                        | `Commit _ when filter_history -> false | _ -> true)
                      keys
                  in
                  (match key with
                  | `Commit _ ->
                      prepare_pred ~filter_history:(List.mem key min) ()
                  | _ -> prepare_pred ~filter_history:false ())
                  >>= fun keys ->
                  let keys = List.map (fun k -> (k, level + 1)) keys in
                  (visit key keys [@tail])))
    in
    let visit_max key =
      skip key >>= function
      | true -> Lwt.return_unit
      | false ->
          if depth = 0 then Lwt.return_unit
          else (
            Log.debug (fun f -> f "VISIT %a" Type.(pp X.t) key);
            if List.mem key min then treat key
            else
              (if not rev then treat key else Lwt.return_unit) >>= fun () ->
              pred key >>= fun keys ->
              List.map (fun k -> (k, 1)) keys |> visit key)
    in
    Lwt_list.iter_s (fun m -> visit_max m) max

  let iter_track_visited ?(depth = max_int) ~pred ~min ~max ~node ?edge ~skip
      ~rev () =
    let marks = Table.create 1024 in
    let mark key = Table.add marks key () in
    let has_mark key = Table.mem marks key in
    let node k =
      mark k;
      node k
    in
    let skip k = if has_mark k then Lwt.return_true else skip k in
    iter ~depth ~pred ~min ~max ~node ?edge ~skip ~rev ()

  let closure ?(depth = max_int) ~pred ~min ~max () =
    let g = G.create ~size:1024 () in
    List.iter (G.add_vertex g) max;
    let node key =
      if not (G.mem_vertex g key) then G.add_vertex g key else ();
      Lwt.return_unit
    in
    let edge node pred =
      G.add_edge g pred node;
      Lwt.return_unit
    in
    let skip _ = Lwt.return_false in
    iter_track_visited ~depth ~pred ~min ~max ~node ~edge ~skip ~rev:false ()
    >|= fun () -> g

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
