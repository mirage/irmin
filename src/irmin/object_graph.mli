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

(** Graphs. *)

module type S = sig
  include Graph.Sig.I
  (** Directed graph *)

  include Graph.Oper.S with type g := t
  (** Basic operations. *)

  (** Topoogical traversal *)
  module Topological : sig
    val fold : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  end

  val vertex : t -> vertex list
  (** Get all the vertices. *)

  val edges : t -> (vertex * vertex) list
  (** Get all the relations. *)

  val closure :
    ?depth:int ->
    pred:(vertex -> vertex list Lwt.t) ->
    min:vertex list ->
    max:vertex list ->
    unit ->
    t Lwt.t
  (** [closure min max pred] creates the clansitive closure of [max]
      using the precedence relation [pred]. The closure will not
      contain any keys before the the one specified in [min]. *)

  val output :
    Format.formatter ->
    (vertex * Graph.Graphviz.DotAttributes.vertex list) list ->
    (vertex * Graph.Graphviz.DotAttributes.edge list * vertex) list ->
    string ->
    unit
  (** [output ppf vertex edges name] create aand dumps the graph
      contents on [ppf]. The graph is defined by its [vertex] and
      [edges]. [name] is the name of the output graph.*)

  val min : t -> vertex list
  (** Compute the minimum vertex. *)

  val max : t -> vertex list
  (** Compute the maximun vertex. *)

  type dump = vertex list * (vertex * vertex) list
  (** Expose the graph internals. *)

  val export : t -> dump
  (** Expose the graph as a pair of vertices and edges. *)

  val import : dump -> t
  (** Import a graph. *)

  module Dump : Type.S with type t = dump
  (** The base functions over graph internals. *)
end

module Make
    (Contents : Type.S)
    (Metadata : Type.S)
    (Node : Type.S)
    (Commit : Type.S)
    (Branch : Type.S) :
  S
    with type V.t =
          [ `Contents of Contents.t * Metadata.t
          | `Node of Node.t
          | `Commit of Commit.t
          | `Branch of Branch.t ]
(** Build a graph. *)
