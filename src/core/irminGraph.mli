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

(** Graphs. *)

(** Main signature. *)
module type S = sig

  (** Type of keys *)
  module Vertex: IrminBase.S

  (** Directed graph *)
  include Graph.Sig.I with type V.t = Vertex.t
  include Graph.Oper.S with type g := t

  (** Topoogical traversal *)
  module Topological: sig
    val fold: (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  end

  (** Get all the vertices. *)
  val vertex: t -> vertex list

  (** Get all the relations. *)
  val edges: t -> (vertex * vertex) list

  (** [closure min max pred] creates the clansitive closure of [max]
      using the precedence relation [pred]. The closure will not
      contain any keys before the the one specified in [min]. *)
  val closure:
    (vertex -> vertex list Lwt.t)
    -> min:vertex list
    -> max:vertex list
    -> t Lwt.t

  (** [output g tags name] dumps the graph contents [g], which the
      vertices labelled by [tags]. [name] is the graph global
      label.  *)
  val output: t ->
    ?labels:(vertex * string) list ->
    ?overlay:(vertex * vertex) list ->
    string -> unit

  (** Compute the minimum vertex. *)
  val min: t -> vertex list

  (** Compute the maximun vertex. *)
  val max: t -> vertex list

  (** Implements the base operations. *)
  include IrminBase.S with type t := t

end

(** Build a graph. *)
module Make(B: IrminBase.S): S with type Vertex.t = B.t
