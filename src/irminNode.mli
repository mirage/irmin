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

  (** The *node store* is a graph of keys. *)


  val read: t -> key -> node Lwt.t
  (** Read a node. *)

  val write: t -> key -> node -> unit Lwt.t
  (** Write a node. *)

  module Graph: IrminGraph.S with type Vertex.t = node
  (** Graph of nodes. *)

  val graph: t -> ?min:key list -> ?max:key list -> unit -> Graph.t Lwt.t
  (** [reads t ~min ~max ()] returns the nodes in the store, greater or
      equal than the given [min] (if it is set) and lesser or equal
      than the given [max] (if it is set). *)

end
