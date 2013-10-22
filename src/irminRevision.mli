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

(** Manage the database history. *)

module type STORE = sig

  (** The database history is a partial-order of revisions. *)

  type tree
  (** Type of trees. *)

  type revision
  (** Type of revisions. *)

  module Graph: IrminGraph.S with type Vertex.t = revision
  (** Graph of revisions. *)

  include IrminStore.S with type value := tree
  (** Revisions are values. *)

  val create: t -> ?tree:key -> key list -> revision
  (** Create a new revision. *)

  val tree: t -> (key * tree Lwt.t) option
  (** Get the revision tree. *)

  val parents: t -> (key * tree Lwt.t) list
  (** Get the immmediate precessors. *)

  val cut: t -> ?roots:key list -> key list -> Graph.t Lwt.t
  (** [cut t max] returns a consistent cut of the partial order, where
      [max] are the max elements of the cut. If [roots] is set, these are
      the only minimal elements taken into account. *)

end

module Make
    (Store: IrminStore.RAW)
    (K: IrminKey.S)
    (T: IrminTree.STORE with type key = K.t):
  STORE with type key = K.t
         and type tree = T.tree
(** Create a revision store. *)
