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

(** Manage the database history. *)

module type S = sig

  (** Signature for commit objects. *)

  include Ir_contents.S
  (** Base functions over commit objects. *)

  type commit_key
  (** Type for commit Keys. *)

  type node_key
  (** Type for node keys. *)

  type origin
  (** Origin. *)

  val node: t -> node_key option
  (** The underlying node. *)

  val parents: t -> commit_key list
  (** The commit parents. *)

  val origin: t -> origin
  (** The commit provenance. *)

  val edges: t -> (key, 'b) Ir_graph.vertex list
  (** The graph edges. *)

end

module S (K: Ir_uid.S): S with type key = K.t

module SHA1: S with type key = Ir_uid.SHA1.t
(** Simple implementation where keys are SHA1s. *)

module type STORE = sig

  (** Store the history as a partial-order of revisions. *)

  include Ir_ao.S

  type origin
  (** Origin of values. *)

  type node
  (** Node values. *)

  val commit: t -> origin -> ?node:node -> parents:value list -> (key * value) Lwt.t
  (** Create a new commit. *)

  val node: t -> value -> node Lwt.t option
  (** Get the commit node. *)

  val parents: t -> value -> value Lwt.t list
  (** Get the immmediate precessors. *)

  val merge: t -> key Ir_merge.t
  (** Lift [S.merge] to the store keys. *)

  val find_common_ancestor: t -> key -> key -> key option Lwt.t
  (** Find the common ancestor of two commits. *)

  val find_common_ancestor_exn: t -> key -> key -> key Lwt.t
  (** Same as [find_common_ancestor] but raises [Not_found] if the two
      commits share no common ancestor. *)

  val list: t -> ?depth:int -> key list -> key list Lwt.t
  (** Return all previous commit hashes, with an (optional) limit on
      the history depth. *)

  module Key: Ir_uid.S with type t = key
  (** Base functions over keys. *)

  module Value: S with type t = value and type origin = origin
  (** Base functions over values. *)

  module Node: Ir_node.S with type t = node
  (** Base functions over nodes. *)

  module Origin: Ir_origin.S with type t = origin
  (** Base functions over origins. *)

end

module Make (V: S) (Node: Ir_node.STORE) (Commit: Ir_ao.S with type value = V.t)
  : STORE with type t = Node.t * Commit.t
           and type key = Commit.key
           and type value = Commit.value
           and type origin = Ir_origin.t
(** Create a commit store. *)

module Rec (S: STORE): Ir_contents.S with type t = S.key
(** Convert a commit store objects into storable keys, with the
    expected merge functions. *)
