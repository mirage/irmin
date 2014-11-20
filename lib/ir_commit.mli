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

  type commit
  (** Type for commit keys. *)

  type node
  (** Type for node keys. *)

  val create: origin -> ?node:node -> parents:commit list -> t
  (** Create a commit. *)

  val node: t -> node option
  (** The underlying node. *)

  val parents: t -> commit list
  (** The commit parents. *)

  val origin: t -> origin
  (** The commit provenance. *)

  val edges: t -> [`Node of node | `Commit of commit] list
  (** The graph edges. *)

end

module type STORE = sig

  (** Store the history as a partial-order of revisions. *)

  include Ir_ao.STORE

  type node
  (** Node values. *)

  val commit: t -> origin -> ?node:node -> parents:value list -> (key * value) Lwt.t
  (** Create a new commit. *)

  val node: t -> origin -> value -> node Lwt.t option
  (** Get the commit node. *)

  val parents: t -> origin -> value -> value Lwt.t list
  (** Get the immmediate precessors. *)

  val merge: t -> (key, origin) Ir_merge.t
  (** Lift [S.merge] to the store keys. *)

  val find_common_ancestor: t -> origin -> key -> key -> key option Lwt.t
  (** Find the common ancestor of two commits. *)

  val find_common_ancestor_exn: t -> origin -> key -> key -> key Lwt.t
  (** Same as [find_common_ancestor] but raises [Not_found] if the two
      commits share no common ancestor. *)

  val list: t -> origin -> ?depth:int -> key list -> key list Lwt.t
  (** Return all previous commit hashes, with an (optional) limit on
      the history depth. *)

  module Node: Ir_node.STORE
    with type value = node
     and type origin = origin
  (** Base functions over nodes. *)

  module Key: Ir_uid.S with type t = key
  (** Base functions over keys. *)

  module Val: S
    with type t = value
     and type commit = key
     and type origin = origin
     and type node = Node.key
  (** Base functions over values. *)

end

module type MAKER =
  functor (K: Ir_uid.S) ->
  functor (N: Ir_node.STORE) ->
    STORE with type key = K.t
           and type origin = N.origin
           and type node = N.value
           and module Node = N

module Make (S: Ir_ao.MAKER): MAKER
(** Create a commit store. *)

module Rec (S: STORE): Ir_contents.S with type t = S.key
(** Same as [Ir_contents.Rec] but for commit stores. *)
