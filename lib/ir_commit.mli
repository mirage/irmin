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

  val create: Ir_task.t -> ?node:node -> parents:commit list -> t
  (** Create a commit. *)

  val node: t -> node option
  (** The underlying node. *)

  val parents: t -> commit list
  (** The commit parents. *)

  val task: t -> Ir_task.t
  (** The commit provenance. *)

  val edges: t -> [> `Node of node | `Commit of commit] list
  (** The graph edges. *)

end

module Commit (C: Tc.S0) (N: Tc.S0):
  S with type commit = C.t
     and type node = N.t

module type RAW_STORE = sig

  include Ir_ao.STORE

  module Key: Ir_hash.S with type t = key
  (** Base functions over keys. *)

  module Val: S
    with type t = value
     and type commit = key
  (** Base functions over values. *)

end

module type STORE = sig

  (** Store the history as a partial-order of revisions. *)

  module Node: Ir_node.STORE
  (** Base functions over nodes. *)

  include RAW_STORE with type Val.node = Node.key

  type node = Node.value
  (** Node values. *)

  val commit: t -> ?node:node -> parents:value list -> (key * value) Lwt.t
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

  val node_t: t -> Node.t
  (** An handler to the node database. *)

end

module Make
    (C: Ir_contents.RAW_STORE)
    (N: Ir_node.RAW_STORE with type Val.contents = C.key)
    (S: RAW_STORE with type Val.node = N.key):
  STORE with type t = C.t * N.t * S.t
         and type key = S.key
         and type value = S.value
         and module Node = Ir_node.Make(C)(N)
(** Create a commit store. *)
