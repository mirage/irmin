(*
 * Copyright (c) 2013      Louis Gesbert     <louis.gesbert@ocamlpro.com>
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

(** Nodes represent structured values serialized in the block
    store. *)

(** The blocks form a labeled directed acyclic graph (DAG). For
    instance, using the following API it is possible to go from one
    node to another following a path in the graph. Every node of the
    graph might carry some optional contents. *)

module type S = sig

  (** Node values. *)

  include Ir_contents.S

  type contents
  (** Type for contents. *)

  type node
  (** Type for nodes. *)

  val contents: t -> contents option
  (** [contents t] is the (optional) key of the node contents. *)

  val contents_exn: t -> contents
  (** Same as [contents], but raise [Not_found] if it is [None]. *)

  val edges: t -> (contents, node, _, _) Ir_graph.vertex list
  (** Return the list of successor vertices. *)

  val empty: t
  (** The empty node. *)

  val leaf: contents -> t
  (** Create a leaf node, with some contents and no successors. *)

  val create: ?contents:contents -> node list -> t
  (** [create ~contents succ] is the node with contents [contents] and
      successors [succs]. *)

  val is_empty: t -> bool
  (** Is the node empty. *)

  val is_leaf: t -> bool
  (** Is it a leaf node (see [leaf]) ? *)

end

module SHA1: S with type contents = Ir_uid.SHA1.t and type node = Ir_uid.SHA1.t
(** Simple node implementation, where all unique identifiers are
    SHA1s. *)

module S (C: Tc.I0) (N: Tc.I0): S with type contents = C.t and type node = N.t
(** Base functions for nodes. *)

module type STORE = sig

  (** The node store encodes a labeled DAG where every node might hold
      some contents. *)

  include Ir_ao.STORE

  type contents
  (** Node contents. *)

  type path
  (** Paths to go from one node to an other. *)

  val node: t -> ?contents:contents -> ?succ:(string * value) list ->
    unit -> (key * value) Lwt.t
  (** Create a new node. *)

  val contents: t -> value -> contents Lwt.t option
  (** Return the node contents. *)

  val succ: t -> value -> value Lwt.t Ir_misc.StringMap.t
  (** Return the node successors. *)

  val sub: t -> value -> path -> value option Lwt.t
  (** Find a subvalue. *)

  val sub_exn: t -> value -> path -> value Lwt.t
  (** Find a subvalue. Raise [Not_found] if it does not exist. *)

  val map: t -> value -> path -> (value -> value) -> value Lwt.t
  (** Modify a subtree. *)

  val update: t -> value -> path -> contents -> value Lwt.t
  (** Add a value by recusively saving subvalues into the
      corresponding stores. *)

  val find: t -> value -> path -> contents option Lwt.t
  (** Find a value. *)

  val find_exn: t -> value -> path -> contents Lwt.t
  (** Find a value. Raise [Not_found] is [path] is not defined. *)

  val remove: t -> value -> path -> value Lwt.t
  (** Remove a value. *)

  val valid: t -> value -> path -> bool Lwt.t
  (** Is a path valid. *)

  val merge: t -> key Ir_merge.t
  (** Merge two nodes together. *)

  module Contents: Ir_contents.STORE with type value = contents
  (** The contents store. *)

  val contents_t: t -> Contents.t
  (** [contents_t t] is the hanlder of the underlying contents store
      of [t]. *)

  module Path: Ir_path.S with type t = path
  (** Base functions for paths. *)

  module Key: Ir_uid.S with type t = key
  (** Base functions for keys. *)

  module Value:
    S with type t = value and type node = key and type contents = Contents.key
  (** Base functions for values. *)

end

module type MAKER =
  functor (K: Ir_uid.S) ->
  functor (P: Ir_path.S) ->
  functor (C: Ir_contents.S) ->
    STORE with type contents = C.t and type path = P.t

module Make (Contents: Ir_ao.MAKER) (Node: Ir_ao.MAKER): MAKER
(** Create a node store from an append-only database. *)

module Rec (S: STORE): Ir_contents.S with type t = S.key
(** Same as [Ir_contents.Rec] but for node stores. *)
