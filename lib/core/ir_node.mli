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

  type key
  (** Foreign keys. *)

  val contents: t -> key option

  val contents_exn: t -> 'key
  (** Get the contents key, raise [Not_found] if None. *)

  val edges: t -> (key, 'b) Ir_graph.vertex list
  (** Return the list of successor vertices. *)

  val empty: t
  (** The empty node. *)

  val leaf: key -> t
  (** Create a leaf node (with some contents and no successors). *)

  val is_empty: t -> bool
  (** Is the node empty. *)

  val is_leaf: t -> bool
  (** Is it a leaf node (see [leaf]) ? *)

end

module type STORE = sig

  (** The node store encodes a labeled DAG where every node might hold
      some contents. *)

  include Ir_ao.S

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

  module Key: Ir_uid.S with type t = key
  (** Base functions for keys. *)

  module Value: S with type t = value
  (** Base functions for values. *)

end

module SHA1: S with type key = Ir_uid.SHA1.t
(** Simple node implementation, where keys are SHA1s. *)

module Make (S: S)
    (Contents: Ir_contents.STORE) (Node: Ir_ao.S with type value = S.t)
  : STORE with type t = Contents.t * Node.t
           and type key = Node.key
           and type value = Node.value
           and type contents = Contents.value
           and type path = Ir_path.String.t
(** Create a node store from an append-only database. *)

module Rec (S: STORE): Ir_contents.S with type t = S.key
(** Convert a node store objects into storable keys, with the expected
    merge function. *)

module S (K: Ir_uid.S): S with type key = K.t
(** Base functions for nodes. *)
