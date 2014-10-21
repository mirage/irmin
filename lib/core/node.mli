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

type 'key t = {
  contents: 'key option;
  succ    : 'key Map.Make(String).t;
} with bin_io, compare, sexp
(** Node values. They might contain a pointer to an optional contents,
    and pointers to its successors.*)

val equal: ('key -> 'key -> bool) -> 'key t -> 'key t -> bool
(** Compare two nodes. *)

val contents_exn: 'key t -> 'key
(** Get the contents key, raise [Not_found] if None. *)

val edges: 'a t -> ('a, 'b) Digraph.vertex list
(** Return the list of successor vertices. *)

val empty: 'key t
(** The empty node. *)

val leaf: 'key -> 'key t
(** Create a leaf node (with some contents and no successors). *)

val is_empty: 'key t -> bool
(** Is the node empty. *)

val is_leaf: 'key t -> bool
(** Is it a leaf node (see [leaf]) ? *)

module type S = sig

  (** Node values. *)

  type key
  (** Foreign keys. *)

  type nonrec t = key t

  include Contents.S with type t := t

end

module type STORE = sig

  (** The node store encodes a labeled DAG where every node might hold
      some contents. *)

  type key
  (** Database keys. *)

  type value = key t
  (** Node values. *)

  include Sig.AO with type key := key and type value := value

  type contents
  (** Node contents. *)

  type path = Path.t
  (** Paths to go from one node to an other. *)

  val node: t -> ?contents:contents -> ?succ:(string * value) list ->
    unit -> (key * value) Lwt.t
  (** Create a new node. *)

  val contents: t -> value -> contents Lwt.t option
  (** Return the node contents. *)

  val succ: t -> value -> value Lwt.t Misc.StringMap.t
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

  val merge: t -> key Merge.t
  (** Merge two nodes together. *)

  module Key: Key.S with type t = key
  (** Base functions for keys. *)

  module Value: S with type key = key
  (** Base functions for values. *)

end

module SHA1: S with type key = Key.SHA1.t
(** Simple node implementation, where keys are SHA1s. *)

module Make
    (K: Key.S)
    (C: Contents.S)
    (Contents: Contents.STORE with type key = K.t and type value = C.t)
    (Node: Sig.AO with type key = K.t and type value = K.t t)
  : STORE with type t = Contents.t * Node.t
           and type key = K.t
           and type contents = C.t
           and type path = Path.t
(** Create a node store from an append-only database. *)

module Rec (S: STORE): Contents.S with type t = S.key
(** Convert a node store objects into storable keys, with the expected
    merge function. *)

module S (K: Key.S): S with type key = K.t
(** Base functions for nodes. *)
