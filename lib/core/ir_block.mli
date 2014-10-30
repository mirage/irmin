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

(** Store structured values: contents, node and commits. *)

module type S = sig

  (** Signature for structured values. *)

  include Ir_contents.S

  type contents
  (** Type for contents. *)

  type node
  (** Type for nodes. *)

  type commit
  (** Type for commits. *)

  val contents: contents -> t
  val node: node -> t
  val commit: commit -> t

  val eq_contents: t -> contents option
  val eq_node: t -> node option
  val eq_commit: t -> commit option
end

module String: S with type contents = string
(** String contents. *)

module JSON: S with type contents = Ezjsonm.t
(** JSON contents. *)

module type STORE = sig

  (** The block store holds the representation of all the immutable
      values of the system. *)

  include Ir_ao.S

  type contents
  (** Contents values. *)

  type node
  (** Node values. *)

  type commit
  (** Commit values. *)

  val list: t -> ?depth:int -> key list -> key list Lwt.t
  (** Return the related blocks, with an history depth limit. *)

  module Contents: Ir_contents.STORE
    with type key = key and type value = contents

  module Node: Ir_node.STORE
    with type key = key and type value = node and type contents = contents

  module Commit: Ir_commit.STORE
    with type key = key and type value = commit

  val contents_t: t -> Contents.t
  (** The handler for the contents database. *)

  val node_t: t -> Node.t
  (** The handler for the node database. *)

  val commit_t: t -> Commit.t
  (** The handler for the commit database. *)

  val merge: t -> key Ir_merge.t
  (** Merge keys of the store together. *)

  module Key: Ir_uid.S with type t = key
  (** Base functions over keys. *)

  module Value: S
    with type contents = contents and type node = node and type commit = commit
  (** Base functions over values. *)

  module Graph: Ir_graph.S with type V.t = (key, unit) Ir_graph.vertex
  (** Graphs of blocks. *)

end

module Make (V: S)
    (Commit: Ir_commit.STORE) (Block: Ir_ao.S with type value = V.t)
  : STORE with type key = Block.t
           and type contents = Commit.Node.contents
           and type origin = Ir_origin.t
           and type Contents.t = S.t
           and type Node.t = S.t * S.t
           and type Commit.t = (S.t * S.t) * S.t
(** Create a store for structured values. *)

module Mux
  (K: Ir_uid.S)
  (C: Ir_contents.S)
  (Contents: Ir_ao.S with type key = K.t and type value = C.t)
  (Node: Ir_ao.S with type key = K.t and type value = K.t Ir_node.t)
  (Commit: Ir_ao.S with type key = K.t and type value = (Ir_origin.t, K.t) Ir_commit.t)
  : STORE with type key = K.t
           and type contents = C.t
           and type origin = Ir_origin.t
           and type Contents.t = Contents.t
           and type Node.t = Contents.t * Node.t
           and type Commit.t = (Contents.t * Node.t) * Commit.t
(** Combine multiple stores to create a global store for structured
    values. XXX: discuss about the cost model, ie. the difference
    between Mux and Make. *)

module Rec (S: STORE): Ir_contents.S with type t = S.key
(** Interpret the blocks in a block store as storable objects. *)

module S (K: Ir_uid.S) (C: Ir_contents.S):
  S with type key = K.t and type contents = C.t
