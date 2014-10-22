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

type ('origin, 'key, 'contents) t =
  | Contents of 'contents
  | Node of 'key Node.t
  | Commit of ('origin, 'key) Commit.t
(** The different kinds of values which can be stored in the
    database. *)

type origin = Origin.t

module type S = sig

  (** Signature for structured values. *)

  type key
  (** Keys. *)

  type contents
  (** Contents. *)

  include Sig.Contents with type t = (origin, key, contents) t
  (** Base functions over structured values. *)

end

module String: S with type key = Uid.SHA1.t
                  and type contents = Contents.String.t
(** String contents, with SHA1 keys. *)

module JSON: S with type key = Uid.SHA1.t
                and type contents = Contents.JSON.t
(** JSON contents, with SHA1 keys. *)


module type STORE = sig

  (** The block store holds the representation of all the immutable
      values of the system. *)

  type key
  (** Database keys. *)

  type contents
  (** Contents values. *)

  type value =  (origin, key, contents) t
  (** Block values. *)

  type node = key Node.t
  (** Node values. *)

  type commit = (origin, key) Commit.t
  (** Commit values. *)

  include Sig.AO with type key := key and type value := value

  val list: t -> ?depth:int -> key list -> key list Lwt.t
  (** Return the related blocks, with an history depth limit. *)

  module Contents: Contents.STORE with type key = key and type value = contents

  module Node: Node.STORE with type key = key and type contents = contents

  module Commit: Commit.STORE with type key = key

  val contents_t: t -> Contents.t
  (** The handler for the contents database. *)

  val node_t: t -> Node.t
  (** The handler for the node database. *)

  val commit_t: t -> Commit.t
  (** The handler for the commit database. *)

  val merge: t -> key Merge.t
  (** Merge keys of the store together. *)

  module Key: Sig.Uid with type t = key
  (** Base functions over keys. *)

  module Value: S with type key = key and type contents = contents
  (** Base functions over values. *)

  module Graph: Digraph.S with type V.t = (key, unit) Digraph.vertex

end

module Make
  (K: Sig.Uid)
  (C: Sig.Contents)
  (S: Sig.AO with type key = K.t and type value = (origin, K.t, C.t) t)
  : STORE with type key = K.t
           and type contents = C.t
           and type Contents.t = S.t
           and type Node.t = S.t * S.t
           and type Commit.t = (S.t * S.t) * S.t
(** Create a store for structured values. *)

module Mux
  (K: Sig.Uid)
  (C: Sig.Contents)
  (Contents: Sig.AO with type key = K.t and type value = C.t)
  (Node: Sig.AO with type key = K.t and type value = K.t Node.t)
  (Commit: Sig.AO with type key = K.t and type value = (origin, K.t) Commit.t)
  : STORE with type key = K.t
           and type contents = C.t
           and type Contents.t = Contents.t
           and type Node.t = Contents.t * Node.t
           and type Commit.t = (Contents.t * Node.t) * Commit.t
(** Combine multiple stores to create a global store for structured
    values. XXX: discuss about the cost model, ie. the difference
    between Mux and Make. *)

module Rec (S: STORE): Sig.Contents with type t = S.key
(** Interpret the blocks in a block store as storable objects. *)

module S (K: Sig.Uid) (C: Sig.Contents): S with type key = K.t and type contents = C.t
