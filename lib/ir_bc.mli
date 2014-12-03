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

(** Branch-consistent stores: read-write store with support fork/merge
    operations. *)

module type STORE = sig
  include Ir_rw.STORE
  type tag
  val of_tag: Ir_config.t -> Ir_task.t -> tag -> t Lwt.t
  val tag: t -> tag option
  val tag_exn: t -> tag
  val tags: t -> tag list Lwt.t
  val update_tag: t -> tag -> [`Ok | `Duplicated_tag] Lwt.t
  val update_tag_force: t -> tag -> unit Lwt.t
  val switch: t -> tag -> unit Lwt.t
  type head
  val of_head: Ir_config.t -> Ir_task.t -> head -> t Lwt.t
  val head: t -> head option Lwt.t
  val head_exn: t -> head Lwt.t
  val branch: t -> [`Tag of tag | `Head of head]
  val heads: t -> head list Lwt.t
  val detach: t -> unit Lwt.t
  val update_head: t -> head -> unit Lwt.t
  val merge_head: t -> head -> unit Ir_merge.result Lwt.t
  val watch_head: t -> key -> (key * head) Lwt_stream.t
  val clone: t -> tag -> [`Ok of t | `Duplicated_tag] Lwt.t
  val clone_force: t -> tag -> t Lwt.t
  val merge: t -> tag -> unit Ir_merge.result Lwt.t
  type slice
  val export: ?full:bool -> ?depth:int -> ?min:head list -> ?max:head list ->
    t -> slice Lwt.t
  val import: t -> slice -> [`Ok | `Duplicated_tags of tag list] Lwt.t
  val import_force: t -> slice -> unit Lwt.t
end

module type MAKER =
  functor (K: Ir_path.S) ->
  functor (C: Ir_contents.S) ->
  functor (T: Ir_tag.S) ->
  functor (H: Ir_hash.S) ->
    STORE with type key = K.t
           and type value = C.t
           and type head = H.t
           and type tag = T.t

module type PRIVATE = sig
  module Contents: Ir_contents.STORE
  module Node: Ir_node.STORE with type Val.contents = Contents.key
  module Commit: Ir_commit.STORE with type Val.node = Node.key
  module Tag: Ir_tag.STORE with type value = Commit.key
  module Slice: Ir_slice.S
    with type contents = (Contents.key * Contents.value) list
     and type nodes = (Node.key * Node.value) list
     and type commits = (Commit.key * Commit.value) list
     and type tags = (Tag.key * Tag.value) list
end

module Make (X: Ir_ao.MAKER) (Y: Ir_rw.MAKER): MAKER

(** {1 Extended API} *)

module type STORE_EXT = sig

  (** Same as [S] but also expose the store internals. Useful to build
      derived functionalities. *)

  type step

  include STORE with type key = step list

  module Key: Ir_path.S with type step = step
  (** Base functions over keys. *)

  module Val: Ir_contents.S with type t = value
  (** Base functions over values. *)

  module Private: PRIVATE
    with type Contents.value = value
     and type Node.Path.step = step
     and type Commit.key = head
     and type Tag.key = tag
     and type Slice.t = slice
     and module Node.Path = Key

  (** {1 Internal "block", append-only stores. *)

  module Contents:  Ir_contents.STORE_EXT
    with type t = Private.Contents.t
     and type key = Private.Contents.key
     and type value = Private.Contents.value

  module Node: Ir_node.STORE_EXT
    with type t = Private.Contents.t * Private.Node.t
     and type key = Private.Node.key
     and type value = Private.Node.value
     and type step = step
     and module Contents = Contents

  module Commit: Ir_commit.STORE_EXT
    with type t = Private.Contents.t * Private.Node.t * Private.Commit.t
     and type key = head
     and type value = Private.Commit.value
     and module Node = Node

  val contents_t: t -> Contents.t
  val node_t: t -> Node.t
  val commit_t: t -> Commit.t

  (** {1 Internal "tag", read-write store. *)

  module Tag: Ir_tag.STORE
    with type t = Private.Tag.t
     and type key = tag
     and type value = head

  val tag_t: t -> Tag.t

  (** {1 Nodes} *)

  val read_node: t -> key -> Node.value option Lwt.t
  (** Read a node. *)

  val mem_node: t -> key -> bool Lwt.t
  (** Check whether a node exists. *)

  val update_node: t -> key -> Node.value -> unit Lwt.t
  (** Update a node. *)

  module Graph: Ir_graph.S
    with type V.t =
      [ `Contents of Contents.key
      | `Node of Node.key
      | `Commit of Commit.key
      | `Tag of Tag.key ]
      (** The global graph of internal objects. *)

end

module Make_ext (P: PRIVATE): STORE_EXT
  with type step = P.Node.Path.step
   and type value = P.Contents.value
   and type tag = P.Tag.key
   and type head = P.Commit.key
   and type slice = P.Slice.t
   and module Private.Contents = P.Contents
   and module Private.Node = P.Node
   and module Private.Commit = P.Commit
   and module Private.Tag = P.Tag
   and module Private.Slice = P.Slice
