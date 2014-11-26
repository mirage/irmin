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
  val of_tag: (string * Ir_univ.t) list -> Ir_task.t -> tag -> t
  val tag: t -> tag option
  val tag_exn: t -> tag
  val update_tag: t -> tag -> [`Ok | `Duplicated_tag] Lwt.t
  val update_tag_force: t -> tag -> unit Lwt.t
  val switch: t -> tag -> unit Lwt.t
  val detach: t -> unit Lwt.t
  type head
  val of_head: (string * Ir_univ.t) list -> Ir_task.t -> head -> t
  val head: t -> head option Lwt.t
  val head_exn: t -> head Lwt.t
  val heads: t -> head list Lwt.t
  val update_head: t -> head -> unit Lwt.t
  val merge_head: t -> head -> unit Ir_merge.result Lwt.t
  val watch_head: t -> key -> (key * head) Lwt_stream.t
  val clone: t -> tag -> [`Ok of t | `Duplicated_tag] Lwt.t
  val clone_force: t -> tag -> t Lwt.t
  val merge: t -> tag -> unit Ir_merge.result Lwt.t
  type slice
  module Slice: Tc.S0 with type t = slice
  val export: ?full:bool -> ?depth:int -> ?min:head list -> ?max:head list ->
    t -> slice Lwt.t
  val import: t -> slice -> [`Ok | `Duplicated_tags of tag list] Lwt.t
  val import_force: t -> slice -> unit Lwt.t
end

module type MAKER =
  functor (B: Ir_block.STORE) ->
  functor (T: Ir_tag.STORE with type value = B.head)
    -> STORE with type key = B.step list
              and type value = B.contents
              and type tag = T.key
              and type head = B.head

module Make: MAKER

module type STORE_EXT = sig

  (** Same as [S] but also expose the store internals. Useful to build
      derived functionalities. *)

  type step

  include STORE with type key = step list

  module Block: Ir_block.STORE
    with type step = step
     and type contents = value
     and type head = head
  (** The internal block store. *)

  val contents_t: t -> Block.Contents.t
  val node_t: t -> Block.Node.t
  val commit_t: t -> Block.Commit.t

  module Tag: Ir_tag.STORE
    with type key = tag
     and type value = head
  (** The internal tag store. *)

  val tag_t: t -> Tag.t

  module Key: Tc.S0 with type t = Block.step list
  (** Base functions over keys. *)

  module Val: Ir_contents.S with type t = value
  (** Base functions over values. *)

  (** {2 Nodes} *)

  val read_node: t -> key -> Block.Node.value option Lwt.t
  (** Read a node. *)

  val mem_node: t -> key -> bool Lwt.t
  (** Check whether a node exists. *)

  val update_node: t -> key -> Block.Node.value -> unit Lwt.t
  (** Update a node. *)

  (** {2 More Slices} *)

  val slice_contents: slice -> (Block.Contents.key * Block.contents) list
  val slice_nodes: slice -> (Block.Node.key * Block.node) list
  val slice_commits: slice -> (Block.Commit.key * Block.commit) list
  val slice_tags: slice -> (Tag.key * Tag.value) list

  module Graph: Ir_graph.S
    with type V.t =
      [ `Contents of Block.Contents.key
      | `Node of Block.Node.key
      | `Commit of Block.Commit.key
      | `Tag of Tag.key ]
  (** The global graph of internal objects. *)

end

module type MAKER_EXT =
  functor (B: Ir_block.STORE) ->
  functor (T: Ir_tag.STORE with type value = B.head)
    -> STORE_EXT with type step = B.step
                  and type value = B.contents
                  and type tag = T.key
                  and type head = B.head
                  and module Block = B
                  and module Tag = T

module Make_ext: MAKER_EXT
