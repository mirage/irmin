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

module type S = sig

  (** A branch-consistent store is a mutable store which supports
      fork/join operations. *)

  include Ir_rw.S

  type tag
  (** Type of branch tags. *)

  type origin
  (** Type of values keeping track of provenance. *)

  val create: ?tag:tag -> unit -> t Lwt.t
  (** Create a store handle. The default branch, if not set, is
      the [master] tag. *)

  val detach: t -> unit Lwt.t
  (** Detach the current branch (ie. it is not assiaciated to a tag
      name anymore). *)

  val tag: t -> tag option
  (** Return the branch of the given store handle. *)

  val tag_exn: t -> tag
  (** Same as [tag] but raise [Not_found] in case of a detached
      head. *)

  val set_tag: t -> tag -> unit
  (** Update the current tag name. *)

  val update: t -> ?origin:origin -> key -> value -> unit Lwt.t
  (** Same as [RW.update] but with an optional [origin] argument to
      keep track of provenance. *)

  val remove: t -> ?origin:origin -> key -> unit Lwt.t
  (** Same as [RW.remove] but with an optional [origin] argument to
      keep track of provenance. *)

  val clone: t -> tag -> t option Lwt.t
  (** Fork the store, using the given branch name. Return [None] if
      the branch already exists. *)

  val clone_force: t -> tag -> t Lwt.t
  (** Same as [clone] but delete and update the existing branch if a
      branch with the same name already exists. *)

  val switch: t -> tag -> unit Lwt.t
  (** Switch the database contents the be same as the contents of the
      given branch name. The two branches are still independant. *)

  val merge: t -> ?origin:origin -> tag -> unit Ir_merge.result Lwt.t
  (** [merge db t] merges the branch [t] into the current database
      branch. The two branches are still independant. *)

  val merge_exn: t -> ?origin:origin -> tag -> unit Lwt.t
  (** Same as [merge] but raise [Conflict "<msg>"] in case of a
      conflict. *)

end

module type STORE = sig

  (** Same as [S] but also expose the store internals. Useful to build
      derived store functionalities. *)

  include S

  type uid
  (** The type for internal identifiers. *)

  module Block: Ir_block.STORE with type key = uid and type contents = value
  (** The internal block store. *)

  module Tag: Ir_tag.STORE with type key = tag and type value = uid
  (** The internal tag store. *)

  val block_t: t -> Block.t
  (** Get the block store handler. *)

  val commit_t: t -> Block.Commit.t
  (** Get the commit store handler. *)

  val node_t: t -> Block.Node.t
  (** Get the node store handler. *)

  val contents_t: t -> Block.Contents.t
  (** Get the contents store handler. *)

  val tag_t: t -> Tag.t
  (** Get the tag store handler. *)

  (** {2 Commit heads} *)

  val create_head: Block.Commit.key -> t Lwt.t
  (** Create a temporary branch, which will not be recorded in the tag
      branch. *)

  val head: t -> Block.Commit.key option Lwt.t
  (** Return the head commit. *)

  val head_exn: t -> Block.Commit.key Lwt.t
  (** Same as [read_head] but raise [Not_found] if the commit does not
      exist. *)

  val set_head: t -> Block.Commit.key -> unit
  (** Set the commit head. *)

  val update_head: t -> Block.Commit.key -> unit Lwt.t
  (** Set the current branch to point to the given commit. *)

  val merge_head: t -> ?origin:origin -> Block.Commit.key -> unit Ir_merge.result Lwt.t
  (** Merge a commit with the current branch. *)

  (** {2 Nodes} *)

  val read_node: t -> key -> Block.Node.value option Lwt.t
  (** Read a node. *)

  val update_node: t -> origin -> key -> Block.Node.value -> unit Lwt.t
  (** Update a node. *)

  val watch_node: t -> key -> (key * Block.Node.key) Lwt_stream.t
  (** Watch commit changes. Return the stream of commit
      identifiers. *)

  module Graph: Ir_graph.S with type V.t = (Block.key, unit) Ir_graph.vertex
  (** Graph of blocks. *)

end

module type MAKER =
  functor (U: Ir_uid.S) -> functor (C: Ir_contents.S) -> functor (T: Ir_tag.S) ->
    STORE with type key = Ir_path.t
           and type value = C.t
           and type origin = Ir_origin.t
           and type tag = T.t
           and type uid = U.t
(** Signature of functors to create branch-consistent stores. *)

module Make (Block: Ir_block.STORE) (Tag: Ir_tag.STORE with type value = Block.key)
  : STORE with type key = Ir_path.t
           and type value = Block.contents
           and type origin = Ir_origin.t
           and type tag = Tag.key
           and type uid = Block.key
           and module Block = Block
           and module Tag = Tag
(** Build a branch consistent store from custom [Block] and [Tag]
    store implementations. *)
