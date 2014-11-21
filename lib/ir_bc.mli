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

  (** A branch-consistent store is a mutable store which supports
      fork/join operations. *)

  include Ir_rw.STORE

  (** {2 Tags} *)

  type tag
  (** Type of branch tags. *)

  val of_tag: tag -> t
  (** Create a store handle. Similar to [create], but use any tag name
      instead of the [master] tag. *)

  val tag: t -> tag option
  (** Return the branch of the given store handle. *)

  val tag_exn: t -> tag
  (** Same as [tag] but raise [Not_found] in case of a detached
      head. *)

  val update_tag: t -> origin -> tag -> [`Ok | `Duplicated_tag] Lwt.t
  (** Change the current tag name. Fail if a tag with the same name
      already exists. The head is unchanged. *)

  val update_tag_force: t -> origin -> tag -> unit Lwt.t
  (** Same as [update_tag] but delete and update the tag if it already
      exists. *)

  val detach: t -> origin -> unit Lwt.t
  (** Detach the current branch (ie. it is not assiaciated to a tag
      anymore). *)

  (** {2 Heads} *)

  type head
  (** Type for head values. *)

  val of_head: head -> t
  (** Create a temporary detached branch, which will not persist in
      the database as it has no associated persistent tag name. *)

  val head: t -> origin -> head option Lwt.t
  (** Return the head commit. Might block if the branch is persistent
      as it needs to lookup some tag contents. *)

  val head_exn: t -> origin -> head Lwt.t
  (** Same as [read_head] but raise [Not_found] if the commit does not
      exist. *)

  val heads: t -> origin -> head list Lwt.t
  (** The list of all the databse heads. *)

  val update_head: t -> origin -> head -> unit Lwt.t
  (** Set the commit head. *)

  val merge_head: t -> origin -> head -> unit Ir_merge.result Lwt.t
  (** Merge a commit with the current branch. *)

  val watch_head: t -> origin -> key -> (key * head) Lwt_stream.t
  (** Watch changes for given key and the one it has recursive access.
      Return the stream of heads of the modified keys. *)

  (** {2 Functions over stores} *)

  val clone: t -> origin -> tag -> [`Ok of t | `Duplicated_tag] Lwt.t
  (** Fork the store, using the given branch name. Return [None] if
      the branch already exists. *)

  val clone_force: t -> origin -> tag -> t Lwt.t
  (** Same as [clone] but delete and update the existing branch if a
      branch with the same name already exists. *)

  val switch: t -> origin -> tag -> unit Lwt.t
  (** Switch the database contents the be same as the contents of the
      given branch name. The two branches are still independant. *)

  val merge: t -> origin -> tag -> unit Ir_merge.result Lwt.t
  (** [merge db t] merges the branch [t] into the current database
      branch. The two branches are still independant. *)

  module T: Tc.I0 with type t = t
  (** Base functions over values of type [t]. *)

  (** {2 Slices} *)

  type slice
  (** Type for database slices. *)

  module Slice: Tc.I0 with type t = slice
  (** Base functions over slices. *)

  val export: ?full:bool -> ?depth:int -> ?min:head list -> ?max:head list ->
    t -> origin -> slice Lwt.t
  (** [export t origin ~depth ~min ~max] exports the database slice
      between [min] and [max], using at most [depth] history depth
      (starting from the max).

      If [max] is not specified, use the current [heads]. If [min] is
      not specified, use an unbound past (but can be still limited by
      [depth]).

      [depth] is used to limit the depth of the commit history. [None]
      here means no limitation.

      If [full] is set (default is true) the full graph, including the
      commits, nodes and contents, is exported, otherwise it is the
      commit history graph only. *)

  val import: t -> origin -> slice -> [`Ok | `Duplicated_tags of tag list] Lwt.t
  (** Import a database slide. Do not modify existing tags. *)

  val import_force: t -> origin -> slice -> unit Lwt.t
  (** Same as [import] but delete and update the tags they already
      exist in the database. *)

end

module type MAKER =
  functor (B: Ir_block.STORE) ->
  functor (T: Ir_tag.STORE with type value = B.head and type origin = B.origin)
    -> STORE with type key = B.step list
              and type value = B.contents
              and type origin = B.origin
              and type tag = T.key
              and type head = B.head
(** Signature of functors to create branch-consistent stores. *)

module Make: MAKER
(** Build a branch consistent store from custom [Block] and [Tag]
    store implementations. *)

module type STORE_EXT = sig

  (** Same as [S] but also expose the store internals. Useful to build
      derived functionalities. *)

  type step

  include STORE with type key = step list

  module Block: Ir_block.STORE
    with type step = step
     and type contents = value
     and type origin = origin
     and type head = head
  (** The internal block store. *)

  module Tag: Ir_tag.STORE
    with type key = tag and type value = head and type origin = origin
  (** The internal tag store. *)

  module Key: Tc.I0 with type t = Block.step list
  (** Base functions over keys. *)

  module Val: Ir_contents.S with type t = value
  (** Base functions over values. *)

  module Origin: Ir_origin.S with type t = origin
  (** Base functions over origins. *)

  (** {2 Nodes} *)

  val read_node: t -> origin -> key -> Block.Node.value option Lwt.t
  (** Read a node. *)

  val mem_node: t -> origin -> key -> bool Lwt.t
  (** Check whether a node exists. *)

  val update_node: t -> origin -> key -> Block.Node.value -> unit Lwt.t
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
  functor (T: Ir_tag.STORE with type value = B.head and type origin = B.origin)
    -> STORE_EXT with type step = B.step
                  and type value = B.contents
                  and type origin = B.origin
                  and type tag = T.key
                  and type head = B.head
                  and module Block = B
                  and module Tag = T
(** Signature of functors to create extended branch-consistent
    stores. *)

module Make_ext: MAKER_EXT
(** Build an extended branch consistent store from custom [Block] and
    [Tag] store implementations. *)
