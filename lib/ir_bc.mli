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

  val of_tag: tag -> t Lwt.t
  (** Create a store handle. Similar to [create], but use any tag name
      instead of the [master] tag. *)

  val tag: t -> origin -> tag option Lwt.t
  (** Return the branch of the given store handle. *)

  val tag_exn: t -> origin -> tag Lwt.t
  (** Same as [tag] but raise [Not_found] in case of a detached
      head. *)

  val update_tag: t -> origin -> tag -> unit Lwt.t
  (** Update the current tag name. *)

  val detach: t -> origin -> unit Lwt.t
  (** Detach the current branch (ie. it is not assiaciated to a tag
      anymore). *)

  (** {2 Heads} *)

  type head
  (** Type for head values. *)

  val of_head: head -> t Lwt.t
  (** Create a temporary branch, which will not have an associated tag
      name. *)

  val head: t -> origin -> head option Lwt.t
  (** Return the head commit. *)

  val head_exn: t -> origin -> head Lwt.t
  (** Same as [read_head] but raise [Not_found] if the commit does not
      exist. *)

  val update_head: t -> origin -> head -> unit Lwt.t
  (** Set the commit head. *)

  val merge_head: t -> origin -> head -> unit Ir_merge.result Lwt.t
  (** Merge a commit with the current branch. *)

  (** {2 Functions over stores} *)

  val clone: t -> origin -> tag -> t option Lwt.t
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

  val merge_exn: t -> origin -> tag -> unit Lwt.t
  (** Same as [merge] but raise [Conflict "<msg>"] in case of a
      conflict. *)

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

  module Value: Ir_contents.S with type t = value
  (** Base functions over values. *)

  module Origin: Ir_origin.S with type t = origin
  (** Base functions over origins. *)

  val commit_t: t -> Block.Commit.t
  (** Get the commit store handler. *)

  val node_t: t -> Block.Node.t
  (** Get the node store handler. *)

  val contents_t: t -> Block.Contents.t
  (** Get the contents store handler. *)

  val tag_t: t -> Tag.t
  (** Get the tag store handler. *)

  (** {2 Nodes} *)

  val read_node: t -> origin -> key -> Block.Node.value option Lwt.t
  (** Read a node. *)

  val update_node: t -> origin -> key -> Block.Node.value -> unit Lwt.t
  (** Update a node. *)

  val watch_node: t -> origin -> key -> (key * Block.Node.key) Lwt_stream.t
  (** Watch commit changes. Return the stream of commit
      identifiers. *)

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

module Make_ext (Block: Ir_block.MAKER) (Tag: Ir_tag.MAKER) (U: Ir_uid.S): MAKER
(** Build an extended branch consistent store from custom [Block] and
    [Tag] store implementations. *)
