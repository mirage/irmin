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

(** Branch consistent stores: support fork/merge operations. *)

open IrminSig

module type S = sig

  (** A branch-consistent store is a mutable store which supports
      fork/join operations. *)

  include RW

  type branch
  (** Branch names. *)

  val create: ?branch:branch -> unit -> t Lwt.t
  (** Create a store handle. The default branch (if not set) is
      [Branch.master]. *)

  val current_branch: t -> branch Lwt.t
  (** Return the current branch. *)

  val update: t -> ?origin:origin -> key -> value -> unit Lwt.t
  (** Same as [IrminStore.RW.update] but with an optional [origin]
      argument to keep track of provenance. *)

  val remove: t -> ?origin:origin -> key -> unit Lwt.t
  (** Same as [IrminStore.RW.remove] but with an optional [origin]
      argument to keep track of provenance. *)

  val clone: t -> branch -> t option Lwt.t
  (** Fork the store, using the given branch name. Return [None] if the
      branch already exists. *)

  val clone_force: t -> branch -> t Lwt.t
  (** Same as [clone] but delete and update the existing branch if a
      branch with the same name already exists. *)

  val merge: t -> ?origin:origin -> branch -> unit IrminMerge.result Lwt.t
  (** [merge db t] merges the branch [t] into the current database
      branch. *)

  val merge_exn: t -> ?origin:origin -> branch -> unit Lwt.t
  (** Same as [merge] but raise [Conflict "<msg>"] in case of a
      conflict. *)

end

module type STORE = sig

  (** Same as [S] but exposes the lower level internal stores and
      their structure. *)

   module Block: IrminBlock.STORE
  (** Append-only persistent block store where leafs are user-defined
      contents. *)

  module Tag: IrminTag.STORE with type value = Block.key
  (** Read/write store for branch pointers. *)

  include S with type key = IrminPath.t
             and type branch = Tag.key
             and type value = Block.contents

  val block_t: t -> Block.t
  (** Return an handler to the internal store. *)

  val contents_t: t -> Block.Contents.t
  (** Return an handler to the internal contents store. *)

  val node_t: t -> Block.Node.t
  (** Return an hanlder to the internal node store. *)

  val commit_t: t -> Block.Commit.t
  (** Return an handler to the internal commit store. *)

  val tag_t: t -> Tag.t
  (** Return an handler to the reference store. *)

  val map_head_node: t -> key -> f:(Block.Node.t -> Block.node -> key -> 'a Lwt.t) -> 'a Lwt.t
  (** Apply a function from the node domain to the store's head
      node. *)

  val update_head_node: t -> origin:origin -> f:(Block.node -> Block.node Lwt.t) -> unit Lwt.t
  (** Update a the head node. *)

  val merge_commit: t -> ?origin:origin -> Block.key -> unit IrminMerge.result Lwt.t
  (** Merge a commit in the store's current branch. *)

  val watch_nodes: t -> key -> (key * Block.key) Lwt_stream.t
  (** Watch node changes. Return the stream of changing sub-nodes. *)

  module Key: IrminKey.S with type t = key
  (** Base functions over keys. *)

  module Value: IrminContents.S with type t = value
  (** Base functions over values. *)

  module Graph: IrminGraph.S with type V.t = (Block.key, Tag.key) IrminGraph.vertex
  (** Object graph. *)

end

module Make
    (Block: IrminBlock.STORE)
    (Tag  : IrminTag.STORE with type value = Block.key)
  : STORE with module Block = Block
           and module Tag   = Tag
(** Build a branch consistent store from user-defined [Contents] and
    using the provided [Block] and [Tag] store implementations. *)

module type MAKER =
  functor (K: IrminKey.S) ->
  functor (C: IrminContents.S) ->
  functor (T: IrminTag.S) ->
    S with type key = K.t and type value = C.t and type branch = T.t
