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

module type STORE = sig

  (** A branch-consistent store is a mutable store which supports
      fork/join operations. *)

  include RW with type key = IrminPath.t

  type branch
  (** Branch names. *)

  val create: ?branch:branch -> unit -> t
  (** Create a store handle. The default branch (if not set) is
      [Branch.master]. *)

  val branch: t -> branch
  (** Return the branch of the given store handle. *)

  val with_branch: t -> branch -> t
  (** Return a new store handle with a new branch. *)

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

  (** {2 Lower level functions} *)

  module Block: IrminBlock.STORE with type contents = value
  (** Append-only persistent block store where leafs are user-defined
       contents. *)

  module Tag: IrminTag.STORE with type key = branch and type value = Block.key
  (** Read/write store for branch pointers. *)

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

  val read_node: t -> key -> Block.node option Lwt.t
  (** Read a node. *)

  val update_node: t -> origin -> key -> Block.node -> unit Lwt.t
  (** Update a node. *)

  val watch_node: t -> key -> (key * Block.key) Lwt_stream.t
  (** Watch node changes. Return the stream of changing sub-nodes. *)

  val update_commit: t -> Block.key -> unit Lwt.t
  (** Set the current branch to point to the given commit. *)

  val merge_commit: t -> ?origin:origin -> Block.key -> unit IrminMerge.result Lwt.t
  (** Merge a commit in the current branch. *)

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
  : STORE with type branch  = Tag.key
           and type value   = Block.contents
           and module Block = Block
           and module Tag   = Tag
(** Build a branch consistent store from custom Block] and [Tag] store
    implementations. *)

module type MAKER =
  functor (K: IrminKey.S) ->
  functor (C: IrminContents.S) ->
  functor (T: IrminTag.S) ->
    STORE with type Block.key = K.t
           and type value     = C.t
           and type branch    = T.t
(** Branch-consistent store maker. *)
