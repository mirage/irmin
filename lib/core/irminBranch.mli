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

module type STORE = sig

  (** A branch-consistent store is a mutable store which supports
      fork/join operations. *)

  include IrminStore.S with type key = IrminPath.t
                        and type origin = IrminOrigin.t

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

  val create_head: Block.key -> t Lwt.t
  (** Create a temporary branch, which will not be recorded in the tag
      branch. *)

  val head: t -> Block.key option Lwt.t
  (** Return the head commit. *)

  val head_exn: t -> Block.key Lwt.t
  (** Same as [read_head] but raise [Not_found] if the does not
      exist. *)

  val set_head: t -> Block.key -> unit
  (** Set the commit head. *)

  module Key: IrminKey.S with type t = key
  (** Base functions over keys. *)

  module Value: IrminContents.S with type t = value
  (** Base functions over values. *)

  module Branch: IrminTag.S with type t = branch
  (** Base functions over branches. *)

  module Graph: IrminGraph.S with type V.t = (Block.key, Tag.key) IrminGraph.vertex
  (** Object graph. *)

end

module type INTERNAL = sig

  (** Expose internal functions to be used by inside the library. *)

  include STORE

  (** {2 Lower level functions} *)

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

end

module Make
    (Block: IrminBlock.STORE)
    (Tag  : IrminTag.STORE with type value = Block.key)
  : INTERNAL with type value   = Block.contents
              and module Block = Block
              and type branch  = Tag.key
(** Build a branch consistent store from custom Block] and [Tag] store
    implementations. *)
