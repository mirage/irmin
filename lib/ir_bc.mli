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
  include Ir_rw.HIERARCHICAL
  type tag
  val of_tag: Ir_conf.t -> ('a -> Ir_task.t) -> tag -> ('a -> t) Lwt.t
  val tag: t -> tag option
  val tag_exn: t -> tag
  val tags: t -> tag list Lwt.t
  val rename_tag: t -> tag -> [`Ok | `Duplicated_tag] Lwt.t
  val update_tag: t -> tag -> unit Lwt.t
  val merge_tag: t -> tag -> unit Ir_merge.result Lwt.t
  val merge_tag_exn: t -> tag -> unit Lwt.t
  val switch: t -> tag -> unit Lwt.t
  type head
  val of_head: Ir_conf.t -> ('a -> Ir_task.t) -> head -> ('a -> t) Lwt.t
  val head: t -> head option Lwt.t
  val head_exn: t -> head Lwt.t
  val branch: t -> [`Tag of tag | `Head of head]
  val heads: t -> head list Lwt.t
  val detach: t -> unit Lwt.t
  val update_head: t -> head -> unit Lwt.t
  val merge_head: t -> head -> unit Ir_merge.result Lwt.t
  val merge_head_exn: t -> head -> unit Lwt.t
  val watch_head: t -> key -> (key * head) Lwt_stream.t
  val clone: ('a -> Ir_task.t) -> t -> tag -> [`Ok of ('a -> t) | `Duplicated_tag] Lwt.t
  val clone_force: ('a -> Ir_task.t) -> t -> tag -> ('a -> t) Lwt.t
  val merge: 'a -> ('a -> t) -> into:('a -> t) -> unit Ir_merge.result Lwt.t
  val merge_exn: 'a -> ('a -> t) -> into:('a -> t) -> unit Lwt.t
  val lca: 'a -> ('a -> t) -> ('a -> t) -> head list Lwt.t
  val lca_tag: t -> tag -> head list Lwt.t
  val lca_head: t -> head -> head list Lwt.t
  type slice
  val export: ?full:bool -> ?depth:int -> ?min:head list -> ?max:head list ->
    t -> slice Lwt.t
  val import: t -> slice -> unit Lwt.t
end

module type MAKER =
  functor (C: Ir_contents.S) ->
  functor (T: Ir_tag.S) ->
  functor (H: Ir_hash.S) ->
    STORE with type key = C.Path.t
           and type value = C.t
           and type head = H.t
           and type tag = T.t

module type PRIVATE = sig
  module Contents: Ir_contents.STORE
  module Node: Ir_node.STORE
    with type Val.contents = Contents.key and module Path = Contents.Path
  module Commit: Ir_commit.STORE
    with type Val.node = Node.key
  module Tag: Ir_tag.STORE
    with type value = Commit.key
  module Slice: Ir_slice.S
    with type contents = Contents.key * Contents.value
     and type node = Node.key * Node.value
     and type commit = Commit.key * Commit.value
  module Sync: Ir_sync.S
    with type head = Commit.key and type tag = Tag.key
end

module Make (X: Ir_ao.MAKER) (Y: Ir_rw.MAKER): MAKER

(** {1 Extended API} *)

module type STORE_EXT = sig

  (** Same as [S] but also expose the store internals. Useful to build
      derived functionalities. *)

  include STORE

  module Key: Ir_path.S with type t = key
  (** Base functions over keys. *)

  module Val: Ir_contents.S with type t = value
  (** Base functions over values. *)

  module Private: PRIVATE
    with type Contents.value = value
     and module Contents.Path = Key
     and type Commit.key = head
     and type Tag.key = tag
     and type Slice.t = slice

  val config: t -> Ir_conf.t
  val contents_t: t -> Private.Contents.t
  val node_t: t -> Private.Node.t
  val commit_t: t -> Private.Commit.t
  val tag_t: t -> Private.Tag.t

  (** {1 Nodes} *)

  val read_node: t -> key -> Private.Node.key option Lwt.t
  (** Read a node. *)

  val mem_node: t -> key -> bool Lwt.t
  (** Check whether a node exists. *)

  val update_node: t -> key -> Private.Node.key -> unit Lwt.t
  (** Update a node. *)

end

module Make_ext (P: PRIVATE): STORE_EXT
  with type key = P.Contents.Path.t
   and type value = P.Contents.value
   and type tag = P.Tag.key
   and type head = P.Commit.key
   and type slice = P.Slice.t
   and module Key = P.Contents.Path
   and module Private = P
