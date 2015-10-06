(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  module Repo: sig
    type t
    val create: Ir_conf.t -> t Lwt.t
    val config: t -> Ir_conf.t
  end
  include Ir_rw.HIERARCHICAL
  val master: ('a -> Ir_task.t) -> Repo.t -> ('a -> t) Lwt.t
  val repo: t -> Repo.t
  val task: t -> Ir_task.t
  type branch_id
  val of_branch_id: 'a Ir_task.f -> branch_id -> Repo.t -> ('a -> t) Lwt.t
  val name: t -> branch_id option Lwt.t
  val name_exn: t -> branch_id Lwt.t
  val branches: t -> branch_id list Lwt.t
  val remove_branch: t -> branch_id -> unit Lwt.t
  val update_branch: t -> branch_id -> unit Lwt.t
  val merge_branch: t -> ?max_depth:int -> ?n:int -> branch_id -> unit Ir_merge.result Lwt.t
  val merge_branch_exn: t -> ?max_depth:int -> ?n:int -> branch_id -> unit Lwt.t
  type head
  val empty: 'a Ir_task.f -> Repo.t -> ('a -> t) Lwt.t
  val of_head: ('a -> Ir_task.t) -> head -> Repo.t -> ('a -> t) Lwt.t
  val head: t -> head option Lwt.t
  val head_exn: t -> head Lwt.t
  val head_ref: t -> [`Branch of branch_id | `Head of head | `Empty]
  val heads: t -> head list Lwt.t
  val update_head: t -> head -> unit Lwt.t
  val fast_forward_head: t -> ?max_depth:int -> ?n:int -> head -> bool Lwt.t
  val compare_and_set_head: t -> test:head option -> set:head option -> bool Lwt.t
  val merge_head: t -> ?max_depth:int -> ?n:int -> head -> unit Ir_merge.result Lwt.t
  val merge_head_exn: t -> ?max_depth:int -> ?n:int -> head -> unit Lwt.t
  val watch_head: t -> ?init:head -> (head Ir_watch.diff -> unit Lwt.t) ->
    (unit -> unit Lwt.t) Lwt.t
  val watch_branches: t -> ?init:(branch_id * head) list ->
    (branch_id -> head Ir_watch.diff -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
  val watch_key: t -> key -> ?init:(head * value) ->
    ((head * value) Ir_watch.diff -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
  val clone: 'a Ir_task.f -> t -> branch_id -> [`Ok of ('a -> t) | `Duplicated_branch | `Empty_head] Lwt.t
  val clone_force: 'a Ir_task.f -> t -> branch_id -> ('a -> t) Lwt.t
  val merge: 'a -> ?max_depth:int -> ?n:int -> ('a -> t) -> into:('a -> t) ->
    unit Ir_merge.result Lwt.t
  val merge_exn: 'a -> ?max_depth:int -> ?n:int -> ('a -> t) -> into:('a -> t) ->
    unit Lwt.t
  val lcas: 'a -> ?max_depth:int -> ?n:int -> ('a -> t) -> ('a -> t) ->
    [`Ok of head list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
  val lcas_branch: t -> ?max_depth:int -> ?n:int -> branch_id ->
    [`Ok of head list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
  val lcas_head: t -> ?max_depth:int -> ?n:int -> head ->
    [`Ok of head list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
  module History: Graph.Sig.P with type V.t = head
  val history: ?depth:int -> ?min:head list -> ?max:head list -> t -> History.t Lwt.t
  val task_of_head: t -> head -> Ir_task.t Lwt.t
  type slice
  val export: ?full:bool -> ?depth:int -> ?min:head list -> ?max:head list ->
    t -> slice Lwt.t
  val import: t -> slice -> [`Ok | `Error] Lwt.t
end

module type MAKER =
  functor (C: Ir_contents.S) ->
  functor (R: Ir_tag.S) ->
  functor (H: Ir_hash.S) ->
    STORE with type key = C.Path.t
           and type value = C.t
           and type head = H.t
           and type branch_id = R.t

module type PRIVATE = sig
  module Contents: Ir_contents.STORE
  module Node: Ir_node.STORE
    with type Val.contents = Contents.key and module Path = Contents.Path
  module Commit: Ir_commit.STORE
    with type Val.node = Node.key
  module Ref: Ir_tag.STORE
    with type value = Commit.key
  module Slice: Ir_slice.S
    with type contents = Contents.key * Contents.value
     and type node = Node.key * Node.value
     and type commit = Commit.key * Commit.value
  module Sync: Ir_sync.S
    with type head = Commit.key and type branch_id = Ref.key
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
     and type Ref.key = branch_id
     and type Slice.t = slice

  val contents_t: t -> Private.Contents.t
  val node_t: t -> Private.Node.t
  val commit_t: t -> Private.Commit.t
  val ref_t: t -> Private.Ref.t

  (** {1 Nodes} *)

  val read_node: t -> key -> Private.Node.key option Lwt.t
  (** Read a node. *)

  val mem_node: t -> key -> bool Lwt.t
  (** Check whether a node exists. *)

  val update_node: t -> key -> Private.Node.key -> unit Lwt.t
  (** Update a node. *)

  val merge_node: t -> key -> (head * Private.Node.key) ->
    unit Ir_merge.result Lwt.t

  val remove_node: t -> key -> unit Lwt.t
  (** Remove a node. *)

  val iter_node: t -> Private.Node.key ->
    (key -> value Lwt.t -> unit Lwt.t) -> unit Lwt.t

end

module Make_ext (P: PRIVATE): STORE_EXT
  with type key = P.Contents.Path.t
   and type value = P.Contents.value
   and type branch_id = P.Ref.key
   and type head = P.Commit.key
   and type slice = P.Slice.t
   and module Key = P.Contents.Path
   and module Private = P
