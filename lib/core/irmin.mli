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

(** API entry point *)

exception Conflict of string
(** Merge conflict. *)

module type SNAPSHOTABLE = sig

  (** Snapshot/revert capabilities. *)

  type db
  (** Database handlers. *)

  type path
  (** Database paths. *)

  include IrminKey.S
  (** Snapshot states. *)

  val create: db -> t Lwt.t
  (** Snapshot the current state of the store. *)

  val update: db -> t -> unit Lwt.t
  (** Revert the store to a previous state. *)

  val merge: ?origin:IrminOrigin.t -> db -> t -> unit IrminMerge.result Lwt.t
  (** Merge the given snasphot into the current branch of the
      database. *)

  val merge_exn: ?origin:IrminOrigin.t -> db -> t -> unit Lwt.t
  (** Same as [merge_snapshot] but raise a [Conflict] exception in
      case of conflict. *)

  val watch: db -> path -> (path * t) Lwt_stream.t
  (** Subscribe to the stream of modification events attached to a
      given path. Return an event for each modification of a
      subpath. *)

end

module type BRANCHABLE = sig

  (** Fork/join capabilities. *)

  type db
  (** Database handler. *)

  include IrminReference.S
  (** Branch values. *)

  val create: db -> t -> db option Lwt.t
  (** Fork the store, using the given branch name. Return [None] if
      the branch already exists. *)

  val create_force: db -> t -> db Lwt.t
  (** Same as [create] but delete and update the existing branch if
      a branch with the same name already exists. *)

  val mem: db -> t -> bool Lwt.t
  (** Check whether a branch exists. *)

  val list: db -> t list Lwt.t
  (** Return the list of branches. *)

  val current: db -> t
  (** Get the current branch name. *)

  val merge: ?origin:IrminOrigin.t -> db -> t -> unit IrminMerge.result Lwt.t
  (** [merge db t] merges the branch [t] into the current database
      branch. *)

  val merge_exn: ?origin:IrminOrigin.t -> db -> t -> unit Lwt.t
  (** Same as [merge] but raise [Conflict "<msg>"] in case of a
      conflict. *)

end

module type DUMPABLE = sig

  (** Import/export capabilities. *)

  type db
  (** Database handlers. *)

  type snapshot
  (** Database snapshots. *)

  include IrminDump.S
  (** Database contents. *)

  val create: db -> snapshot list -> t Lwt.t
  (** [create t last] returns the new contents stored in [t] since the
      last [state] snaphots has been taken. If no previous snapshots
      are provided, return the full contents of the store. *)

  val update: db -> t -> unit Lwt.t
  (** [update t dump] imports the contents of [dump] in the
      database. This replace the current branch with the imported
      contents.  *)

  val merge: ?origin:IrminOrigin.t -> db -> t -> unit IrminMerge.result Lwt.t
  (** Same as [update] but merge with the current branch. *)

  val merge_exn: ?origin:IrminOrigin.t -> db -> t -> unit Lwt.t
  (** Same as [merge] but merge raise an exception in case of conflict. *)

  val output: db -> string -> unit Lwt.t
  (** Create a Graphviz graph representing the store state. Could be
      no-op if the backend does not support that operation (for instance,
      for remote connections). *)

end

module type VIEWABLE = sig

  (** Signature for databases able to translate paths into in-memory
      views. *)

  type db
  (** Database handler. *)

  type path
  (** Database path. *)

  include IrminView.S
  (** Database view. *)

  val of_path: db -> path -> t Lwt.t
  (** Read a view from a path in the store. This is a cheap operation,
      all the real reads operation will be done on-demand when the
      view is used. *)

  val update_path: ?origin:IrminOrigin.t -> db -> path -> t -> unit Lwt.t
  (** Commit a view to the store. The view *replaces* the current
      subtree, so if you want to do a merge, you have to do it
      manually (by creating a new branch, or rebasing before
      commiting). [origin] helps keeping track of provenance. *)

  val merge_path: ?origin:IrminOrigin.t -> db -> path -> t -> unit IrminMerge.result Lwt.t
  (** Same as [update_view] but *merges* with the current subtree. *)

  val merge_path_exn: ?origin:IrminOrigin.t -> db -> path -> t -> unit Lwt.t
  (** Same as [merge_view] but throw [Conflict "msg"] in case of
      conflict. *)

end

module type S = sig

  (** {2 Main signature for Irminsule stores} *)

  type key = string list
  (** Keys are path in the prefix tree. *)

  type value
  (** User-defined contents. *)

  include IrminStore.RW with type key   := key
                         and type value := value
  (** On an high-level view, Irminsule exposes the same interface as a
      low-level mutable store where keys are paths in the prefix tree
      and values are defined by the user. *)

  (** {2 Block store} *)

  module Block: IrminValue.STORE with type contents = value
  (** Append-only persistent block store where leafs are user-defined
      contents. *)

  val block: t -> Block.t
  (** Return an handler to the internal store. *)

  (** {2 Tag store} *)

  module Tag: IrminReference.STORE with type value = Block.key
  (** Read/write store for branch pointers. *)

  val tag: t -> Tag.t
  (** Return an handler to the reference store. *)

  (** {2 Snapshots} *)

  module Snapshot: SNAPSHOTABLE with type db   = t
                                 and type path = key

  (** {2 Branching} *)

  module Branch: BRANCHABLE with type db = t

  (** {2 Import/export} *)

  module Dump: DUMPABLE with type db       = t
                         and type key      = Block.key
                         and type contents = Block.contents
                         and type snapshot = Snapshot.t

  (** {2 Views} *)

  module View: VIEWABLE with type node  = Block.key
                         and type value = value

  (** {2 Extension to base functions} *)

  val create: ?branch:Branch.t -> unit -> t Lwt.t
  (** Create a store handle. The default branch (if not set) is
      [Branch.master]. *)

  val update: ?origin:IrminOrigin.t -> t -> key -> value -> unit Lwt.t
  (** Same as [IrminStore.RW.update] but with an optional [origin]
      argument to keep track of provenance. *)

  val remove: ?origin:IrminOrigin.t -> t -> key -> unit Lwt.t
  (** Same as [IrminStore.RW.remove] but with an optional [origin]
      argument to keep track of provenance. *)


  module Key: IrminKey.S with type t = key
  (** Base functions over keys. *)

  module Value: IrminContents.S with type t = value
  (** Base functions over values. *)

end

type ('key, 'value, 'ref) t =
  (module S with type Block.key = 'key
             and type value     = 'value
             and type Tag.key   = 'ref)

module Make
    (K : IrminKey.S)
    (C : IrminContents.S)
    (R : IrminReference.S)
    (Block: IrminValue.STORE     with type key = K.t and type contents = C.t)
    (Tag  : IrminReference.STORE with type key = R.t and type value = K.t)
  : S with type value = C.t
       and module Block = Block
       and module Tag   = Tag
(** Build a full iminsule store. *)

module Binary
    (K : IrminKey.S)
    (C : IrminContents.S)
    (R : IrminReference.S)
    (AO: IrminStore.AO_BINARY)
    (RW: IrminStore.RW_BINARY)
  : S with type value     = C.t
       and type Block.key = K.t
       and type Tag.key   = R.t
(** Create an irminsule store from binary store makers. Use only one
    append-only store for values, nodes and commits and a mutable
    store for the tags. *)
