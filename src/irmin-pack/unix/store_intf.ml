(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

open! Import

(** [Irmin-pack-unix]-specific extensions to the [Store] module type. *)

module type S = sig
  (** An [irmin-pack-unix] store. This provides the common {!Irmin} interface
      with [irmin-pack-unix] specific extensions. *)

  include Irmin.Generic_key.S
  (** @inline *)

  (** {1 Integrity Check} *)

  val integrity_check :
    ?ppf:Format.formatter ->
    ?heads:commit list ->
    auto_repair:bool ->
    repo ->
    ( [> `Fixed of int | `No_error ],
      [> `Cannot_fix of string | `Corrupted of int ] )
    result
    Lwt.t
  (** Checks the integrity of the repository. if [auto_repair] is [true], will
      also try to fix the issues. [ppf] is a formatter for progressive
      reporting. [`Fixed] and [`Corrupted] report the number of fixed/corrupted
      entries. *)

  val integrity_check_inodes :
    ?heads:commit list ->
    repo ->
    ([> `No_error ], [> `Cannot_fix of string ]) result Lwt.t

  val traverse_pack_file :
    [ `Reconstruct_index of [ `In_place | `Output of string ]
    | `Check_index
    | `Check_and_fix_index ] ->
    Irmin.config ->
    unit

  val test_traverse_pack_file :
    [ `Reconstruct_index of [ `In_place | `Output of string ]
    | `Check_index
    | `Check_and_fix_index ] ->
    Irmin.config ->
    unit

  (** {1 Chunking} *)

  val split : repo -> unit
  (** [split t] starts a fresh chunk file for appending data. Only allowed on
      stores that allow gc.

      Raises [RO_Not_Allowed] if called by a readonly instance.

      Raises [Split_forbidden_during_batch] if called inside a batch operation.

      Raises [Multiple_empty_chunks] if no data was added to the currently-used
      chunk file.

      Raises [Split_disallowed] if {!is_split_allowed} is false.

      TODO: Detail exceptions raised. *)

  val is_split_allowed : repo -> bool
  (** [is_split_allowed repo] returns if split is supported. Currently returns
      the same value as {!Gc.is_allowed}. *)

  (** {1 Lower layer} *)

  val add_volume : repo -> unit
  (** [add_volume t] creates a new empty volume in the lower layer.

      Raises [RO_Not_Allowed] if called by a readonly instance.

      Raises [Add_volume_forbidden_during_gc] if called while a GC is running.

      Raises [Multiple_empty_volumes] if there is already an empty volume. *)

  (** {1 On-disk} *)

  val reload : repo -> unit
  (** [reload t] reloads a readonly pack with the files on disk. Raises
      [invalid_argument] if called by a read-write pack.*)

  val flush : repo -> unit
  (** [flush t] flush read-write pack on disk. Raises [RO_Not_Allowed] if called
      by a readonly instance.*)

  val create_one_commit_store : repo -> commit_key -> string -> unit Lwt.t
  (** [create_one_commit_store t key path] creates a new store at [path] from
      the existing one, containing only one commit, specified by the [key]. Note
      that this operation is blocking.

      It requires that the files existing on disk when the operation is
      launched, remain on disk until the operation completes. In particular, a
      Gc running in a different process could remove files from disk. *)

  (** {1 Garbage Collection} *)

  module Gc : sig
    type process_state =
      [ `Idle | `Running | `Finalised of Stats.Latest_gc.stats ]
    (** The state of the GC process after calling {!finalise_exn} *)

    (** {1 Low-level API} *)

    val start_exn : ?unlink:bool -> repo -> commit_key -> bool Lwt.t
    (** [start_exn] tries to start the GC process and returns true if the GC is
        launched. If a GC is already running, a new one is not started.

        The GC process will not be automatically finalised. The caller is
        responsible for calling {!finalise_exn}.

        If [unlink] is false then temporary files and files from the previous
        generation will be kept on disk after the GC finished. This option is
        useful for debugging. The default is [true].

        TODO: Detail exceptions raised. *)

    val finalise_exn : ?wait:bool -> repo -> process_state Lwt.t
    (** [finalise_exn ?wait repo] waits for the GC process to finish in order to
        finalise it. It returns the state of the GC process from the point of
        view of the function call; subsequent calls of [finalise_exn] after a
        return of [`Finalised] will return [`Idle].

        Finalising consists of mutating [repo] so that it points to the new file
        and to flush the internal caches that could be referencing GCed objects.
        What is done with the discarded data depends on {!behaviour}.

        If [wait = true] (the default), the call blocks until the GC process
        finishes. If [wait = false], finalisation will occur if the process has
        ended.

        If there are no running GCs, the call is a no-op and it returns [`Idle].

        TODO: Detail exceptions raised. *)

    (** {1 High-level API} *)

    type msg = [ `Msg of string ]
    (** Pretty-print error messages meant for informational purposes, like
        logging *)

    val run :
      ?finished:((Stats.Latest_gc.stats, msg) result -> unit Lwt.t) ->
      repo ->
      commit_key ->
      (bool, msg) result Lwt.t
    (** [run repo commit_key] attempts to start a GC process for a [repo] by
        discarding or archiving all data prior to [commit_key] (depending on
        {!behaviour}. If a GC process is already running, a new one will not be
        started.

        [run] will also finalise the GC process automatically. For more detailed
        control, see {!start_exn} and {!finalise_exn}.

        When the GC process is finalised, [finished] is called with the result
        of finalisation.

        To monitor progress of GC, see {!wait} or {!is_finished}.

        Returns whether a GC process successfully started or not.

        All exceptions that [Irmin_pack] knows how to handle are caught and
        returned as pretty-print error messages; others are re-raised. The error
        messages should be used only for informational purposes, like logging. *)

    val wait : repo -> (Stats.Latest_gc.stats option, msg) result Lwt.t
    (** [wait repo] blocks until GC is finished or is idle.

        If a GC finalises, its stats are returned.

        All exceptions that [Irmin_pack] knows how to handle are caught and
        returned as pretty-print error messages; others are re-raised. The error
        messages should be used only for informational purposes, like logging. *)

    val cancel : repo -> bool
    (** [cancel repo] aborts the current GC and returns [true], or returns
        [false] if no GC was running. *)

    val is_finished : repo -> bool
    (** [is_finished repo] is [true] if a GC is finished (or idle) and [false]
        if a GC is running for the given [repo]. *)

    val behaviour : repo -> [ `Archive | `Delete ]
    (** [behaviour repo] returns the behaviour that the GC will have during
        finalization.

        This depends on the presence of a lower layer in the store: if a lower
        layer is present, the GC will archive old data into that lower layer.
        Else, it will delete that data. *)

    val is_allowed : repo -> bool
    (** [is_allowed repo] returns true if a gc can be run on the store. *)

    val latest_gc_target : repo -> commit_key option
    (** [latest_gc_target] returns the commit key on which the latest, finished
        gc was called on. *)
  end

  (** {1 Snapshots} *)

  module Snapshot : sig
    type kinded_hash = Contents of hash * metadata | Node of hash
    [@@deriving irmin]

    type entry = { step : string; hash : kinded_hash } [@@deriving irmin]

    type inode_tree = {
      depth : int;
      length : int;
      pointers : (int * hash) list;
    }
    [@@deriving irmin]

    type v = Inode_tree of inode_tree | Inode_value of entry list
    [@@deriving irmin]

    type inode = { v : v; root : bool } [@@deriving irmin]

    type t = Inode of inode | Blob of Backend.Contents.Val.t
    [@@deriving irmin]

    val export :
      ?on_disk:[ `Path of string ] ->
      repo ->
      (t -> unit Lwt.t) ->
      root_key:Tree.kinded_key ->
      int Lwt.t
    (** [export ?on_disk repo f ~root_key] applies [f] to all inodes and
        contents in a rooted tree, with root specified by [root_key].

        The traversal requires an index to keep track of visited elements.

        - if [on_disk] is not specified, the index is in memory.
        - if [on_disk] is [`Path path], a temporary index is created at path.

        The traversal order is stable. In [Inode_tree], it is lexicographic on
        the [index] function (see {!Conf.inode_child_order}). In [Inode_value],
        it is lexicographic on the steps.

        [f] is called in post-order, that is [f] is first called on the leaves,
        and the last call to [f] is on the root designated by [root_key].

        The traversal skips objects that are structurally equal to objects that
        were already traversed. In other words, [export] internally uses a hash
        set in order to guarantee that all the objects passed to [f] don't hash
        the same way.

        Returns the total number of elements visited. *)

    module Import : sig
      type process

      val v : ?on_disk:[ `Path of string | `Reuse ] -> repo -> process
      (** [v ?on_disk repo] create a [snaphot] instance. The traversal requires
          an index to keep track of visited elements.

          - if [on_disk] is not specified, the index is in memory.
          - if [on_disk] is [`Path path], a temporary index is created at path.
          - if [on_disk] is [`Reuse] the store's index is reused. *)

      val save_elt : process -> t -> node_key Lwt.t
      (** [save_elt snapshot elt] saves [elt] to the store. *)

      val close : process -> repo -> unit
      (** [close snapshot] close the [snaphot] instance.*)
    end
  end

  (** {1 Statistics} *)

  val stats :
    dump_blob_paths_to:string option -> commit:commit -> repo -> unit Lwt.t

  (** {1 Internals} *)

  module Internal : sig
    (** Unstable internal API agnostic about the underlying storage. Use it only
        to implement or test inodes. *)

    module Io = Io.Unix
    module Errs : Io_errors.S with module Io = Io
    module Index : Pack_index.S with type key = hash

    module File_manager :
      File_manager.S
        with module Io = Io
         and module Errs = Errs
         and module Index = Index

    val file_manager : repo -> File_manager.t

    module Dict : Dict.S

    val dict : repo -> Dict.t

    module Dispatcher : Dispatcher.S

    val dispatcher : repo -> Dispatcher.t

    module XKey : Pack_key.S with type hash = Schema.Hash.t

    val suffix_commit_mem : repo -> XKey.t -> bool
    val suffix_node_mem : repo -> XKey.t -> bool
    val suffix_contents_mem : repo -> XKey.t -> bool
    val kill_gc : repo -> bool
  end
end

module S_is_a_store (X : S) : Irmin.Generic_key.S = X

module type Maker = sig
  type endpoint = unit

  include Irmin.Key.Store_spec.S

  module Make (Schema : Irmin.Schema.Extended) :
    S
    (* We can't have `with module Schema = Schema` here, since the Schema
       on the RHS contains more information than the one on the LHS. We _want_
       to do something like `with module Schema = (Schema : Irmin.Schema.S)`,
       but this isn't supported.

       TODO: extract these extensions as a separate functor argument instead. *)
      with type Schema.Hash.t = Schema.Hash.t
       and type Schema.Branch.t = Schema.Branch.t
       and type Schema.Metadata.t = Schema.Metadata.t
       and type Schema.Path.t = Schema.Path.t
       and type Schema.Path.step = Schema.Path.step
       and type Schema.Contents.t = Schema.Contents.t
       and type Schema.Info.t = Schema.Info.t
       and type contents_key = (Schema.Hash.t, Schema.Contents.t) contents_key
       and type node_key = Schema.Hash.t node_key
       and type commit_key = Schema.Hash.t commit_key
       and type Backend.Remote.endpoint = endpoint
end
with type ('h, _) contents_key = 'h Pack_key.t
 and type 'h node_key = 'h Pack_key.t
 and type 'h commit_key = 'h Pack_key.t

module type KV = sig
  type endpoint = unit
  type hash = Irmin.Schema.default_hash

  include Pack_key.Store_spec

  type metadata = Irmin.Metadata.None.t

  module Make (C : Irmin.Contents.S) :
    S
      with module Schema.Contents = C
       and type Schema.Metadata.t = metadata
       and type Backend.Remote.endpoint = endpoint
       and type Schema.Hash.t = hash
       and type contents_key = (hash, C.t) contents_key
       and type node_key = hash node_key
       and type commit_key = hash commit_key
       and type Schema.Path.step = string
       and type Schema.Path.t = string list
       and type Schema.Branch.t = string
end

module type Sigs = sig
  module type S = S
  module type Maker = Maker
  module type KV = KV
end
