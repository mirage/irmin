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
  include Irmin.Generic_key.S

  val integrity_check :
    ?ppf:Format.formatter ->
    auto_repair:bool ->
    repo ->
    ( [> `Fixed of int | `No_error ],
      [> `Cannot_fix of string | `Corrupted of int ] )
    result
  (** Checks the integrity of the repository. if [auto_repair] is [true], will
      also try to fix the issues. [ppf] is a formatter for progressive
      reporting. [`Fixed] and [`Corrupted] report the number of fixed/corrupted
      entries. *)

  val reload : repo -> unit
  (** [reload t] reloads a readonly pack with the files on disk. Raises
      [invalid_argument] if called by a read-write pack.*)

  val flush : repo -> unit
  (** [flush t] flush read-write pack on disk. Raises [RO_Not_Allowed] if called
      by a readonly instance.*)

  module Gc : sig
    (** GC *)

    type process_state =
      [ `Idle | `Running | `Finalised of Stats.Latest_gc.stats ]
    (** The state of the GC process after calling {!finalise_exn} *)

    (** {2 Low-level API} *)

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

        If [wait = true] (the default), the call blocks until the GC process
        finishes. If [wait = false], finalisation will occur if the process has
        ended.

        If there are no running GCs, the call is a no-op and it returns [`Idle].

        TODO: Detail exceptions raised. *)

    (** {2 High-level API} *)

    type msg = [ `Msg of string ]
    (** Pretty-print error messages meant for informational purposes, like
        logging *)

    val run :
      ?finished:((Stats.Latest_gc.stats, msg) result -> unit Lwt.t) ->
      repo ->
      commit_key ->
      (bool, msg) result Lwt.t
    (** [run repo commit_key] attempts to start a GC process for a [repo] by
        discarding all data prior to [commit_key]. If a GC process is already
        running, a new one will not be started.

        [run] will also finalise the GC process automaticlly. For more detailed
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

    val is_allowed : repo -> bool
    (** [is_allowed repo] returns true if a gc can be run on the store. *)
  end

  val integrity_check_inodes :
    ?heads:commit list ->
    repo ->
    ([> `Msg of string ], [> `Msg of string ]) result Lwt.t

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

  val stats :
    dump_blob_paths_to:string option -> commit:commit -> repo -> unit Lwt.t

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

module type Maker_persistent =
  Maker
    with type ('h, _) contents_key = 'h Pack_key.t
     and type 'h node_key = 'h Pack_key.t
     and type 'h commit_key = 'h Pack_key.t
