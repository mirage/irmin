(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

exception RO_not_allowed

module type Checkable = sig
  type 'a t
  type hash

  val integrity_check :
    offset:int63 ->
    length:int ->
    hash ->
    _ t ->
    (unit, [ `Wrong_hash | `Absent_value ]) result
end

(** This module type should be included where appropriate, with [type repo := repo] *)
module type Layers = sig
  type repo 

  (** Supported by irmin-pack.unix and irmin-pack.mem *)
  val get_config: repo -> Irmin.Backend.Conf.t

  type commit_hash_s := string

  (** The following triggers gc for the given hash, in irmin-pack.unix; in .mem, it does
      nothing *)
  val trigger_gc: repo -> commit_hash_s -> unit
end

(** [Irmin-pack]-specific extensions to the [Store] module type. *)
module type Specifics = sig
  type repo
  type commit

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

  val sync : repo -> unit
  (** [sync t] syncs a readonly pack with the files on disk. Raises
      [invalid_argument] if called by a read-write pack.*)

  val flush : repo -> unit
  (** [flush t] flush read-write pack on disk. Raises [RO_Not_Allowed] if called
      by a readonly instance.*)
end

module type S = sig
  include Irmin.Generic_key.S
  include Specifics with type repo := repo and type commit := commit

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

      val close : process -> unit
      (** [close snapshot] close the [snaphot] instance.*)
    end
  end

  include Layers with type repo := repo

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
