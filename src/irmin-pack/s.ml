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
  type key

  val integrity_check :
    offset:int63 ->
    length:int ->
    key ->
    _ t ->
    (unit, [ `Wrong_hash | `Absent_value ]) result
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

  val clear : repo -> unit Lwt.t
  (** [clear t] removes all the data persisted in [t]. This operations provides
      snapshot isolation guarantees for read-only instances: read-only instance
      will continue to see all the data until they explicitely call {!sync}. *)

  val migrate : Irmin.config -> unit
  (** [migrate conf] upgrades the repository with configuration [conf] to use
      the latest storage format.

      {b Note:} performing concurrent store operations during the migration, or
      attempting to use pre-migration instances of the repository after the
      migration is complete, will result in undefined behaviour. *)

  val flush : repo -> unit
  (** [flush t] flush read-write pack on disk. Raises [RO_Not_Allowed] if called
      by a readonly instance.*)
end

module type S = sig
  include Irmin.S
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
end

module S_is_a_store (X : S) : Irmin.S = X

module type Maker = sig
  type endpoint = unit

  include Irmin.Key.Store_spec.Hash_keyed

  module Make (Schema : Irmin.Schema.Extended) :
    S
    (* We can't have `with module Schema = Schema` here, since the Schema
       on the RHS contains more information than the one on the LHS. We _want_
       to do something like `with module Schema = (Schema : Irmin.Schema.S)`,
       but this isn't supported.

       TODO: extract these extensions as a separate functor argument instead. *)
      with type hash = Schema.Hash.t
       and type Schema.Branch.t = Schema.Branch.t
       and type Schema.Metadata.t = Schema.Metadata.t
       and type Schema.Path.t = Schema.Path.t
       and type Schema.Path.step = Schema.Path.step
       and type Schema.Contents.t = Schema.Contents.t
       and type Schema.Info.t = Schema.Info.t
       and type Backend.Remote.endpoint = endpoint
end
