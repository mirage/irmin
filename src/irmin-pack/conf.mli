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

type length_header = [ `Varint ] option

type inode_child_order =
  [ `Seeded_hash  (** use a non-crypto seeded-hash of the step *)
  | `Hash_bits  (** crypto hash the step and extract the relevant bits. *)
  | `Custom of depth:int -> bytes -> int  (** use a custom index *) ]

module type S = sig
  val entries : int
  (** The branching factor of the inode tree. 32 is a good choice for general
      applications. *)

  val stable_hash : int
  (** This offers a way to conditional base hashing on node entries instead of
      inodes. It is available for some backwards compatibility applications, but
      for most applications, this should be set to 0. *)

  val contents_length_header : length_header
  (** Describes the length header of the user's contents values when
      binary-encoded. Supported modes are:

      - [Some `Varint]: the length header is a LEB128-encoded integer at the
        very beginning of the encoded value.

      - [None]: there is no length header, and values have unknown size. NOTE:
        when using [irmin-pack] in this mode, the selected indexing strategy
        {i must} index all contents values (as recovering contents values from
        the store will require referring to the index for their length
        information). *)

  val inode_child_order : inode_child_order
  (** The strategy used for sorting entries. [`Hash_bits] is the recommended
      choice. [`Seeded_hash] is vunerable to attacks if storing user-generated
      keys. *)

  val forbid_empty_dir_persistence : bool
  (** If [true], irmin-pack raises [Failure] if it is asked to save the empty
      inode. This default is [false]. It should be set to [true] if the [Schema]
      of the store allows a hash collision between the empty inode and this
      string of length 1: ["\000"].

      See https://github.com/mirage/irmin/issues/1304 *)
end

val spec : Irmin.Backend.Conf.Spec.t

type merge_throttle = [ `Block_writes | `Overcommit_memory ] [@@deriving irmin]
(** Strategy for when attempting to write when the index log is full and waiting
    for an in-progress merge to complete.

    - [`Block_writes] will block writes
    - [`Overcommit_memory] will allow writes by growing the in-memory cache
      indefinitely *)

module Key : sig
  val fresh : bool Irmin.Backend.Conf.key
  val lru_size : int Irmin.Backend.Conf.key
  val lru_max_memory : int option Irmin.Backend.Conf.key
  val index_log_size : int Irmin.Backend.Conf.key
  val readonly : bool Irmin.Backend.Conf.key
  val root : string Irmin.Backend.Conf.key
  val lower_root : string option Irmin.Backend.Conf.key
  val merge_throttle : merge_throttle Irmin.Backend.Conf.key
  val indexing_strategy : Indexing_strategy.t Irmin.Backend.Conf.key
  val use_fsync : bool Irmin.Backend.Conf.key
  val dict_auto_flush_threshold : int Irmin.Backend.Conf.key
  val suffix_auto_flush_threshold : int Irmin.Backend.Conf.key
  val no_migrate : bool Irmin.Backend.Conf.key
end

val fresh : Irmin.Backend.Conf.t -> bool
(** Flag to indicate that the store will start with fresh data on disk. Warning:
    setting this to [true] will delete existing data. Default is [false]. *)

val lru_size : Irmin.Backend.Conf.t -> int
(** Maximum size, in number of entries, of LRU cache. Default [100_000]. Unused
    if {!lru_max_memory} is set. *)

val lru_max_memory : Irmin.Backend.Conf.t -> int option
(** Maximum memory, in bytes, for the LRU cache to use. Default [None], which
    falls back to {!lru_size} for LRU limit. *)

val index_log_size : Irmin.Backend.Conf.t -> int
(** Size, in number of entries, of index log. Default [2_500_000]. *)

val readonly : Irmin.Backend.Conf.t -> bool
(** Flag for opening data in read-only mode. Default [false]. *)

val merge_throttle : Irmin.Backend.Conf.t -> merge_throttle
(** Strategy for how to handle writes when index log is full and a merge is
    in-progress. Default [`Block_writes]. *)

val root : Irmin.Backend.Conf.t -> string
(** Location of directory for saving data on disk.

    Note: The path before the root directory must exist. Only the final
    directory in the path will be created if it is missing. *)

val lower_root : Irmin.Backend.Conf.t -> string option
(** Optional path for lower layer directory. Default [None].

    The presence or not of a lower layer has implications on the behaviour of
    the GC: if a lower layer is present, the GC will archive data instead of
    deleting it.*)

val indexing_strategy : Irmin.Backend.Conf.t -> Indexing_strategy.t
(** Strategy for choosing which objects to index. See {!Indexing_strategy.t} for
    more discussion. Default {!Indexing_strategy.default} *)

val use_fsync : Irmin.Backend.Conf.t -> bool
(** Flag to indicate that fsync should be used to enforce durability when
    flushing data to disk. Default [false]. *)

val dict_auto_flush_threshold : Irmin.Backend.Conf.t -> int
(** Size, in bytes, when automatic flushing of dict file to disk. Default
    [1_000_000]. *)

val suffix_auto_flush_threshold : Irmin.Backend.Conf.t -> int
(** Size, in bytes, when automatic flushing of suffix file to disk. Default
    [1_000_000]. *)

val no_migrate : Irmin.Backend.Conf.t -> bool
(** Flag to prevent migration of data. Default [false]. *)

val init :
  ?fresh:bool ->
  ?readonly:bool ->
  ?lru_size:int ->
  ?lru_max_memory:int option ->
  ?index_log_size:int ->
  ?merge_throttle:merge_throttle ->
  ?indexing_strategy:Indexing_strategy.t ->
  ?use_fsync:bool ->
  ?dict_auto_flush_threshold:int ->
  ?suffix_auto_flush_threshold:int ->
  ?no_migrate:bool ->
  ?lower_root:string option ->
  string ->
  Irmin.config
(** [init root] creates a backend configuration for storing data with default
    configuration parameters and stored at [root]. Flags are documented above. *)
