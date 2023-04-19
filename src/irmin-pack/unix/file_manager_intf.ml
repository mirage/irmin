(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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

open Import

module type S = sig
  (** Abstraction that governs the lifetime of the various files that are part
      of a pack store (except the branch store).

      The file manager (FM) handles the files one by one and makes explicit all
      the interactions between them (except the index which is handled from a
      high level API). This allows to gain confidence on SWMR consistency and
      crash consistency.

      {2 Two types of guarantees}

      Irmin_pack_unix supports the SWMR access scheme. It means that it is
      undefined for the files to be opened twice in RW mode by 2 FMs. It also
      means that it is undefined for a FM in RW mode to be used simultaneously
      from 2 different fibers.

      Irmin_pack_unix aims to be (crash) consistent (in the ACID sense). In case
      of a system failure (e.g. power outage), the files should be left in a
      consistent state for later uses.

      Ensuring SWMR consistency is easier than ensuring crash consistency
      because the of the OS's shared page cache.

      {2 Files mutations}

      Here are all the moments where the files managed may be mutated:

      - 1. During [create_rw].
      - 2. During [open_rw] if a major version upgrade is necessary.
      - 3. During the flush routines in file_manager.
      - 4. During a GC, from the worker.
      - 5. At the end of a GC, from the RW fiber, in [swap].
      - 6. During integrity check routines.

      1. 2. and 6. don't support SWMR and leave the store in an undefined state
      in case of crash.

      4. operates on files private to the worker. It doesn't necessitate to
      worry about crash concistency and SWMR.

      3. and 5. are highly critical. *)

  module Io : Io.S
  module Control : Control_file.Upper with module Io = Io
  module Dict : Append_only_file.S with module Io = Io
  module Suffix : Chunked_suffix.S with module Io = Io
  module Index : Pack_index.S
  module Errs : Io_errors.S with module Io = Io
  module Sparse : Sparse_file.S with module Io = Io
  module Lower : Lower.S with module Io = Io

  type t

  (** A series of getters to the underlying files managed. *)

  val control : t -> Control.t
  val dict : t -> Dict.t
  val suffix : t -> Suffix.t
  val index : t -> Index.t
  val prefix : t -> Sparse.t option
  val lower : t -> Lower.t option

  type create_error :=
    [ Io.create_error
    | Io.write_error
    | Io.open_error
    | Io.mkdir_error
    | `Corrupted_mapping_file of string
    | `Corrupted_control_file of string
    | `Double_close
    | `Unknown_major_pack_version of string
    | `Volume_missing of string
    | `Not_a_directory of string
    | `Multiple_empty_volumes
    | `Index_failure of string
    | `Sys_error of string
    | `No_tmp_path_provided ]

  val create_rw :
    overwrite:bool -> Irmin.Backend.Conf.t -> (t, [> create_error ]) result
  (** Create a rw instance of [t] by creating the files.

      Note on SWMR consistency: It is undefined for a reader to attempt an
      opening before [create_rw] is over.

      Note on crash consistency: Crashing during [create_rw] leaves the storage
      in an undefined state.

      Note on errors: If [create_rw] returns an error, the storage is left in an
      undefined state and some file descriptors might not be closed. *)

  type open_rw_error :=
    [ `Corrupted_control_file of string
    | `Corrupted_mapping_file of string
    | `Double_close
    | `Closed
    | `File_exists of string
    | `Index_failure of string
    | `Invalid_argument
    | `Invalid_layout
    | `Io_misc of Io.misc_error
    | `Migration_needed
    | `Migration_to_lower_not_allowed
    | `No_such_file_or_directory of string
    | `Not_a_directory of string
    | `Not_a_file
    | `No_tmp_path_provided
    | `Read_out_of_bounds
    | `Ro_not_allowed
    | `Sys_error of string
    | `V3_store_from_the_future
    | `Only_minimal_indexing_strategy_allowed
    | `Unknown_major_pack_version of string
    | `Index_failure of string
    | `Sys_error of string
    | `Inconsistent_store
    | `Volume_missing of string
    | `Multiple_empty_volumes
    | `Invalid_parent_directory
    | `Pending_flush ]

  val open_rw : Irmin.Backend.Conf.t -> (t, [> open_rw_error ]) result
  (** Create a rw instance of [t] by opening existing files.

      If the pack store has already been garbage collected, opening with a
      non-minimal indexing strategy will return an error.

      If [no_migrate = false] in the config, the store will undergo a major
      version upgrade if necessary.

      Note on SWMR consistency: It is undefined for a reader to attempt an
      opening during an [open_rw], because of major version upgrades.

      Note on crash consistency: If [open_rw] crashes during a major version
      upgrade, the storage is left in an undefined state. Otherwise the storage
      is unaffected.

      Note on errors: If [open_rw] returns an error during a major version
      upgrade, the storage is left in an undefined state. Otherwise the storage
      is unaffected. Anyhow, some file descriptors might not be closed. *)

  type open_ro_error :=
    [ `Corrupted_control_file of string
    | `Corrupted_mapping_file of string
    | `Io_misc of Io.misc_error
    | `Migration_needed
    | `No_such_file_or_directory of string
    | `Not_a_file
    | `Double_close
    | `Closed
    | `V3_store_from_the_future
    | `Index_failure of string
    | `Unknown_major_pack_version of string
    | `Inconsistent_store
    | `Invalid_argument
    | `Read_out_of_bounds
    | `Invalid_layout
    | `Volume_missing of string ]

  val open_ro : Irmin.Backend.Conf.t -> (t, [> open_ro_error ]) result
  (** Create a ro instance of [t] by opening existing files.

      Note on SWMR consistency: [open_ro] is supposed to work whichever the
      state of the pack store and the writer, with 2 exceptions: 1. the files
      must exist and [create_rw] must be over and 2. during a major version
      upgrade of the files (which occurs during a [open_rw]).

      Note on crash consistency: Crashes during [open_ro] cause no issues
      because it doesn't mutate the storage.

      Note on errors: The storage is never mutated. Some file descriptors might
      not be closed. *)

  type close_error :=
    [ `Double_close
    | `Index_failure of string
    | `Io_misc of Io.misc_error
    | `Pending_flush
    | `Ro_not_allowed ]

  val close : t -> (unit, [> close_error ]) result
  (** Close all the files.

      This call fails if the append buffers are not in a flushed stated. This
      situation will most likely never occur because the append buffers will
      contain data only during the scope of a batch function. *)

  type flush_error :=
    [ `Index_failure of string
    | `Io_misc of Io.misc_error
    | `Ro_not_allowed
    | `Closed
    | `Double_close
    | `File_exists of string
    | `Sys_error of string
    | `No_tmp_path_provided ]

  type flush_stages := [ `After_dict | `After_suffix ]
  type 'a hook := 'a -> unit

  val flush : ?hook:flush_stages hook -> t -> (unit, [> flush_error ]) result
  (** Execute the flush routine. Note that this routine may be automatically
      triggered when buffers are filled. *)

  type fsync_error := flush_error

  val fsync : t -> (unit, [> fsync_error ]) result
  (** [fsync] executes an fsync for all files of the file manager.

      Note: This function exists primarily for operations like snapshot imports.
      If fsync is enabled for the store (see {!Irmin_pack.Config.use_fsync}),
      calls to {!flush} will also call fsync and therefore there is little need
      to call this function directly. *)

  type reload_stages := [ `After_index | `After_control | `After_suffix ]

  val reload : ?hook:reload_stages hook -> t -> (unit, [> Errs.t ]) result
  (** Execute the reload routine.

      Is a no-op if the control file did not change. *)

  val register_dict_consumer :
    t -> after_reload:(unit -> (unit, Errs.t) result) -> unit

  val register_prefix_consumer :
    t -> after_reload:(unit -> (unit, Errs.t) result) -> unit

  val register_suffix_consumer : t -> after_flush:(unit -> unit) -> unit

  type version_error :=
    [ `Corrupted_control_file of string
    | `Corrupted_legacy_file
    | `Invalid_layout
    | `Io_misc of Io.misc_error
    | `No_such_file_or_directory of string
    | `Not_a_directory of string
    | `Unknown_major_pack_version of string ]

  val version : root:string -> (Import.Version.t, [> version_error ]) result
  (** [version ~root] is the version of the pack stores at [root]. *)

  val cleanup : t -> (unit, [> `Sys_error of string ]) result
  (** [cleanup t] performs cleanup operations for files related to GC. *)

  val swap :
    t ->
    generation:int ->
    mapping_size:int63 ->
    suffix_start_offset:int63 ->
    chunk_start_idx:int ->
    chunk_num:int ->
    suffix_dead_bytes:int63 ->
    latest_gc_target_offset:int63 ->
    volume:Lower.volume_identifier option ->
    (unit, [> Errs.t ]) result
  (** Swaps to using files from the GC [generation]. The values
      [suffix_start_offset], [chunk_start_idx], [chunk_num], and
      [suffix_dead_bytes] are used to properly load and read the suffix after a
      GC. The value [volume] is used to reload the lower if it was modified by
      the GC. The control file is also updated on disk. *)

  val readonly : t -> bool
  val generation : t -> int
  val gc_allowed : t -> bool
  val split : t -> (unit, [> Errs.t ]) result
  val add_volume : t -> (unit, [> Errs.t ]) result

  val gc_behaviour : t -> [ `Delete | `Archive ]
  (** Decides if the GC will delete or archive the garbage data, depending on
      the presence of a lower layer. *)

  val gc_destination : t -> [ `Delete | `Archive of Lower.t ]
  (** Returns where data discarded by the GC will end up. (see {!gc_behaviour}). *)

  val create_one_commit_store :
    t ->
    Irmin.Backend.Conf.t ->
    Control_file.Payload.Upper.Latest.gced ->
    Index.key Pack_key.t ->
    (unit, [> open_rw_error | close_error ]) result
  (** [create_one_commit_store t conf generation new_store_root key] is called
      when creating a new store at [new_store_root] from the existing one,
      containing only one commit, specified by the [key]. Ths new store will use
      configuration options from [conf] and set to [generation]. *)
end

module type Sigs = sig
  module type S = S

  module Make
      (Io : Io.S)
      (Index : Pack_index.S with module Io = Io)
      (Errs : Io_errors.S with module Io = Io) :
    S with module Io = Io and module Index = Index and module Errs = Errs
end
