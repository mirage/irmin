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
open! Import

module Payload = struct
  module Upper = struct
    module V3 = struct
      type from_v1_v2_post_upgrade = { entry_offset_at_upgrade_to_v3 : int63 }
      [@@deriving irmin]
      (** [entry_offset_at_upgrade_to_v3] is the offset of the first entry that
          is known to have been created using [irmin_pack_version = `V2] or
          more. The entries before that point may be v1 entries. V1 entries need
          an entry in index because it is the only place their lenght is stored. *)

      type from_v3_gced = { suffix_start_offset : int63; generation : int }
      [@@deriving irmin]
      (** [suffix_start_offset] is 0 if the suffix file was never garbage
          collected. Otherwise it is the offset of the very first entry of the
          suffix file. Note that offsets in the suffix file are virtual. The
          garbage collections don't reset the offsets.

          [generation] is the number of past GCs. A suffix file, a prefix file
          and a mapping containing that integer in their filename exist. *)

      (** [From_v1_v2_post_upgrade] corresponds to a pack store that was
          upgraded to [`V3]. It contains infos related to backward
          compatibility. GCs are forbidden on it.

          [From_v3_no_gc_yet] corresponds to a pack store that was created using
          [`V3] code. It never underwent a GC.

          [From_v3_used_non_minimal_indexing_strategy] corresponds to a pack
          store that was created using [`V3] code. It never underwent a GC and
          it will never be possible to GC it because entries were pushed using a
          non-minimal indexing strategy.

          [From_v3_gced] is a store that was GCed at least once.

          The [T*] tags are provisional tags that the binary decoder is aware of
          and that may in the future be used to add features to the [`V3]
          payload. *)
      type status =
        | From_v1_v2_post_upgrade of from_v1_v2_post_upgrade
        | From_v3_no_gc_yet
        | From_v3_used_non_minimal_indexing_strategy
        | From_v3_gced of from_v3_gced
        | T1
        | T2
        | T3
        | T4
        | T5
        | T6
        | T7
        | T8
        | T9
        | T10
        | T11
        | T12
        | T13
        | T14
        | T15
      [@@deriving irmin]

      type t = {
        dict_end_poff : int63;
        suffix_end_poff : int63;
        status : status; (* must be last to allow extensions *)
      }
      [@@deriving irmin]
      (** The [`V3] payload of the irmin-pack control file. [`V3] is a major
          version. If [`V4] ever exists, it will have its own dedicated payload,
          but the [`V3] definition will still have to stick in the codebase for
          backward compatibilty of old pack stores.

          A store may only change its major version during an [open_rw] in
          [File_manager]. Note that upgrading a major version is the only reason
          why [open_rw] would modify files in an irmin-pack directory.

          For a given major version, the format of a payload may change, but
          only in a backward compatible way. I.e., all versions of irmin-pack
          should forever be able to decode a [`V3] control file, it allows for
          control file corruption and out-of-date code to be distinguishable.

          It is legal for the payload decoder to not fully consume the input
          buffer. Remaining bytes means that the definition of a payload was
          changed.

          {3 Fields}

          [dict_end_poff] is the offset in the dict file just after the last
          valid dict bytes. The next data to be pushed to the dict will be
          pushed at this offset.

          [suffix_end_poff] is similar to [dict_end_poff] but for the suffix
          file.

          [status] is a variant that encode the state of the irmin-pack
          directory. This field MUST be the last field of the record, in order
          to allow extensions *)
    end

    module V4 = struct
      type gced = {
        suffix_start_offset : int63;
        generation : int;
        latest_gc_target_offset : int63;
        suffix_dead_bytes : int63;
      }
      [@@deriving irmin]
      (** Similar to [from_v3_gced]. New fields:

          [latest_gc_target_offset] is the commit on which the latest gc was
          called on.

          [suffix_dead_bytes] is the number of bytes at the beginning of the
          suffix that should be considered unreachable after a GC. *)

      (** [From_v1_v2_post_upgrade] similar to [V3.From_v1_v2_post_upgrade]

          [No_gc_yet] corresponds to a pack store that was created using [`V3]
          or above. It never underwent a GC.

          [Used_non_minimal_indexing_strategy] corresponds to a pack store that
          was created using [`V3] or above. It never underwent a GC and it will
          never be possible to GC it because entries were pushed using a
          non-minimal indexing strategy.

          [Gced] is a [`V3] or [`V4] store that was GCed at least once.

          The [T*] tags are provisional tags that the binary decoder is aware of
          and that may in the future be used to add features to the [`V4]
          payload. *)
      type status =
        | From_v1_v2_post_upgrade of V3.from_v1_v2_post_upgrade
        | No_gc_yet
        | Used_non_minimal_indexing_strategy
        | Gced of gced
        | T1
        | T2
        | T3
        | T4
        | T5
        | T6
        | T7
        | T8
        | T9
        | T10
        | T11
        | T12
        | T13
        | T14
        | T15
      [@@deriving irmin]

      type t = {
        dict_end_poff : int63;
        appendable_chunk_poff : int63;
        upgraded_from_v3_to_v4 : bool;
        checksum : int63;
        chunk_start_idx : int;
        chunk_num : int;
        status : status; (* must be last to allow extensions *)
      }
      [@@deriving irmin]
      (** The same as {!V3.t}, with the following modifications:

          New fields

          - [upgraded_from_v3_to_v4] recalls if the store was originally created
            in [`V3].
          - [chunk_start_idx] is the index for the starting chunk of the suffix
          - [chunk_num] is the number of chunks in the suffix
          - [checksum] for storing a checksum of the payload
          - [appendable_chunk_poff] is a value used by the chunked suffix. See
            {!Chunked_suffix.S.appendable_chunk_poff} for more details.

          Removed fields

          - [suffix_end_poff] is replaced by [appendable_chunk_poff] *)
    end

    module V5 = struct
      type gced = {
        suffix_start_offset : int63;
        generation : int;
        latest_gc_target_offset : int63;
        suffix_dead_bytes : int63;
        mapping_end_poff : int63 option;
      }
      [@@deriving irmin]

      type status =
        | From_v1_v2_post_upgrade of V3.from_v1_v2_post_upgrade
        | No_gc_yet
        | Used_non_minimal_indexing_strategy
        | Gced of gced
        | T1
        | T2
        | T3
        | T4
        | T5
        | T6
        | T7
        | T8
        | T9
        | T10
        | T11
        | T12
        | T13
        | T14
        | T15
      [@@deriving irmin]

      type t = {
        dict_end_poff : int63;
        appendable_chunk_poff : int63;
        upgraded_from : int option;
        checksum : int63;
        chunk_start_idx : int;
        chunk_num : int;
        volume_num : int;
        status : status; (* must be last to allow extensions *)
      }
      [@@deriving irmin]
      (** The same as {!V4.t}, with the following modifications:

          New fields

          - [volume_num] stores the number of volumes in the lower layer.
          - [mapping_end_poff] stores the mapping file size (optional if missing
            or unknown after a migration from V4).

          Changed fields

          - Replaced [upgraded_from_v3_to_v4] with generic field [upgraded_from]
            to track version upgrades. Note, it is an [int option] since
            [Version.t option] has a bug somewhere in repr that needs further
            investigation. *)
    end

    type version = V3 of V3.t | V4 of V4.t | V5 of V5.t [@@deriving irmin]

    type raw_payload = Valid of version | Invalid of version
    [@@deriving irmin]

    module Latest = V5
  end

  module Volume = struct
    module V5 = struct
      type t = {
        start_offset : int63;
        end_offset : int63;
        mapping_end_poff : int63;
        checksum : int63;
      }
      [@@deriving irmin]
      (** The payload for a control file of a volume.

          Fields

          - [start_offset] is the global offset for the start of the volume's
            data. Used for routing reads.
          - [end_offset] is the global offset for the end of the volume's data.
            Used for routing reads.
          - [mapping_end_poff] is the end offset for the mapping file. Used when
            writing. *)
    end

    type version = V5 of V5.t [@@deriving irmin]

    type raw_payload = Valid of version | Invalid of version
    [@@deriving irmin]

    module Latest = V5
  end
end

module type S = sig
  (** Abstraction for an irmin-pack control file.

      It is parameterized with [Io], a file system abstraction (e.g. unix,
      mirage, eio_linux).

      None of the functions raise exceptions. *)

  module Io : Io.S

  type payload
  type raw_payload
  type t

  val create_rw :
    path:string ->
    tmp_path:string option ->
    overwrite:bool ->
    payload ->
    (t, [> Io.create_error | Io.write_error ]) result
  (** Create a rw instance of [t] by creating a control file. *)

  type open_error :=
    [ `Corrupted_control_file of string
    | `Io_misc of Io.misc_error
    | `No_such_file_or_directory of string
    | `Not_a_file
    | `Closed
    | `Unknown_major_pack_version of string ]

  val open_ :
    path:string ->
    tmp_path:string option ->
    readonly:bool ->
    (t, [> open_error ]) result
  (** Create a rw instance of [t] by reading an existing file at [path].
      [tmp_path] will be used by RW instances when updating it's content, it is
      not required for RO instances or RW instances which will never be updated. *)

  val close : t -> (unit, [> Io.close_error ]) result

  val read_payload :
    path:string -> (payload, [> open_error | Io.close_error ]) result
  (** [read_payload ~path] reads the payload at [path]. It is a convenient way
      to read the payload without needing to call {!open_}, {!payload},
      {!close}. *)

  val read_raw_payload :
    path:string -> (raw_payload, [> open_error | Io.close_error ]) result

  val payload : t -> payload
  (** [payload t] is the payload in [t].

      That function doesn't perform IO.

      {3 RW mode}

      [payload t] is the payload, as it was written to the file system.

      {3 RO mode}

      [payload t] is the [payload], as it was seen during [open_] or during the
      most recent [reload]. *)

  type reload_error := [ `Rw_not_allowed | open_error | Io.close_error ]

  val reload : t -> (unit, [> reload_error ]) result
  (** {3 RW mode}

      Always returns an error.

      {3 RO mode}

      Reread the file on disk.

      If the file changed since the last read, the payload in [t] is updated to
      match the content of the file. *)

  type move_error := [ `Sys_error of string ]

  type set_error :=
    [ `No_tmp_path_provided
    | Io.create_error
    | Io.write_error
    | move_error
    | Io.close_error ]

  val set_payload : t -> payload -> (unit, [> set_error ]) result
  (** {3 RW mode}

      Write a new payload on disk.

      {3 RO mode}

      Always returns an error. *)

  val readonly : t -> bool

  val fsync : t -> (unit, [> Io.write_error ]) result
  (** {3 RW mode}

      Tell the OS to fush its internal buffers.

      {3 RO mode}

      Always returns [Error `Ro_not_allowed]. *)
end

module type Upper = sig
  include
    S
      with type payload := Payload.Upper.Latest.t
       and type raw_payload := Payload.Upper.raw_payload
end

module type Volume = sig
  include
    S
      with type payload := Payload.Volume.Latest.t
       and type raw_payload := Payload.Volume.raw_payload
end

module type Sigs = sig
  module Payload = Payload

  module type S = S
  module type Upper = Upper
  module type Volume = Volume

  module Upper (Io : Io.S) : Upper with module Io = Io
  module Volume (Io : Io.S) : Volume with module Io = Io
end
