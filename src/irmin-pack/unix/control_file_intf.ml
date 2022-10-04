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

module Payload_v3 = struct
  type from_v1_v2_post_upgrade = { entry_offset_at_upgrade_to_v3 : int63 }
  [@@deriving irmin]
  (** [entry_offset_at_upgrade_to_v3] is the offset of the first entry that is
      known to have been created using [irmin_pack_version = `V2] or more. The
      entries before that point may be v1 entries. V1 entries need an entry in
      index because it is the only place their lenght is stored. *)

  type from_v3_gced = { suffix_start_offset : int63; generation : int }
  [@@deriving irmin]
  (** [suffix_start_offset] is 0 if the suffix file was never garbage collected.
      Otherwise it is the offset of the very first entry of the suffix file.
      Note that offsets in the suffix file are virtual. The garbage collections
      don't reset the offsets.

      [generation] is the number of past GCs. A suffix file, a prefix file and a
      mapping containing that integer in their filename exist. *)

  (** [From_v1_v2_post_upgrade] corresponds to a pack store that was upgraded to
      [`V3]. It contains infos related to backward compatibility. GCs are
      forbidden on it.

      [From_v3_no_gc_yet] corresponds to a pack store that was created using
      [`V3] code. It never underwent a GC.

      [From_v3_used_non_minimal_indexing_strategy] corresponds to a pack store
      that was created using [`V3] code. It never underwent a GC and it will
      never be possible to GC it because entries were pushed using a non-minimal
      indexing strategy.

      [From_v3_gced] is a store that was GCed at least once.

      The [T*] tags are provisional tags that the binary decoder is aware of and
      that may in the future be used to add features to the [`V3] payload. *)
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

  type t = { dict_end_poff : int63; suffix_end_poff : int63; status : status }
  [@@deriving irmin]
  (** The [`V3] payload of the irmin-pack control file. [`V3] is a major
      version. If [`V4] ever exists, it will have its own dedicated payload, but
      the [`V3] definition will still have to stick in the codebase for backward
      compatibilty of old pack stores.

      A store may only change its major version during an [open_rw] in
      [File_manager]. Note that upgrading a major version is the only reason why
      [open_rw] would modify files in an irmin-pack directory.

      For a given major version, the format of a payload may change, but only in
      a backward compatible way. I.e., all versions of irmin-pack should forever
      be able to decode a [`V3] control file, it allows for control file
      corruption and out-of-date code to be distinguishable.

      It is legal for the payload decoder to not fully consume the input buffer.
      Remaining bytes means that the definition of a payload was changed.

      {3 Fields}

      [dict_end_poff] is the offset in the dict file just after the last valid
      dict bytes. The next data to be pushed to the dict will be pushed at this
      offset.

      [suffix_end_poff] is similar to [dict_end_poff] but for the suffix file.

      [status] is a variant that encode the state of the irmin-pack directory.
      This field MUST be the last field of the record, in order to allow
      extensions *)
end

module Latest_payload = Payload_v3

module type S = sig
  (** Abstraction for irmin-pack's control file.

      It is parameterized with [Io], a file system abstraction (e.g. unix,
      mirage, eio_linux).

      None of the functions raise exceptions. *)

  module Io : Io.S

  type t

  val create_rw :
    path:string ->
    overwrite:bool ->
    Latest_payload.t ->
    (t, [> Io.create_error | Io.write_error ]) result
  (** Create a rw instance of [t] by creating a control file. *)

  type open_error :=
    [ `Corrupted_control_file
    | `Io_misc of Io.misc_error
    | `No_such_file_or_directory
    | `Not_a_file
    | `Closed
    | `Unknown_major_pack_version of string ]

  val open_ : path:string -> readonly:bool -> (t, [> open_error ]) result
  (** Create a rw instance of [t] by reading an existing file at [path]. *)

  val close : t -> (unit, [> Io.close_error ]) result

  val payload : t -> Latest_payload.t
  (** [payload t] is the payload in [t].

      That function doesn't perform IO.

      {3 RW mode}

      [payload t] is the payload, as it was written to the file system.

      {3 RO mode}

      [payload t] is the [payload], as it was seen during [open_] or during the
      most recent [reload]. *)

  type reload_error :=
    [ `Corrupted_control_file
    | `Io_misc of Io.misc_error
    | `Closed
    | `Rw_not_allowed
    | `Unknown_major_pack_version of string ]

  val reload : t -> (unit, [> reload_error ]) result
  (** {3 RW mode}

      Always returns an error.

      {3 RO mode}

      Reread the file on disk.

      If the file changed since the last read, the payload in [t] is updated to
      match the content of the file. *)

  val set_payload : t -> Latest_payload.t -> (unit, [> Io.write_error ]) result
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

module type Sigs = sig
  module Latest_payload = Payload_v3
  module Payload_v3 = Payload_v3

  module type S = S

  module Make (Io : Io.S) : S with module Io = Io
end
