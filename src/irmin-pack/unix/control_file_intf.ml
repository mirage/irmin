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

  type from_v3_gc_allowed = {
    entry_offset_suffix_start : int63;
    gc_generation : int;
  }
  [@@deriving irmin]
  (** [entry_offset_suffix_start] is 0 if the suffix file was never garbage
      collected. Otherwise it is the offset of the very first entry of the
      suffix file. Note that offsets in the suffix file are virtual. The garbage
      collections don't reset the offsets.

      [gc_generation] is the number of past GCs. A suffix file and a prefix file
      containing that integer in their name exist. *)

  (** [From_v1_v2_post_upgrade] corresponds to a pack store that was upgraded to
      [`V3]. It contains infos related to backward compatibility. GCs are
      forbidden on it.

      TODO: Change to something better for future upgrades.

      [from_v3_gc_disallowed] corresponds to a pack store that was created using
      [`V3] code. GCs are forbidden on it because it uses an indexing strategy.

      [from_v3_gc_allowed] corresponds to a pack store that was created using
      [`V3] code. GCs are possible on it, but the indexing strategy is
      constrained to minimal. *)
  type status =
    | From_v1_v2_post_upgrade of from_v1_v2_post_upgrade
    | From_v3_gc_disallowed
    | From_v3_gc_allowed of from_v3_gc_allowed
  [@@deriving irmin]

  type t = {
    dict_offset_end : int63;
    entry_offset_suffix_end : int63;
    status : status;
  }
  [@@deriving irmin]
  (** Payload of the control file for [irmin_pack_version = `V3].

      [dict_offset_end] is the offset in the dict file just after the last valid
      dict bytes. The next data to be pushed to the dict will be pushed at this
      offset.

      [entry_offset_suffix_end] is similar to [dict_offset_end] but for the
      suffix file. *)
end

module Latest_payload = Payload_v3

module type S = sig
  module Io : Io.S

  type t

  val create_rw :
    path:string ->
    overwrite:bool ->
    use_fsync:bool ->
    Latest_payload.t ->
    (t, [> Io.create_error | Io.write_error ]) result
  (** Create a rw instance of [t] by creating a control file. *)

  val open_rw :
    path:string ->
    use_fsync:bool ->
    (t, [> Io.open_error | Io.read_error | `Decoding_error ]) result
  (** Create a rw instance of [t] by reading an existing file at [path]. *)

  val open_ro :
    path:string ->
    (t, [> Io.open_error | Io.read_error | `Decoding_error ]) result
  (** Create a ro instance of [t] by reading an existing file at [path]. *)

  val close : t -> (unit, [> Io.close_error ]) result

  val payload : t -> Latest_payload.t
  (** [payload t] is the payload in [t].

      That function doesn't perform IO.

      {3 RW mode}

      [payload t] is the payload, as it was written to the file system.

      {3 RO mode}

      [payload t] is the [payload], as it was seen during [open_] or during the
      most recent [reload]. *)

  type reload_error := [ `Decoding_error | Io.reload_error | Io.read_error ]

  val reload : t -> (unit, [> reload_error ]) result
  (** Reread the file.

      {3 RW mode}

      Always returns an error.

      {3 RO mode}

      Reread the file on disk.

      If the file did not change since the last read, nothing happens.

      If the file changed since the last read, the payload in [t] is updated to
      match the content of the file. *)

  val set_payload : t -> Latest_payload.t -> (unit, [> Io.write_error ]) result
  (** {3 RW mode}

      Write a new payload on disk.

      {3 RO mode}

      Always returns an error. *)

  val readonly : t -> bool
end

module type Sigs = sig
  module Latest_payload = Payload_v3
  module Payload_v3 = Payload_v3

  module type S = S

  (** Abstraction for irmin-pack's control file.

      It is parameterized with [IO], a file system abstraction (e.g. unix,
      mirage, eio_linux).

      In rw mode, if [use_fsync = true], all changes to the control file during
      [open_rw], [create_rw] and [set_payload] are immediatly persisted to disk
      by asking the OS to do so. Otherwise, the OS may keep an in-memory buffer
      that may be either process-wise or OS-wise. Anyhow, there is no write
      buffer in the user space.

      None of the functions raise exceptions. *)
  module Make (Io : Io.S) : S with module Io = Io
end
