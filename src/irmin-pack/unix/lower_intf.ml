(*
 * Copyright (c) 2023 Tarides <contact@tarides.com>
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

module type Volume = sig
  module Io : Io.S
  module Errs : Io_errors.S

  type t

  type open_error =
    [ Io.open_error
    | `Closed
    | `Double_close
    | `Corrupted_control_file
    | `Unknown_major_pack_version of string ]

  val v : readonly:bool -> string -> (t, [> open_error ]) result
  (** [v ~readonly path] loads the volume at [path].

      If [readonly] is true, no write operations are allowed. *)

  val path : t -> string
  (** [path t] is the directory that contains the volume. *)

  val is_empty : t -> bool
  (** [is_empty t] returns whether [t] is empty or not. *)

  val control : t -> Control_file.Payload.Volume.Latest.t option
  (** [control t] returns the control file payload for the volume. *)
end

module type S = sig
  module Io : Io.S
  module Errs : Io_errors.S
  module Volume : Volume

  type t
  type open_error = [ Volume.open_error | `Volume_missing of string ]
  type close_error = [ | Io.close_error ]

  type add_error =
    [ open_error
    | `Ro_not_allowed
    | `Multiple_empty_volumes
    | `File_exists of string
    | `Invalid_parent_directory ]

  val v :
    readonly:bool -> volume_num:int -> string -> (t, [> open_error ]) result
  (** [v ~readonly ~volume_num lower_root] loads all volumes located in the
      directory [lower_root].

      [volume_num] is the number of volumes that are expected in [lower_root]. 0
      is valid for an empty lower.

      [Error `Volume_missing path] is returned if an expected volume is not on
      disk, with the path to first volume that is missing. This can happen if
      [volume_num] is larger than the number of volumes on disk, or if one of
      the volume directories has been renamed accidentally.

      If [readonly] is false, no write operations are allowed. *)

  val reload : volume_num:int -> t -> (unit, [> open_error ]) result
  (** [reload ~volume_num t] reloads volumes located in the root directory of
      [t], using [volume_num] as the expected number of volumes. *)

  val close : t -> (unit, [> close_error ]) result
  (** [close t] closes all resources opened by [t]. *)

  val volume_num : t -> int
  (** [volume_num t] returns the number of volumes in the lower [t]. *)

  val add_volume : t -> (Volume.t, [> add_error ]) result
  (** [add_volume t] adds a new empty volume to [t].

      If there is already an empty volume, [Error `Multiple_empty_volumes] is
      returned. Only one empty volume is allowed.

      If [t] is read-only, [Error `Ro_not_allowed] is returned. *)

  val find_volume : offset:int63 -> t -> Volume.t option
  (** [find_volume ~offset t] returns the {!Volume} that contains [offset]. *)
end

module type Sigs = sig
  module type S = S

  module Make_volume (Io : Io.S) (Errs : Io_errors.S with module Io = Io) :
    Volume with module Io = Io and module Errs = Errs

  module Make (Io : Io.S) (Errs : Io_errors.S with module Io = Io) :
    S with module Io = Io and module Errs = Errs
end
