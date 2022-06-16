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

module type S = sig
  (** Abstraction that governs the lifetime of the various files that are part
      of a pack store (except the branch store).

      The file manager handles the files one by one and makes explicit all the
      interactions between them (except the index which is handled at a high
      level). *)

  module Io : Io.S
  module Control : Control_file.S
  module Dict : Append_only_file.S
  module Suffix : Append_only_file.S
  module Index : Pack_index.S

  type t

  val control : t -> Control.t
  val dict : t -> Dict.t
  val suffix : t -> Suffix.t
  val index : t -> Index.t

  val create_rw :
    overwrite:bool ->
    Irmin.Backend.Conf.t ->
    ( t,
      [> Io.create_error
      | Io.write_error
      | Io.open_error
      | Io.mkdir_error
      | `Not_a_directory of string ] )
    result

  val open_rw :
    Irmin.Backend.Conf.t ->
    ( t,
      [> `Corrupted_control_file
      | `Double_close
      | `File_exists of string
      | `Invalid_argument
      | `Invalid_layout
      | `Io_misc of Io.misc_error
      | `No_such_file_or_directory
      | `Not_a_directory of string
      | `Not_a_file
      | `Read_on_closed
      | `Read_out_of_bounds
      | `Ro_not_allowed
      | `Write_on_closed ] )
    result

  val open_ro :
    Irmin.Backend.Conf.t ->
    ( t,
      [> `Corrupted_control_file
      | `File_exists of string
      | `Io_misc of Io.misc_error
      | `Migration_needed
      | `No_such_file_or_directory
      | `Not_a_file
      | `Read_on_closed ] )
    result

  val close : t -> (unit, [> Io.close_error | `Pending_flush | `Tmp ]) result
  (** Close all the files.

      This call fails if the append buffers are not in a flushed stated. This
      situation will most likely never occur because the append buffers will
      contain data only during the scope of a batch function.

      After *)

  val flush : t -> (unit, [> Io.write_error | `Tmp ]) result

  val reload :
    t ->
    ( unit,
      [> `Corrupted_control_file
      | `Invalid_argument
      | `Io_misc of Io.misc_error
      | `Read_on_closed
      | `Read_out_of_bounds
      | `Rw_not_allowed
      | `Tmp ] )
    result

  val register_dict_consumer :
    t -> after_reload:(unit -> (unit, Io.read_error) result) -> unit

  val register_suffix_consumer : t -> after_flush:(unit -> unit) -> unit

  val version :
    root:string ->
    ( Import.Version.t,
      [> `Corrupted_control_file
      | `Corrupted_legacy_file
      | `Invalid_layout
      | `Io_misc of Io.misc_error
      | `No_such_file_or_directory
      | `Not_a_directory of string ] )
    result
end

module type Sigs = sig
  module type S = S

  module Make
      (Control : Control_file.S with module Io = Io.Unix)
      (Dict : Append_only_file.S with module Io = Control.Io)
      (Suffix : Append_only_file.S with module Io = Control.Io)
      (Index : Pack_index.S)
      (Errs : Errors.S with module Io = Control.Io) :
    S
      with module Io = Control.Io
       and module Control = Control
       and module Dict = Dict
       and module Suffix = Suffix
       and module Index = Index
end
