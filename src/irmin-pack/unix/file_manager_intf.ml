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
      | `Not_a_directory ] )
    result

  val open_rw :
    Irmin.Backend.Conf.t ->
    ( t,
      [> Io.open_error
      | Io.close_error
      | Io.read_error
      | Io.write_error
      | `Not_a_directory
      | `Invalid_layout
      | `Decoding_error
      | `Corrupted_legacy_file
      | `File_exists ] )
    result

  val open_ro :
    Irmin.Backend.Conf.t ->
    ( t,
      [> Io.open_error | Io.read_error | `Decoding_error | `File_exists ] )
    result

  val close : t -> (unit, [> Io.close_error | `Pending_flush | `Tmp ]) result
  (** Close all the files.

      This call fails if the append buffers are not in a flushed stated. This
      situation will most likely never occur because the append buffers will
      contain data only during the scope of a batch function.

      After *)

  val flush : t -> (unit, [> Io.write_error | `Tmp ]) result
  val flush_exn : t -> unit

  val reload :
    t ->
    (unit, [> Io.read_error | `Rw_not_allowed | `Decoding_error | `Tmp ]) result

  val reload_exn : t -> unit

  val register_dict_consumer :
    t -> after_reload:(unit -> (unit, Io.read_error) result) -> unit

  val register_suffix_consumer : t -> after_flush:(unit -> unit) -> unit
end

module type Sigs = sig
  module type S = S

  module Make
      (Control : Control_file.S with module Io = Io.Unix)
      (Dict : Append_only_file.S with module Io = Control.Io)
      (Suffix : Append_only_file.S with module Io = Control.Io)
      (Index : Pack_index.S) :
    S
      with module Io = Control.Io
       and module Control = Control
       and module Dict = Dict
       and module Suffix = Suffix
       and module Index = Index
end
