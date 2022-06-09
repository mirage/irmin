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
    ] )
    result

  val open_ro :
    Irmin.Backend.Conf.t ->
    (t, [> Io.open_error | Io.read_error | `Decoding_error ]) result

  val close : t -> (unit, [> Io.close_error | `Pending_flush | `Tmp ]) result
  val flush : t -> (unit, [> Io.write_error | `Tmp ]) result
  val flush_exn : t -> unit

  val reload :
    t ->
    (unit, [> Io.read_error | `Rw_not_allowed | `Decoding_error | `Tmp ]) result

  val reload_exn : t -> unit
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
