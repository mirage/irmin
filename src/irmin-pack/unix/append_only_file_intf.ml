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
  (** Abstraction for irmin-pack's append only files (i.e. suffix and dict).

      It is parameterized with [Io], a file system abstraction (e.g. unix,
      mirage, eio_linux).

      It comprises a persistent file, an append buffer and take care of
      automatically shifting offsets to deal with legacy file headers. *)

  module Io : Io.S
  module Errs : Io_errors.S

  type t

  type auto_flush_procedure = [ `Internal | `External of t -> unit ]
  (** [auto_flush_procedure] defines behavior when the flush threshold is
      reached.

      - Use [`Internal] to have the buffer automatically flushed.
      - Use [`External f] to have [f] called when the flush threshold is
        reached. It is the responsibility of [f] to call flush, in addition to
        any other processing it does. *)

  val create_rw :
    path:string ->
    overwrite:bool ->
    auto_flush_threshold:int ->
    auto_flush_procedure:auto_flush_procedure ->
    (t, [> Io.create_error ]) result
  (** Create a rw instance of [t] by creating the file at [path]. *)

  val open_rw :
    path:string ->
    end_poff:int63 ->
    dead_header_size:int ->
    auto_flush_threshold:int ->
    auto_flush_procedure:auto_flush_procedure ->
    ( t,
      [> Io.open_error
      | `Closed
      | `Invalid_argument
      | `Read_out_of_bounds
      | `Inconsistent_store ] )
    result
  (** Create a rw instance of [t] by opening an existing file at [path].

      {3 End Offset}

      The file has an end offset at which new data will be saved. While this
      information could be computed by looking at the size of the file, we
      prefer storing that information elsewhere (i.e. in the control file). This
      is why [open_rw] and [open_ro] take an [end_poff] parameter, and also why
      [refresh_end_poff] exists. The abstractions above [Append_only_file] are
      responsible for reading/writing the offsets from/to the control file.

      {3 [dead_header_size]}

      Designates a small area at the beginning of the file that should be
      ignored. The offsets start after that area.

      The actual persisted size of a file is [end_poff + dead_header_size].

      This concept exists in order to keep supporting [`V1] and [`V2] pack
      stores with [`V3].

      {3 Auto Flushes}

      One of the goals of the [Append_only_file] abstraction is to provide
      buffered appends. [auto_flush_threshold] is the soft cap after which the
      buffer should be flushed. When a call to [append_exn] fills the buffer,
      either the buffer will be flushed automatically, if
      [auto_flush_procedure = `Internal], or the supplied external function [f]
      will be called, if [auto_flush_procedure = `External f]. *)

  val open_ro :
    path:string ->
    end_poff:int63 ->
    dead_header_size:int ->
    ( t,
      [> Io.open_error
      | `Closed
      | `Inconsistent_store
      | `Invalid_argument
      | `Read_out_of_bounds ] )
    result
  (** Create a ro instance of [t] by opening an existing file at [path] *)

  val close : t -> (unit, [> Io.close_error | `Pending_flush ]) result
  (** Close the underlying file.

      The internal buffer is expected to be in a flushed state when [close] is
      called. Otherwise, an error is returned. *)

  val end_poff : t -> int63
  (** [end_poff t] is the number of bytes of the file. That function doesn't
      perform IO.

      {3 RW mode}

      It also counts the bytes not flushed yet.

      {3 RO mode}

      This information originates from the latest reload of the control file.
      Calling [refresh_end_poff t] updates [end_poff]. *)

  val read_to_string :
    t -> off:int63 -> len:int -> (string, [> Io.read_error ]) result

  val read_exn : t -> off:int63 -> len:int -> bytes -> unit
  (** [read_exn t ~off ~len b] puts the [len] bytes of [t] at [off] to [b].

      [read_to_string] should always be favored over [read_exn], except when
      performences matter.

      It is not possible to read from an offset further than [end_poff t].

      Raises [Io.Read_error] and [Errors.Pack_error `Read_out_of_bounds].

      {3 RW mode}

      Attempting to read from the append buffer results in an
      [`Read_out_of_bounds] error. This feature could easily be implemented in
      the future if ever needed. It was not needed with io_legacy. *)

  val append_exn : t -> string -> unit
  (** [append_exn t ~off b] writes [b] to the end of [t]. Might trigger an auto
      flush.

      Grows [end_poff], but the parent abstraction is expected to persist this
      somewhere (e.g. in the control file).

      Post-condition: [end_poff t - end_poff (old t) = String.length b].

      Raises [Io.Write_error]

      {3 RW mode}

      Always raises [Errors.RO_not_allowed] *)

  val flush : t -> (unit, [> Io.write_error ]) result
  (** Flush the append buffer. Does not call [fsync].

      {3 RO mode}

      Always returns [Error `Ro_not_allowed]. *)

  val fsync : t -> (unit, [> Io.write_error ]) result
  (** Tell the os to fush its internal buffers. Does not call [flush].

      {3 RO mode}

      Always returns [Error `Ro_not_allowed]. *)

  val refresh_end_poff : t -> int63 -> (unit, [> `Rw_not_allowed ]) result
  (** Ingest the new end offset of the file. Typically happens in RO mode when
      the control file has been re-read.

      {3 RW mode}

      Always returns [Error `Rw_not_allowed]. *)

  val readonly : t -> bool
  val auto_flush_threshold : t -> int option
  val empty_buffer : t -> bool
  val path : t -> string
end

module type Sigs = sig
  module type S = S

  module Make (Io : Io.S) (Errs : Io_errors.S with module Io = Io) :
    S with module Io = Io and module Errs = Errs
end
