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
  module Io : Io.S

  type t

  val create_rw :
    path:string ->
    overwrite:bool ->
    auto_flush_threshold:int ->
    auto_flush_callback:(unit -> unit) ->
    (t, [> Io.create_error ]) result
  (** Create a rw instance of [t] by creating the file. *)

  val open_rw :
    path:string ->
    end_offset:int63 ->
    dead_header_size:int ->
    auto_flush_threshold:int ->
    auto_flush_callback:(unit -> unit) ->
    (t, [> Io.open_error ]) result
  (** Create a rw instance of [t] by opening an existing file at [path].

      {3 [dead_header_size]}

      Designates a small area at the beginning of the file that should be
      ignored. The offsets start after that area.

      The actual persisted size of a file is [end_offset + dead_header_size].

      This concept exists in order to keep supporting [`V1] and [`V2] pack
      stores with [`V3]. *)

  val open_ro :
    path:string ->
    end_offset:int63 ->
    dead_header_size:int ->
    (t, [> Io.open_error ]) result
  (** Create a ro instance of [t] by opening an existing file at [path] *)

  (* val inject : t -> (unit -> unit) -> unit *)
  (** Finish constructing a rw instace of [t] by injecting a callback that will
      take care of auto flushes. *)

  val close : t -> (unit, [> Io.close_error ]) result

  val end_offset : t -> int63
  (** [end_offset t] is the number of bytes of the file. That function doesn't
      perform IO.

      {3 RW mode}

      It also counts the bytes not flushed yet.

      {3 RO mode}

      This information originates from the latest reload of the control file.
      Calling [refresh_end_offset t] updates [end_offset]. *)

  val read_exn : t -> off:int63 -> len:int -> bytes -> unit
  (** [read_exn t ~off ~len b] puts the [len] bytes of [t] at [off] to [b].

      Raises [Io.Read_error]

      {3 RW mode}

      Attempting to read from the append buffer results in an
      [`Read_out_of_bounds] error. This feature could easily be implemented in
      the future if ever needed. It was not needed with io_legacy.

      {3 RO mode}

      It is not possible to read from an offset further than [end_offset t]. *)

  val append_exn : t -> string -> unit
  (** [append_exn t ~off b] writes [b] to the end of [t]. Might trigger an auto
      flush.

      Post-condition: [end_offset t - end_offset (old t) = String.length b].

      Raises [Io.Write_error] *)

  val flush : t -> (unit, [> Io.write_error ]) result
  (** Flush the append buffer. Does not call [fsync]. *)

  val fsync : t -> (unit, [> Io.write_error ]) result
  (** Tell the os to fush its internal buffers. Does not call [flush]. *)

  val refresh_end_offset : t -> int63 -> (unit, [> `Rw_not_allowed ]) result
  (** Ingest the new end offset of the file. *)

  val readonly : t -> bool
end

module type Sigs = sig
  module type S = S

  (** Abstraction for irmin-pack's append only files (i.e. suffix and dict).

      It is parameterized with [IO], a file system abstraction (e.g. unix,
      mirage, eio_linux).

      It comprises a persistent file and an append buffer. *)
  module Make (Io : Io.S) : S with module Io = Io
end
