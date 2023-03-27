(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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

module type S = sig
  module Io : Io.S
  module Errs : Io_errors.S with module Io = Io

  type t
  type open_error := [ Io.open_error | `Corrupted_mapping_file of string ]

  val open_ro :
    mapping_size:int ->
    mapping:string ->
    data:string ->
    (t, [> open_error ]) result
  (** [open_ro ~mapping_size ~mapping ~data] returns a new read-only view of the
      sparse file, represented on disk by two files named [mapping] and [data].
      The mapping file is expected to have size at least [mapping_size] (and the
      rest is ignored if the file is larger). *)

  val close : t -> (unit, [> Io.close_error ]) result
  (** Close the underlying files. *)

  val read_exn : t -> off:int63 -> len:int -> bytes -> unit
  (** [read_exn t ~off ~len buffer] writes into [buffer] the bytes from [off] to
      [off+len]. *)

  val read_range_exn :
    t -> off:int63 -> min_len:int -> max_len:int -> bytes -> int
  (** Same as [read_exn], the amount read is [max_len] if possible or at least
      [min_len] if reading more would step over a hole in the sparse file.

      Returns the actually read length. *)

  val next_valid_offset : t -> off:int63 -> int63 option
  (** [next_valid_offset t ~off] returns [Some off'] such that [off'] is the
      smallest readable offset larger or equal to [off]. This enables jumping
      over a sparse hole to the next compact range of data. *)

  val iter : t -> (off:int63 -> len:int -> unit) -> (unit, Errs.t) result
  (** [iter t f] calls [f] on each [(off,len)] pair in [mapping]. Only used for
      testing and debugging.

      It is guaranteed for the offsets to be iterated in monotonic order.

      It is guaranteed that entries don't overlap.

      The exceptions raised by [f] are caught and returned (as long as they are
      known by [Errs]). *)

  module Wo : sig
    type t

    val open_wo :
      mapping_size:int ->
      mapping:string ->
      data:string ->
      (t, [> open_error ]) result
    (** [open_wo ~mapping_size ~mapping ~data] returns a write-only instance of
        the sparse file.

        Note: This is unsafe and is only used by the GC to mark the parent
        commits as dangling. One must ensure that no read-only instance is
        opened at the same time, as otherwise the writes would be observable by
        it. *)

    val write_exn : t -> off:int63 -> len:int -> string -> unit
    (** [write_exn t ~off ~len str] writes the first [len] bytes of [str] to [t]
        at the virtual offset [off]. *)

    val fsync : t -> (unit, [> Io.write_error ]) result
    (** [fsync t] persists to the file system the effects of previous writes. *)

    val close : t -> (unit, [> Io.close_error ]) result
    (** Close the underlying files. *)

    val create_from_data :
      mapping:string ->
      dead_header_size:int ->
      size:Int63.t ->
      data:string ->
      (int63, [> Io.create_error | Io.write_error | Io.close_error ]) result
    (** [create_from_data ~mapping ~dead_header_size ~size ~data] initializes a
        new sparse file on disk from the existing file [data], by creating the
        corresponding [mapping] file. The first [dead_header_size] bytes are
        ignored and the remaining [size] bytes of [data] are made available.

        On success, returns the size of the [mapping] file to be stored in the
        control file for consistency checking on open. *)
  end

  module Ao : sig
    type t

    val end_off : t -> Int63.t
    (** [end_off t] returns the largest virtual offset contained in the sparse
        file [t]. Attempting to append with a strictly smaller virtual offset
        will fail. *)

    val mapping_size : t -> Int63.t
    (** [end_off t] returns the current size of the mapping file associated to
        the sparse file [t] including additions not yet flushed to the file
        system. It can be passed to {!open_ao} as [mapping_size] when opening
        the file again. *)

    val create :
      mapping:string -> data:string -> (t, [> Io.create_error ]) result
    (** [create ~mapping ~data] initializes a new empty sparse file, represented
        on disk by two files named [mapping] and [data]. *)

    val open_ao :
      mapping_size:Int63.t ->
      mapping:string ->
      data:string ->
      ( t,
        [> Io.open_error
        | `Closed
        | `Invalid_argument
        | `Read_out_of_bounds
        | `Inconsistent_store ] )
      result
    (** [open_ao ~mapping_size ~mapping ~data] returns an append-only instance
        of the sparse file. *)

    val append_seq_exn : t -> off:int63 -> string Seq.t -> unit
    (** [append_seq_exn t ~off seq] appends the sequence of strings [seq] to the
        sparse file [t], at the virtual offset [off] which must be larger than
        the previously appended offsets. *)

    val flush : t -> (unit, [> Io.write_error ]) result
    (** Flush the append buffer. Does not call [fsync]. *)

    val close : t -> (unit, [> Io.close_error | `Pending_flush ]) result
    (** Close the underlying files. *)
  end
end

module type Sigs = sig
  module type S = S

  module Make (Io : Io.S) : S with module Io = Io
end
