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
  (** Abstraction for a chunked suffix. It is functionally equivalent to
      {!Append_only_file} but with a chunked implementation that is
      parameterized by

      - [start_idx] for {!create_rw} to know the starting file name, and
      - [start_idx] and [chunk_num] for the open functions to know the starting
        file name and how many files there are. *)

  module Io : Io.S
  module Errs : Io_errors.S
  module Ao : Append_only_file.S

  type t
  type create_error = Io.create_error

  type open_error =
    [ Io.open_error
    | `Closed
    | `Invalid_argument
    | `Inconsistent_store
    | `Read_out_of_bounds ]

  type add_new_error =
    [ open_error
    | Io.close_error
    | `Pending_flush
    | `File_exists of string
    | `Multiple_empty_chunks ]

  val create_rw :
    root:string ->
    start_idx:int ->
    overwrite:bool ->
    auto_flush_threshold:int ->
    auto_flush_procedure:Ao.auto_flush_procedure ->
    (t, [> create_error ]) result

  val open_rw :
    root:string ->
    appendable_chunk_poff:int63 ->
    start_idx:int ->
    chunk_num:int ->
    dead_header_size:int ->
    auto_flush_threshold:int ->
    auto_flush_procedure:Ao.auto_flush_procedure ->
    (t, [> open_error ]) result

  val open_ro :
    root:string ->
    appendable_chunk_poff:int63 ->
    dead_header_size:int ->
    start_idx:int ->
    chunk_num:int ->
    (t, [> open_error ]) result

  val add_chunk :
    auto_flush_threshold:int ->
    auto_flush_procedure:Ao.auto_flush_procedure ->
    t ->
    (unit, [> add_new_error ]) result

  val start_idx : t -> int
  val chunk_num : t -> int
  val close : t -> (unit, [> Io.close_error | `Pending_flush ]) result
  val empty_buffer : t -> bool
  val flush : t -> (unit, [> Io.write_error ]) result
  val fsync : t -> (unit, [> Io.write_error ]) result

  val appendable_chunk_poff : t -> int63
  (** [appendable_chunk_poff t] is the number of bytes of the chunk file that is
      currently appendable. It does not perform IO.

      {3 RW mode}

      It also counts the bytes not flushed yet.

      {3 RO mode}

      This information originates from the latest reload of the control file.
      Calling [refresh_appendable_chunk_poff t] updates [appendable_chunk_poff]. *)

  val refresh_appendable_chunk_poff :
    t -> int63 -> (unit, [> `Rw_not_allowed ]) result
  (** Ingest the new end offset of the appendable chunk file. Typically happens
      in RO mode when the control file has been re-read.

      {3 RW mode}

      Always returns [Error `Rw_not_allowed]. *)

  val end_soff : t -> int63
  (** [end_soff t] is the end offset for the chunked suffix. The valid range of
      offsets is 0 <= off < end_soff. Therefore, [end_soff] also represents the
      length of the chunked suffix. *)

  val read_exn : t -> off:int63 -> len:int -> bytes -> unit

  val read_range_exn :
    t -> off:int63 -> min_len:int -> max_len:int -> bytes -> int

  val append_exn : t -> string -> unit
  val readonly : t -> bool
  val auto_flush_threshold : t -> int option

  val fold_chunks :
    (acc:'a ->
    idx:int ->
    start_suffix_off:int63 ->
    end_suffix_off:int63 ->
    is_appendable:bool ->
    'a) ->
    'a ->
    t ->
    'a
end

module type Sigs = sig
  module type S = S

  module Make (Io : Io.S) (Errs : Io_errors.S with module Io = Io) :
    S with module Io = Io and module Errs = Errs
end
