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

  val create_rw :
    root:string ->
    start_idx:int ->
    overwrite:bool ->
    auto_flush_threshold:int ->
    auto_flush_procedure:Ao.auto_flush_procedure ->
    (t, [> create_error ]) result

  val open_rw :
    root:string ->
    end_poff:int63 ->
    start_idx:int ->
    chunk_num:int ->
    dead_header_size:int ->
    auto_flush_threshold:int ->
    auto_flush_procedure:Ao.auto_flush_procedure ->
    (t, [> open_error ]) result

  val open_ro :
    root:string ->
    end_poff:int63 ->
    dead_header_size:int ->
    start_idx:int ->
    chunk_num:int ->
    (t, [> open_error ]) result

  val close : t -> (unit, [> Io.close_error | `Pending_flush ]) result
  val empty_buffer : t -> bool
  val flush : t -> (unit, [> Io.write_error ]) result
  val fsync : t -> (unit, [> Io.write_error ]) result

  (* TODO: rename [end_poff] to something that represents what purpose it serves as
     a check for what data we know has been written in the appendable chunk. Also
     rename the corresponding control file field.

     Possible new names: [consistency_poff], [persisted_poff].
  *)
  val end_poff : t -> int63
  val read_exn : t -> off:int63 -> len:int -> bytes -> unit
  val append_exn : t -> string -> unit

  (* TODO: rename [refresh_end_poff] to cohere with rename of [end_poff]. *)
  val refresh_end_poff : t -> int63 -> (unit, [> `Rw_not_allowed ]) result
  val readonly : t -> bool
  val auto_flush_threshold : t -> int option
end

module type Sigs = sig
  module type S = S

  module Make (Io : Io.S) (Errs : Io_errors.S with module Io = Io) :
    S with module Io = Io and module Errs = Errs
end
