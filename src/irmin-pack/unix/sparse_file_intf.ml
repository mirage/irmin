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

  module Mapping_file : sig
    type t
    (** A mapping file is a collection of chunks which contain 3 integers. A
        length, the global offset ([off]) of the chunk and the offset of the
        chunk in the prefix file ([poff]).

        The chunks have consecutive [poff] with respect to their lengths.

        There is no need to close a [t] because its underlying file-descriptor
        is always closed. *)

    val create :
      ?report_mapping_size:(int63 -> unit) ->
      path:string ->
      register_entries:(register_entry:(off:int63 -> len:int -> unit) -> unit) ->
      unit ->
      (t, Errs.t) result
    (** [create ~path ~register_entries] creates a mapping file named [path].

        [register_entries] is a user callback that is responsible for calling
        [register_entry] for each live entry. It must be called with strictly
        decreasing offsets (or fails otherwise).

        Returns an error if the platform is not 64bits. *)

    val iter : t -> (off:int63 -> len:int -> unit) -> (unit, Errs.t) result
    (** [iter mapping f] calls [f] on each [(off,len)] pair in [mapping].

        It is guaranteed for the offsets to be iterated in monotonic order.

        It is guaranteed that entries don't overlap.

        The exceptions raised by [f] are caught and returned (as long as they
        are known by [Errs]). *)

    val iter_exn : t -> (off:int63 -> len:int -> unit) -> unit
    (** Similar to [iter mapping f] but raises exceptions. *)
  end

  type t
  type open_error := [ Io.open_error | `Corrupted_mapping_file of string ]

  val open_ro : mapping:string -> data:string -> (t, [> open_error ]) result
  val v : mapping:Mapping_file.t -> data:Io.t -> t
  val get_mapping : t -> Mapping_file.t
  val get_data : t -> Io.t
  val close : t -> (unit, [> Io.close_error ]) result
  val fsync : t -> (unit, [> Io.write_error ]) result
  val read_exn : t -> off:int63 -> len:int -> bytes -> unit

  val read_range_exn :
    t -> off:int63 -> min_len:int -> max_len:int -> bytes -> unit

  val write_exn : t -> off:int63 -> len:int -> string -> unit
  val next_valid_offset : t -> off:int63 -> int63 option
end

module type Sigs = sig
  module type S = S

  module Make (Io : Io.S) : S with module Io = Io
end
