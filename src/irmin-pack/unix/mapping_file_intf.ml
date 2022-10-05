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
  type t
  (** A mapping file is a collection of chunks which contain 3 integers. A
      length, the global offset ([off]) of the chunk and the offset of the chunk
      in the prefix file ([poff]).

      The chunks have consecutive [poff] with respect to their lengths.

      There is no need to close a [t] because its underlying file-descriptor is
      always closed. *)

  module Io : Io.S
  module Errs : Io_errors.S with module Io = Io

  type open_error :=
    [ `Corrupted_mapping_file of string | `No_such_file_or_directory ]

  type file_sizes := int63 * int63 * int63

  val create :
    ?report_file_sizes:(file_sizes -> unit) ->
    root:string ->
    generation:int ->
    register_entries:(register_entry:(off:int63 -> len:int -> unit) -> unit) ->
    unit ->
    (t, Errs.t) result
  (** [create] creates inside the directory [root] a mapping file. It never
      raises exceptions.

      [register_entries] is a user callback that is responsible for calling
      [register_entry] for each live entry. Duplicates allowed, no specfic order
      expected.

      Returns an error if the platform is not 64bits.

      Creates temporary files in [root] that are unlinked before the function
      returns. *)

  val open_map : root:string -> generation:int -> (t, [> open_error ]) result
  (** [open_map ~root ~generation] opens a mapping file. *)

  val iter : t -> (off:int63 -> len:int -> unit) -> (unit, Errs.t) result
  (** [iter mapping f] calls [f] on each [(off,len)] pair in [mapping].

      It is guaranteed for the offsets to be iterated in monotonic order.

      It is guaranteed that entries don't overlap.

      The exceptions raised by [f] are caught and returned (as long as they are
      known by [Errs]). *)

  val iter_exn : t -> (off:int63 -> len:int -> unit) -> unit
  (** Similar to [iter mapping f] but raises exceptions. *)

  type entry = { off : int63; poff : int63; len : int }

  val find_nearest_leq : t -> int63 -> entry option
  (** [find_nearest_leq t off] returns the entry in [t] whose offset is the
      nearest [<=] the given [off] *)
end

module type Sigs = sig
  module type S = S

  module Make (Io : module type of Io.Unix) : S with module Io = Io
end
