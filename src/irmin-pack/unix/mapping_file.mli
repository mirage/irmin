(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

module Make (Errs : Io_errors.S with module Io = Io.Unix) : sig
  val create :
    root:string ->
    generation:int ->
    register_entries:(register_entry:(off:int63 -> len:int -> unit) -> unit) ->
    (unit, [> Errs.t ]) result
  (** [create] creates inside the directory [root] a mapping file. It never
      raises exceptions.

      [register_entries] is a user callback that is responsible for calling
      [register_entry] for each live entry. Duplicates allowed, no specfic order
      expected.

      Returns an error if the platform is not 64bits.

      Works on both little-endian and big-endian platforms.

      Creates temporary files in [root] that are unlinked before the function
      returns. *)

  val load_mapping_as_mmap : string -> int_bigarray
  (** [load_mapping_as_mmap path] returns an mmap-backed [int_bigarray];
      assuming the path is for a mapping file previously created via [create],
      the array should hold pairs of [(off,len)] data *)

  val iter_mmap : int_bigarray -> (off:int63 -> len:int -> unit) -> unit
  (** [iter_mmap arr f] calls [f] on each [(off,len)] pair in [arr], starting
      from the beginning of [arr]. This is a common pattern in the rest of the
      code, so exposed as a helper function here. *)
end
