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

      Creates temporary files in [root] that are unlinked before the function
      returns. *)

  type mapping_as_int_bigarray = private
    | Int_bigarray of int_bigarray
        (** [mapping_as_int_bigarray] holds the [int_bigarray] loaded from an
            mmap. NOTE invariant-mapping-array: The [int_bigarray] holds ints;
            each consecutive group of 3 ints corresponds to a tuple
            [(off,poff,len)], where [off] is the virtual offset; [poff] is the
            offset in the prefix file; and [len] is the length of the
            corresponding chunk in the prefix file. The size of the array is
            always a multiple of 3. *)

  val empty_mapping : mapping_as_int_bigarray

  val load_mapping_as_mmap :
    string -> (mapping_as_int_bigarray, [> Errs.t ]) result
  (** [load_mapping_as_mmap path] returns an mmap-backed [int_bigarray];
      assuming the path is for a mapping file previously created via [create],
      the array should hold triples of [(off,poff,len)] data, where [off] is a
      virtual offset, [poff] is an offset in the prefix file for a live region
      of data, and [len] is the length of the region *)

  val iter_mmap :
    mapping_as_int_bigarray ->
    (off:int63 -> len:int -> unit) ->
    (unit, [> Errs.t ]) result
  (** [iter_mmap arr f] calls [f] on each [(off,len)] pair in [arr], starting
      from the beginning of [arr]. This is a common pattern in the rest of the
      code, so exposed as a helper function here.

      It is guaranteed for the offsets to be iterated in monotonic order.

      It is guaranteed that entries don't overlap.

      The exceptions raised by [f] are caught and returned (as long as they are
      known by [Errs]). *)
end
