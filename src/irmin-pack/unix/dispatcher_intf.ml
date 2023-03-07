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

open Import

module type S = sig
  module Fm : File_manager.S

  type t

  val v : Fm.t -> (t, [> Fm.Errs.t ]) result

  val read_exn :
    t ->
    off:int63 ->
    len:int ->
    ?volume_identifier:Lower.volume_identifier ->
    bytes ->
    Lower.volume_identifier option
  (** [read_exn t ~off ~len buffer] writes into [buffer] the bytes from [off] to
      [off+len]. If the read occurred, in a lower volume, its identifier is
      returned.

      If you know which volume to read from in the lower, provide
      [volume_identifier] to skip checking the prefix.

      Note: [read_exn] is the only read function that supports reading in the
      lower. *)

  val read_range_exn :
    t ->
    off:int63 ->
    min_len:int ->
    max_len:int ->
    ?volume_identifier:Lower.volume_identifier ->
    bytes ->
    int * Lower.volume_identifier option
  (** Same as [read_exn], the amount read is [max_len] if possible or at least
      [min_len] if reading more would step over a hole in the sparse file.
      Returns the actually read length and optionnaly the volume where the data
      was found. *)

  val end_offset : t -> int63
  (** [end_offset] is the end offsets of the pack entries, counting that the
      prefix doesn't start at 0. It counts the entries not yet flushed from the
      prefix. *)

  val suffix_start_offset : t -> int63
  (** [suffix_start_offset] is the offsets of the first pack entry in the
      suffix. All pack entries in the prefix fit below [suffix_start_offset]. *)

  val offset_of_soff : t -> int63 -> int63
  (** [offset_of_soff t suffix_off] converts a suffix offset into a (global)
      offset. *)

  val soff_of_offset : t -> int63 -> int63
  (** [soff_of_offset t global_offset] converts a global offset to a suffix
      offset. *)

  val read_seq_exn : t -> off:int63 -> len:int63 -> string Seq.t

  val read_bytes_exn : t -> f:(string -> unit) -> off:int63 -> len:int63 -> unit
  (** [read_bytes_exn] reads a slice of the global offset space defined by [off]
      and [len].

      The calls to [f] ignore the objects boundaries (i.e. the string passed to
      [f] will most of the time not be the beginning of an object).

      The strings passed to [f] are safe. They can be kept around, they are not
      the result of an [unsafe_to_string] conversion.

      The call will fail if the [(off, len)] range is invalid. It will succeed
      in these cases:

      - If the range designates a slice of the suffix.
      - If the range designates a slice of contiguous live bytes in the prefix
      - If the range designates a slice of contiguous live bytes that starts in
        the prefix and ends in the suffix. This implies that the last chunk of
        the prefix is contiguous to the start of the suffix. *)

  val next_valid_offset : t -> off:int63 -> int63 option
  (** [next_valid_offset t ~off] returns an offset greater or equal to [off]
      that can be read. Used to iterate over the entries while skipping over the
      holes in the sparse file. *)
end

module type Sigs = sig
  module type S = S

  module Make (Fm : File_manager.S with module Io = Io.Unix) :
    S with module Fm = Fm
end
