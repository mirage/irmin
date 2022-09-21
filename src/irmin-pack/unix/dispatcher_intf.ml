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
  module Mapping_file : Mapping_file.S with module Io = Fm.Io

  type t

  type accessor
  (** An [accessor] designates a valid readable area in one of the pack files.

      Accessors are meant to be used from within the [Irmin_pack_unix]
      implementation. Their validity is only checked at creation time, so they
      are not meant to be kept for a long time. (e.g. if kept over a GC
      finalisation, an accessor could no longer point to a valid area because
      the GC changes the domain of valid readable areas) *)

  val v : Fm.t -> (t, [> Fm.Errs.t ]) result

  val create_accessor_exn : t -> off:int63 -> len:int -> accessor
  (** [create_accessor_exn] returns an accessor if [off] and [len] designate a
      readable area of the pack files, otherwise it raises one of
      [Errors.Pack_error `Read_out_of_bounds],
      [Errors.Pack_error (`Invalid_prefix_read _)] and
      [Errors.Pack_error (`Invalid_read_of_gced_object _)]. *)

  val create_accessor_from_range_exn :
    t -> off:int63 -> min_len:int -> max_len:int -> accessor
  (** [create_accessor_from_maxlen_exn] is similar to [create_accessor_exn]
      except that the precise length of the span will be decided during the
      call. *)

  val shrink_accessor_exn : accessor -> new_len:int -> accessor
  (** [shrink_accessor_exn a ~new_len] is [a] where the length is smaller than
      in [a].*)

  module Sequential : sig
    val create_accessor_exn : int63 -> int63 -> off:int63 -> len:int -> accessor
    (** [create_accessor_exn prefix_len suffix_len ~off ~len] returns an
        accessor if [off] and [len] designate a readable area of the pack
        files, otherwise it raises one of [Errors.Pack_error `Read_out_of_bounds].
        [prefix_len] and [suffix_len] are directly given to the function as they
        are computed by costly functions to call.
        In the contrary of the above create_accessor_exn function, it does not
        take "virtual" lengths (aka. lengths taking gc'ed chunks into account),
        but physical lengths in argument. *)

    val create_accessor_from_range_exn :
      int63 -> int63 -> off:int63 -> min_len:int -> max_len:int -> accessor
    (** [create_accessor_from_range_exn] is similar to
        [create_accessor_exn] except that the precise length of the span will be
        decided during the call. *)

    val create_accessor_seq :
      t ->
      min_header_len:int ->
      max_header_len:int ->
      read_len:(bytes -> int) ->
      (int63 * accessor) Seq.t
    (** [create_accessor_seq ~min_header_len ~max_header_len ~read_len]
        returns a sequence of accessors, which simulates iterating sequentially
        trough the entries of a pack file. [min_header_len] & [max_header_len]
        represents the minimum & maximum lengths required to read the header of
        an entry. [read_len] will then be called with a buffer containing the
        header of the entry and should return the total length of the entry (the
        length of he header plus the length of the payload)*)
  end

  val read_exn : t -> accessor -> bytes -> unit
  (** [read_exn] either reads in the prefix or the suffix file, depending on
      [accessor]. *)

  val end_offset : t -> int63
  (** [end_offset] is the end offsets of the pack entries, counting that the
      prefix doesn't start at 0. It counts the entries not yet flushed from the
      prefix. *)

  val offset_of_suffix_off : t -> int63 -> int63
  (** [offset_of_suffix_off t suffix_off] converts a suffix offset into a
      (global) offset. *)

  val read_in_prefix_and_suffix_exn : t -> off:int63 -> len:int -> bytes -> unit
  (** Simlar to [read_exn] but if [off + len] is greater than the end of the
      prefix, it will read the remaining in the prefix. *)
end

module type Sigs = sig
  module type S = S

  module Make (Fm : File_manager.S with module Io = Io.Unix) :
    S with module Fm = Fm
end
