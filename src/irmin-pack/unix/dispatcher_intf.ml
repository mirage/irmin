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

  val v : root:string -> Fm.t -> (t, [> Fm.Errs.t ]) result

  val read_exn : t -> off:int63 -> len:int -> bytes -> unit
  (** [read_exn] either reads in the prefix or the suffix file, depending on
      [off]. See [Io.read_exn] for the arguments. If it tries to read a gced
      object, an exception is raised. *)

  val read_at_most_exn : t -> off:int63 -> len:int -> bytes -> int
  (** [read_at_most_exn] is similar to [read_exn] but if the end of file is
      reached while reading [len] bytes, then only the available bytes are read.
      No [`Read_out_of_bounds] error is raised. The number of bytes read are
      returned. *)

  val end_offset : t -> int63
  (** [end_offset] is the end offsets of the pack entries, counting that the
      prefix doesn't start at 0. It counts the entries not yet flushed from the
      prefix. *)

  val read_if_not_gced : t -> off:int63 -> len:int -> bytes -> bool
  (** Similar to [read_exn] but returns false if the object was gced, instead of
      raising an expection. *)

  val offset_of_suffix_off : t -> int63 -> int63
  (** [offset_of_suffix_off t suffix_off] converts a suffix offset into a
      (global) offset. *)

  val read_in_prefix_and_suffix_exn : t -> off:int63 -> len:int -> bytes -> unit
  (** Simlar to [read_exn] but if [off + len] is greater than the end of the
      prefix, it will read the remaining in the prefix. *)

  type mapping
  (** [mapping] implements a map from global offset to [(offset,len)] in the
      prefix file *)

  val load_mapping : string -> (mapping, [> Fm.Errs.t ]) result
  (** [load_mapping path] loads the [mapping] from the given path and returns it *)

  val poff_of_entry_exn : mapping -> off:int63 -> len:int -> int63
end

module type Sigs = sig
  module type S = S

  module Make (Fm : File_manager.S with module Io = Io.Unix) :
    S with module Fm = Fm
end
