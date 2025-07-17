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

open! Import

(** Stat collection during GC *)

module Main (Io : Io_intf.S) : sig
  type t

  val create :
    string ->
    generation:int ->
    commit_offset:int63 ->
    before_suffix_start_offset:int63 ->
    before_suffix_end_offset:int63 ->
    after_suffix_start_offset:int63 ->
    t

  val finish_current_step : t -> string -> t

  val finalise :
    t ->
    Stats.Latest_gc.worker ->
    after_suffix_end_offset:int63 ->
    Stats.Latest_gc.stats
end

module Worker (Io : Io_intf.S) : sig
  type t

  val create : string -> t
  val set_objects_traversed : t -> int -> t
  val add_suffix_transfer : t -> int63 -> t
  val add_file_size : t -> string -> int63 -> t
  val finish_current_step : t -> string -> t
  val finalise : t -> Stats.Latest_gc.worker
end
