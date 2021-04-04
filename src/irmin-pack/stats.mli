(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

type t = {
  mutable finds : int;
  mutable cache_misses : int;
  mutable appended_hashes : int;
  mutable appended_offsets : int;
}
(** The type for stats for a store S.

    - [finds] is the number of calls to [S.find];
    - [cache_misses] is the number of times a cache miss occured during calls to
      [S.find];
    - [appended_hashes] is the number of times a hash was appended, during calls
      to [add];
    - [appended_offsets] is the number of times an offset was appended, during
      calls to [add];

    [appended_hashes] + [appended_offsets] = the number of calls to [add] *)

val reset_stats : unit -> unit
val get : unit -> t
val incr_finds : unit -> unit
val incr_cache_misses : unit -> unit
val incr_appended_hashes : unit -> unit
val incr_appended_offsets : unit -> unit

type cache_stats = { cache_misses : float }
type offset_stats = { offset_ratio : float; offset_significance : int }

val get_cache_stats : unit -> cache_stats
val get_offset_stats : unit -> offset_stats
