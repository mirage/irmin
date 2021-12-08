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

module Find : sig
  type location = Staging | Lru | Pack_direct | Pack_indexed | Not_found
  [@@deriving irmin]

  type t = {
    mutable total : int;
    mutable from_staging : int;
    mutable from_lru : int;
    mutable from_pack_direct : int;
    mutable from_pack_indexed : int;
  }
  [@@deriving irmin]

  val cache_misses : t -> int
end

type t = {
  finds : Find.t;
  mutable appended_hashes : int;
  mutable appended_offsets : int;
  mutable inode_add : int;
  mutable inode_remove : int;
  mutable inode_of_seq : int;
  mutable inode_of_raw : int;
  mutable inode_rec_add : int;
  mutable inode_rec_remove : int;
  mutable inode_to_binv : int;
  mutable inode_decode_bin : int;
  mutable inode_encode_bin : int;
}
[@@deriving irmin]
(** The type for stats for a store S.

    - [finds] stores the total number of calls to [S.find], and tracks the
      source locations of successful finds (i.e. whether the value was recovered
      from the staging table, LRU, or the pack file);
    - [appended_hashes] is the number of times a hash was appended, during calls
      to [add];
    - [appended_offsets] is the number of times an offset was appended, during
      calls to [add];
    - [inode_add + inode_remove + inode_of_seq + inode_of_raw] is the total
      number of [Inode.Val.t] built;
    - [inode_rec_add + inode_rec_remove] are witnesses of the quantity of work
      that is done modifying inodes;
    - [inode_to_binv] is the number of [Inode.Bin.v] built;
    - [inode_encode_bin] is the number of [Bin] to [Compress] conversions;
    - [inode_decode_bin] is the number of [Compress] to [Bin] conversions; *)

val reset_stats : unit -> unit
val get : unit -> t
val report_find : location:Find.location -> unit
val incr_appended_hashes : unit -> unit
val incr_appended_offsets : unit -> unit
val incr_inode_add : unit -> unit
val incr_inode_remove : unit -> unit
val incr_inode_of_seq : unit -> unit
val incr_inode_of_raw : unit -> unit
val incr_inode_rec_add : unit -> unit
val incr_inode_rec_remove : unit -> unit
val incr_inode_to_binv : unit -> unit
val incr_inode_decode_bin : unit -> unit
val incr_inode_encode_bin : unit -> unit

type cache_stats = { cache_misses : float }
type offset_stats = { offset_ratio : float; offset_significance : int }

val get_cache_stats : unit -> cache_stats
val get_offset_stats : unit -> offset_stats
