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

module Pack_store : sig
  type field =
    | Appended_hashes
    | Appended_offsets
    | Staging  (** Found in the store's write buffer. *)
    | Lru  (** Found in the store's LRU of recent [find] results. *)
    | Pack_direct
        (** Decoded directly from the pack file (via a direct key). *)
    | Pack_indexed
        (** Binding recovered from the pack file after first checking the index
            for its offset and length (via an indexed key). *)
    | Not_found  (** Find returned [None]. *)
  [@@deriving irmin]

  type t = private {
    mutable appended_hashes : int;
    mutable appended_offsets : int;
    mutable total : int;
    mutable from_staging : int;
    mutable from_lru : int;
    mutable from_pack_direct : int;
    mutable from_pack_indexed : int;
  }
  [@@deriving irmin]

  type stat

  val cache_misses : t -> int
  val export : stat -> t
end

module Index : sig
  type t = Index.Stats.t = private {
    mutable bytes_read : int;
    mutable nb_reads : int;
    mutable bytes_written : int;
    mutable nb_writes : int;
    mutable nb_merge : int;
    mutable merge_durations : float list;
    mutable nb_replace : int;
    mutable replace_durations : float list;
    mutable nb_sync : int;
    mutable time_sync : float;
    mutable lru_hits : int;
    mutable lru_misses : int;
  }
  [@@deriving irmin]

  type stat

  val export : stat -> t
end

type t = { pack_store : Pack_store.stat; index : Index.stat }

val reset_stats : unit -> unit
val get : unit -> t
val report_pack_store : field:Pack_store.field -> unit
val report_index : unit -> unit
val incr_appended_hashes : unit -> unit
val incr_appended_offsets : unit -> unit

type cache_stats = { cache_misses : float }
type offset_stats = { offset_ratio : float; offset_significance : int }

val get_cache_stats : unit -> cache_stats
val get_offset_stats : unit -> offset_stats
