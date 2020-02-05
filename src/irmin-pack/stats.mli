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
