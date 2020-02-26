type t = {
  mutable finds : int;
  mutable cache_misses : int;
  mutable appended_hashes : int;
  mutable appended_offsets : int;
}

let fresh_stats () =
  { finds = 0; cache_misses = 0; appended_hashes = 0; appended_offsets = 0 }

let stats = fresh_stats ()

let reset_stats () =
  stats.finds <- 0;
  stats.cache_misses <- 0;
  stats.appended_hashes <- 0;
  stats.appended_offsets <- 0;
  ()

let get () = stats

let incr_finds () = stats.finds <- succ stats.finds

let incr_cache_misses () = stats.cache_misses <- succ stats.cache_misses

let incr_appended_hashes () =
  stats.appended_hashes <- succ stats.appended_hashes

let incr_appended_offsets () =
  stats.appended_offsets <- succ stats.appended_offsets

type cache_stats = { cache_misses : float }

type offset_stats = { offset_ratio : float; offset_significance : int }

let div_or_zero a b = if b = 0 then 0. else float_of_int a /. float_of_int b

let get_cache_stats () =
  { cache_misses = div_or_zero stats.cache_misses stats.finds }

let get_offset_stats () =
  {
    offset_ratio =
      div_or_zero stats.appended_offsets
        (stats.appended_offsets + stats.appended_hashes);
    offset_significance = stats.appended_offsets + stats.appended_hashes;
  }
