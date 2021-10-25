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

let fresh_stats () =
  {
    finds = 0;
    cache_misses = 0;
    appended_hashes = 0;
    appended_offsets = 0;
    inode_add = 0;
    inode_remove = 0;
    inode_of_seq = 0;
    inode_of_raw = 0;
    inode_rec_add = 0;
    inode_rec_remove = 0;
    inode_to_binv = 0;
    inode_decode_bin = 0;
    inode_encode_bin = 0;
  }

let s = fresh_stats ()

let reset_stats () =
  s.finds <- 0;
  s.cache_misses <- 0;
  s.appended_hashes <- 0;
  s.appended_offsets <- 0;
  s.inode_add <- 0;
  s.inode_remove <- 0;
  s.inode_of_seq <- 0;
  s.inode_of_raw <- 0;
  s.inode_rec_add <- 0;
  s.inode_rec_remove <- 0;
  s.inode_to_binv <- 0;
  s.inode_decode_bin <- 0;
  s.inode_encode_bin <- 0;
  ()

let get () = s
let incr_finds () = s.finds <- succ s.finds
let incr_cache_misses () = s.cache_misses <- succ s.cache_misses
let incr_appended_hashes () = s.appended_hashes <- succ s.appended_hashes
let incr_appended_offsets () = s.appended_offsets <- succ s.appended_offsets
let incr_inode_add () = s.inode_add <- s.inode_add + 1
let incr_inode_remove () = s.inode_remove <- s.inode_remove + 1
let incr_inode_of_seq () = s.inode_of_seq <- s.inode_of_seq + 1
let incr_inode_of_raw () = s.inode_of_raw <- s.inode_of_raw + 1
let incr_inode_rec_add () = s.inode_rec_add <- s.inode_rec_add + 1
let incr_inode_rec_remove () = s.inode_rec_remove <- s.inode_rec_remove + 1
let incr_inode_to_binv () = s.inode_to_binv <- s.inode_to_binv + 1
let incr_inode_decode_bin () = s.inode_decode_bin <- s.inode_decode_bin + 1
let incr_inode_encode_bin () = s.inode_encode_bin <- s.inode_encode_bin + 1

type cache_stats = { cache_misses : float }
type offset_stats = { offset_ratio : float; offset_significance : int }

let div_or_zero a b = if b = 0 then 0. else float_of_int a /. float_of_int b
let get_cache_stats () = { cache_misses = div_or_zero s.cache_misses s.finds }

let get_offset_stats () =
  {
    offset_ratio =
      div_or_zero s.appended_offsets (s.appended_offsets + s.appended_hashes);
    offset_significance = s.appended_offsets + s.appended_hashes;
  }
