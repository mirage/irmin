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

module Find = struct
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

  let create () =
    {
      total = 0;
      from_staging = 0;
      from_lru = 0;
      from_pack_direct = 0;
      from_pack_indexed = 0;
    }

  let clear t =
    t.total <- 0;
    t.from_staging <- 0;
    t.from_lru <- 0;
    t.from_pack_direct <- 0;
    t.from_pack_indexed <- 0

  let cache_misses
      {
        (* Total finds (hits + misses) *)
        total;
        (* In-memory hits: *)
        from_staging;
        from_lru;
        from_pack_direct = _;
        from_pack_indexed = _;
      } =
    total - (from_staging + from_lru)
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

let fresh_stats () =
  {
    finds = Find.create ();
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

let () =
  at_exit (fun () -> Fmt.epr "\n\nStatistics: %a\n@." (Irmin.Type.pp_dump t) s)

let reset_stats () =
  Find.clear s.finds;
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

let report_find ~(location : Find.location) =
  let finds = s.finds in
  finds.total <- succ finds.total;
  match location with
  | Staging -> finds.from_staging <- succ finds.from_staging
  | Lru -> finds.from_lru <- succ finds.from_lru
  | Pack_direct -> finds.from_pack_direct <- succ finds.from_pack_direct
  | Pack_indexed -> finds.from_pack_indexed <- succ finds.from_pack_indexed
  | Not_found -> ()

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

let get_cache_stats () =
  let cache_misses = Find.cache_misses s.finds in
  { cache_misses = div_or_zero cache_misses s.finds.total }

let get_offset_stats () =
  {
    offset_ratio =
      div_or_zero s.appended_offsets (s.appended_offsets + s.appended_hashes);
    offset_significance = s.appended_offsets + s.appended_hashes;
  }
