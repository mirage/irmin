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

module Inode : sig
  type field =
    | Inode_add
    | Inode_remove
    | Inode_of_seq
    | Inode_of_raw
    | Inode_rec_add
    | Inode_rec_remove
    | Inode_to_binv
    | Inode_decode_bin
    | Inode_encode_bin

  type t = private {
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

      - [inode_add + inode_remove + inode_of_seq + inode_of_raw] is the total
        number of [Inode.Val.t] built;
      - [inode_rec_add + inode_rec_remove] are witnesses of the quantity of work
        that is done modifying inodes;
      - [inode_to_binv] is the number of [Inode.Bin.v] built;
      - [inode_encode_bin] is the number of [Bin] to [Compress] conversions;
      - [inode_decode_bin] is the number of [Compress] to [Bin] conversions; *)

  type stat

  val export : stat -> t
end

type t = { inode : Inode.stat }

val reset_stats : unit -> unit
val get : unit -> t
val incr_inode_add : unit -> unit
val incr_inode_remove : unit -> unit
val incr_inode_of_seq : unit -> unit
val incr_inode_of_raw : unit -> unit
val incr_inode_rec_add : unit -> unit
val incr_inode_rec_remove : unit -> unit
val incr_inode_to_binv : unit -> unit
val incr_inode_decode_bin : unit -> unit
val incr_inode_encode_bin : unit -> unit
