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

open! Import
include Pack_index_intf

module Make (K : Irmin.Hash.S) = struct
  module Key = struct
    type t = K.t
    [@@deriving irmin ~short_hash ~equal ~to_bin_string ~decode_bin]

    let hash = short_hash ?seed:None
    let hash_size = 30
    let encode = to_bin_string
    let encoded_size = K.hash_size
    let decode s off = decode_bin s (ref off)
  end

  module Val = struct
    type t = int63 * int * Pack_value.Kind.t [@@deriving irmin]

    let encoded_size = (64 / 8) + (32 / 8) + 1

    let encode ((off, len, kind) : t) =
      let buf = Bytes.create encoded_size in
      Bytes.set_int64_be buf 0 (Int63.to_int64 off);
      Bytes.set_int32_be buf 8 (Int32.of_int len);
      Bytes.set buf 12 (Pack_value.Kind.to_magic kind);
      Bytes.unsafe_to_string buf

    let decode s pos : t =
      let buf = Bytes.unsafe_of_string s in
      let off = Bytes.get_int64_be buf pos |> Int63.of_int64 in
      let len = Bytes.get_int32_be buf (pos + 8) |> Int32.to_int in
      let kind = Bytes.get buf (pos + 12) |> Pack_value.Kind.of_magic_exn in
      (off, len, kind)
  end

  module Irmin_pack_unix_stats = Stats
  module Stats = Index.Stats
  module I = Index
  module Index = Index_unix.Make (Key) (Val) (Index.Cache.Unbounded)
  include Index

  let v_exn =
    let cache = None in
    Index.v ?cache

  let v ?flush_callback ?fresh ?readonly ?throttle ?lru_size ~log_size root =
    try
      Ok
        (v_exn ?flush_callback ?fresh ?readonly ?throttle ?lru_size ~log_size
           root)
    with
    | I.RO_not_allowed ->
        (* Happens when [fresh = true = readonly] *)
        assert false
    | Index_unix.Private.Raw.Not_written ->
        (* This is not expected to be raised but let's catch anyway to trigger
           a more precise error instead (i.e. the [assert false] below). This
           error is expected to be raised when a RO instance attemps an opening
           on a non-existing file. *)
        assert false
    | Unix.Unix_error (x, y, z) -> Error (`Io_misc (x, y, z))
    | Failure msg -> Error (`Index_failure msg)

  let add ?overcommit t k v = replace ?overcommit t k v
  let find t k = match find t k with exception Not_found -> None | h -> Some h
  let close_exn t = Index.close t

  let close t =
    try
      close_exn t;
      Ok ()
    with
    | I.RO_not_allowed -> assert false
    | Index_unix.Private.Raw.Not_written -> assert false
    | Unix.Unix_error (x, y, z) -> Error (`Io_misc (x, y, z))
    | Failure msg -> Error (`Index_failure msg)

  let reload t =
    try
      Index.sync t;
      Ok ()
    with
    | I.RO_not_allowed -> assert false
    | Index_unix.Private.Raw.Not_written -> assert false
    | Unix.Unix_error (x, y, z) -> Error (`Io_misc (x, y, z))
    | Failure msg -> Error (`Index_failure msg)

  let flush t ~with_fsync =
    try
      Index.flush ~no_callback:() ~with_fsync t;
      (* record the index flush in stats; an alternative would be to add flush stats to
         the index package *)
      Irmin_pack_unix_stats.incr_index_flushes ();
      Ok ()
    with
    | I.RO_not_allowed -> assert false
    | Index_unix.Private.Raw.Not_written -> assert false
    | Unix.Unix_error (x, y, z) -> Error (`Io_misc (x, y, z))
    | Failure msg -> Error (`Index_failure msg)
end
