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
include Pack_key_intf

type 'hash t = 'hash

module Make (Hash : Irmin.Hash.S) = struct
  include Irmin.Key.Of_hash (Hash)

  type nonrec t = t [@@deriving irmin ~of_bin_string]

  let null =
    match of_bin_string (String.make Hash.hash_size '\000') with
    | Ok x -> x
    | Error _ -> assert false
end

module type Store_spec = sig
  type ('h, _) contents_key = 'h t
  type 'h node_key = 'h t
  type 'h commit_key = 'h t
end

module rec Store_spec : Store_spec = Store_spec
