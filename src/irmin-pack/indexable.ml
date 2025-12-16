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

include Indexable_intf
open! Import

module Closeable (CA : S) = struct
  include Irmin.Indexable.Check_closed_store (CA)

  (** override of {!Irmin.Indexable.S.add} to allow read-only *)
  let add t v = (get_if_open_exn t |> CA.add) v

  (** override of {!Irmin.Indexable.S.unsafe_add} to allow read-only *)
  let unsafe_add t k v = (get_if_open_exn t |> CA.unsafe_add) k v

  let index_direct t h = (get_if_open_exn t |> CA.index_direct) h

  let unsafe_append ~ensure_unique ~overcommit t k v =
    (get_if_open_exn t |> CA.unsafe_append ~ensure_unique ~overcommit) k v

  let unsafe_mem t k = (get_if_open_exn t |> CA.unsafe_mem) k

  let unsafe_find ~check_integrity t k =
    (get_if_open_exn t |> CA.unsafe_find ~check_integrity) k
end
