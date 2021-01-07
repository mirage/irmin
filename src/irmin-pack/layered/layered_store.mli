(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESIrmin. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

val pp_current_upper : bool Fmt.t

module Content_addressable
    (H : Irmin.Hash.S)
    (Index : Irmin_pack.Private.Pack_index.S)
    (U : Irmin_pack.Pack.S with type index = Index.t and type key = H.t)
    (L : Irmin_pack.Pack.S
           with type index = U.index
            and type key = U.key
            and type value = U.value) :
  S.LAYERED_PACK
    with type index = U.index
     and type U.index = Index.t
     and type L.index = Index.t
     and type key = U.key
     and type U.key = H.t
     and type L.key = H.t
     and type value = U.value
     and type L.value = U.value

module Atomic_write
    (K : Irmin.Branch.S)
    (U : S.ATOMIC_WRITE_STORE with type key = K.t)
    (L : S.ATOMIC_WRITE_STORE with type key = U.key and type value = U.value) :
  S.LAYERED_ATOMIC_WRITE_STORE with type key = U.key and type value = U.value

module Pack_Maker
    (H : Irmin.Hash.S)
    (Index : Irmin_pack.Private.Pack_index.S)
    (P : Irmin_pack.Pack.MAKER with type key = H.t and type index = Index.t) :
  S.LAYERED_PACK_MAKER with type key = P.key and type index = P.index
