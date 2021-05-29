(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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
    (Hash : Irmin.Hash.S)
    (Val : Irmin.Type.S)
    (Index : Irmin_pack.Index.S)
    (U : Irmin_pack.Content_addressable.Createable
           with type index = Index.t
            and type key = Hash.t
            and type value = Val.t)
    (L : Irmin_pack.Content_addressable.Createable
           with type index = Index.t
            and type key = Hash.t
            and type value = Val.t) :
  S.Content_addressable
    with type index = Index.t
     and type key = Hash.t
     and type value = Val.t

module Atomic_write
    (K : Irmin.Branch.S)
    (V : Irmin.Type.S)
    (U : Irmin_pack.Atomic_write.S with type key = K.t and type value = V.t)
    (L : Irmin_pack.Atomic_write.S with type key = K.t and type value = V.t) :
  S.Atomic_write with type key = K.t and type value = V.t

module Pack_maker
    (H : Irmin.Hash.S)
    (Index : Irmin_pack.Index.S)
    (P : Irmin_pack.Content_addressable.Maker
           with type key = H.t
            and type index = Index.t) :
  S.Content_addressable_maker with type key = P.key and type index = P.index
