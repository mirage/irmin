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

module Content_addressable
    (H : Irmin.Hash.S)
    (Index : Pack_index.S)
    (Pack : Pack.S with type index = Index.t and type key = H.t) :
  S.LAYERED_CONTENT_ADDRESSABLE_STORE
    with type key = Pack.key
     and type value = Pack.value
     and type index = Pack.index
     and module U = Pack
     and module L = Pack

module Atomic_write
    (K : Irmin.Branch.S)
    (A : S.ATOMIC_WRITE_STORE with type key = K.t) :
  S.LAYERED_ATOMIC_WRITE_STORE with type key = A.key and type value = A.value

module Pack_Maker
    (H : Irmin.Hash.S)
    (Index : Pack_index.S)
    (Pack : Pack.MAKER with type key = H.t and type index = Index.t) :
  S.LAYERED_MAKER with type key = Pack.key and type index = Pack.index
