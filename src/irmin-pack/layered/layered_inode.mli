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

module Store
    (Schema : Irmin_pack.Schema.S)
    (Index : Irmin_pack.Index.S)
    (C : Irmin.Contents.Store
           with type key = Schema.hash
            and type value = Schema.contents)
    (CA : Irmin_pack.Content_addressable.Maker
            with type key = Schema.hash
             and type index = Index.t) : sig
  type key = Schema.hash
  type value = Schema.node

  module Raw :
    S.Content_addressable
      with type key = key
       and type value = Schema.Node.Raw.t
       and type index = Index.t

  include
    Irmin.Node.Store
      with type 'a t = 'a C.t * 'a Raw.t
       and type key := key
       and type value := value
       and module Path = Schema.Path
       and module Metadata = Schema.Metadata
       and module Contents = C
end
