(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Merge.OP
open Misc.OP

module BC (B: Block.STORE) (T: Tag.STORE with type value = B.key) =
  Branch.Make(B)(T)

module Binary
    (AO: Sig.AO_BINARY)
    (RW: Sig.RW_BINARY)
    (K : Sig.Uid)
    (C : Sig.Contents)
    (T : Sig.Tag) =
struct
  module V = Block.S(K)(C)
  module B = Block.S(K)(C)
  module XBlock = Block.Make(K)(C)(AO_BINARY(AO)(K)(B))
  module XTag = Tag.Make(T)(K)(RW_BINARY(RW)(T)(K))
  include BC(XBlock)(XTag)
end
