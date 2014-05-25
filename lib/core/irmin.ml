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

open Core_kernel.Std
open Lwt
open IrminMerge.OP
open IrminSig

module Log = Log.Make(struct let section = "IRMIN" end)

module type S = sig
  include IrminBranch.STORE with type key = IrminPath.t
  module Snapshot: IrminSnapshot.STORE with type t = t
  module Dump: IrminDump.STORE with type t        = t
                                and type key      = Block.key
                                and type contents = Block.contents
  module View: IrminView.STORE with type node  = Block.key
                                and type value = value
end

type ('key, 'contents, 'tag) t =
  (module S with type Block.key = 'key
             and type Block.contents = 'contents
             and type Tag.key = 'tag)

module Make (Block: IrminBlock.STORE) (Tag: IrminTag.STORE with type value = Block.key) =
struct

  module S = IrminBranch.Make(Block)(Tag)
  module Snapshot = IrminSnapshot.Make(S)
  module Dump = IrminDump.Make(S)
  module View = IrminView.Store(S)
  include S

end

module Binary
    (K : IrminKey.S)
    (C : IrminContents.S)
    (T : IrminTag.S)
    (AO: AO_BINARY)
    (RW: RW_BINARY) =
struct

  module V = IrminBlock.S(K)(C)

  module AO = IrminStore.AO_MAKER(AO)
  module RW = IrminStore.RW_MAKER(RW)

  module XBlock = IrminBlock.Make(K)(C)(AO(K)(V))
  module XTag   = IrminTag.Make(T)(K)(RW(T)(K))

  include Make(XBlock)(XTag)

end


module type BACKEND = sig
  type config
  module RO: RO_MAKER
  module AO: AO_MAKER
  module RW: RW_MAKER
  module BC: IrminBranch.MAKER
  module Make (K: IrminKey.S) (C: IrminContents.S) (T: IrminTag.S): sig
    type nonrec t = (K.t, C.t, T.t) t
    val create: config -> t
    val cast: t -> (module S)
  end
end
