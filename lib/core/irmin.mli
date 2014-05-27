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

(** API entry point *)

module type S = sig

  (** Main signature for Irminsule stores. *)

  include IrminBranch.STORE with type key = IrminPath.t

  module Snapshot: IrminSnapshot.STORE with type db = t

  module Dump: IrminDump.STORE with type db       = t
                                and type key      = Block.key
                                and type contents = Block.contents

  module View: IrminView.STORE with type db    = t
                                and type node  = Block.key
                                and type value = value

end

type ('key, 'contents, 'tag) t =
  (module S with type Block.key = 'key
             and type value     = 'contents
             and type branch    = 'tag)

val cast: ('a, 'b, 'c) t -> (module S)

module Make
    (Block: IrminBlock.STORE)
    (Tag  : IrminTag.STORE with type value = Block.key)
  : S with type Block.key = Block.key
       and type value     = Block.contents
       and type branch    = Tag.key
(** Build a full iminsule store. *)

module type BACKEND = sig

  (** Common signature for all backends. *)

  module RO (K: IrminKey.S) (V: IrminIdent.S): IrminStore.RO
  module AO (K: IrminKey.S) (V: IrminIdent.S): IrminStore.AO
  module RW (K: IrminKey.S) (V: IrminKey.S)  : IrminStore.RW

  module Make (K: IrminKey.S) (C: IrminContents.S) (T: IrminTag.S):
    S with type Block.key = K.t
       and type value     = C.t
       and type branch    = T.t

end

(** {2 Binary stores} *)

module RO_BINARY (S: IrminStore.RO_BINARY) (K: IrminKey.S) (V: IrminIdent.S): IrminStore.RO
module AO_BINARY (S: IrminStore.AO_BINARY) (K: IrminKey.S) (V: IrminIdent.S): IrminStore.AO
module RW_BINARY (S: IrminStore.RW_BINARY) (K: IrminKey.S) (V: IrminIdent.S): IrminStore.RW

module Binary
    (AO: IrminStore.AO_BINARY)
    (RW: IrminStore.RW_BINARY)
    (K: IrminKey.S) (C: IrminContents.S) (T: IrminTag.S)
  : S with type Block.key = K.t
       and type value     = C.t
       and type branch    = T.t
(** Create an irminsule store from binary stores. Use one common
    append-only store for contents, nodes and commits and a mutable
    store for the tags. *)
