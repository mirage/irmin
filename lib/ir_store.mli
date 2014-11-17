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

module BC (Block: Block.STORE) (Tag: Tag.STORE with type value = Block.key)
  : Sig.BC with type value = Block.contents and type tag = Tag.key
(** Build an Irmin store. *)

(** {2 Binary stores} *)

module RO_BINARY (S: Sig.RO_BINARY): Sig.RO_MAKER
module AO_BINARY (S: Sig.AO_BINARY): Sig.AO_MAKER
module RW_BINARY (S: Sig.RW_BINARY): Sig.RW_MAKER

module Binary
    (AO: Sig.AO_BINARY) (RW: Sig.RW_BINARY)
    (K: Sig.Uid) (C: Sig.Contents) (T: Sig.Tag)
  : Sig.BC with type value = C.t and type tag = T.t
(** Create an irminsule store from binary stores. Use one common
    append-only store for contents, nodes and commits and a mutable
    store for the tags. *)
