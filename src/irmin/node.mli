(*
 * Copyright (c) 2013      Louis Gesbert     <louis.gesbert@ocamlpro.com>
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Nodes represent structured values serialized in the block
    store. *)

module No_metadata: S.METADATA with type t = unit

module Make (K: Type.S) (P: S.PATH) (M: S.METADATA):
  S.NODE with type hash = K.t
          and type step = P.step
          and type metadata = M.t

module Store
    (C: S.CONTENTS_STORE)
    (P: S.PATH)
    (M: S.METADATA)
    (N: sig
       include S.CONTENT_ADDRESSABLE_STORE with type key = C.key
       module Key: S.HASH with type t = key
       module Val: S.NODE with type t = value
                           and type hash = key
                           and type metadata = M.t
                           and type step = P.step
     end):
  S.NODE_STORE with type 'a t = 'a C.t * 'a N.t
                and type key = N.key
                and type value = N.value
                and module Path = P
                and module Metadata = M
                and type Key.t = N.key
                and module Val = N.Val

module Graph (N: S.NODE_STORE):
  S.NODE_GRAPH with type 'a t = 'a N.t
                and type contents = N.Contents.key
                and type metadata = N.Val.metadata
                and type node = N.key
                and type step = N.Path.step
                and type path = N.Path.t
