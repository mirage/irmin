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

module No_metadata: Ir_s.METADATA with type t = unit

module Make (C: Ir_s.S0) (N: Ir_s.S0) (P: Ir_s.PATH) (M: Ir_s.METADATA):
  Ir_s.NODE with type contents = C.t
             and type node = N.t
             and type step = P.step
             and type metadata = M.t

module Store
    (C: Ir_s.CONTENTS_STORE)
    (P: Ir_s.PATH)
    (M: Ir_s.METADATA)
    (S: sig
       include Ir_s.AO
       module Key: Ir_s.HASH with type t = key
       module Val: Ir_s.NODE with type t = value
                              and type node = key
                              and type metadata = M.t
                              and type contents = C.key
                              and type step = P.step
     end):
  Ir_s.NODE_STORE with type t = C.t * S.t
                   and type key = S.key
                   and type value = S.value
                   and module Path = P
                   and module Metadata = M
                   and module Key = S.Key
                   and module Val = S.Val

module Graph (S: Ir_s.NODE_STORE):
  Ir_s.NODE_GRAPH with type t = S.t
                   and type contents = S.Contents.key
                   and type metadata = S.Val.metadata
                   and type node = S.key
                   and type step = S.Path.step
                   and type path = S.Path.t
