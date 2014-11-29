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

(** Irmin stores *)

module type STORE = sig
  type step
  include Ir_bc.STORE with type key = step list
  module Key: Ir_path.S with type step = step
  module Val: Ir_contents.S with type t = value
  module View: Ir_view.S
      with type db = t
       and type step := step
       and type value = value
  module Snapshot: Ir_snapshot.S
    with type db = t
     and type key = key
     and type value = value
  module Dot: Ir_dot.S
    with type db = t
  module Sync: Ir_sync.STORE
    with type db = t
     and type head := head
end

module type MAKER =
  functor (P: Ir_path.S) ->
  functor (C: Ir_contents.S) ->
  functor (T: Ir_tag.S) ->
  functor (H: Ir_hash.S) ->
    STORE with type step = P.step
           and type value = C.t
           and type tag = T.t
           and type head = H.t

module Make (AO: Ir_ao.MAKER) (RW: Ir_rw.MAKER): MAKER

module Make_ext
    (C: Ir_contents.STORE)
    (N: Ir_node.STORE with type Val.contents = C.key)
    (S: Ir_commit.STORE with type Val.node = N.key)
    (T: Ir_tag.STORE with type value = S.key)
    (R: Ir_sync.S with type head = S.key and type tag = T.key):
  STORE with type step = N.Path.step
         and type value = C.value
         and type tag = T.key
         and type head = S.key
