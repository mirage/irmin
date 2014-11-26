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
  module Step: Tc.S0 with type t = step
  module Key: Tc.S0 with type t = key
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
  module Sync: Ir_sync.S
    with type db = t
     and type head := head
end

module Make
    (C: Ir_contents.RAW_STORE)
    (N: Ir_node.RAW_STORE with type Val.contents = C.key)
    (S: Ir_commit.RAW_STORE with type Val.node = N.key)
    (T: Ir_tag.STORE with type value = S.key)
    (R: Ir_sync.REMOTE):
  STORE with type step = N.Step.t
         and type value = C.value
         and type tag = T.key
         and type head = S.key
