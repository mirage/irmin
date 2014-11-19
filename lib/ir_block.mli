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

(** Store structured values: contents, node and commits. *)

module type STORE = sig

  type origin
  type step
  type contents
  type node
  type commit
  type head

  module Origin: Ir_origin.S
    with type t = origin

  module Step: Ir_step.S
    with type t = step

  module Contents: Ir_contents.STORE
    with type value = contents
     and type origin = origin

  module Node: Ir_node.STORE
    with type value = node
     and type contents = contents
     and type step = step
     and type origin = origin
     and module Contents = Contents

  module Commit: Ir_commit.STORE
    with type key = head
     and type value = commit
     and type node = node
     and type origin = origin
     and module Node = Node

end

module type MAKER =
  functor (C: Ir_commit.STORE) ->
    STORE with type origin = C.origin
           and type step = C.Node.step
           and type contents = C.Node.contents
           and type node = C.node
           and type commit = C.value
           and type head = C.key
           and module Origin = C.Node.Contents.Val.Origin
           and module Step = C.Node.Step
           and module Contents = C.Node.Contents
           and module Node = C.Node
           and module Commit = C

module Make: MAKER

module Rec (S: STORE): Ir_contents.S with type t = S.head
(** Same as [Ir_contents.Rec] but for block stores. *)
