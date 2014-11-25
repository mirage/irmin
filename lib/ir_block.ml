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

module type STORE = sig

  type step
  type contents
  type node
  type commit
  type head

  module Step: Ir_step.S
    with type t = step

  module StepMap: Map.S
    with type key = step

  module Contents: Ir_contents.STORE
    with type value = contents

  module Node: Ir_node.STORE
    with type step = step
     and type value = node
     and module Contents = Contents
     and module Step = Step
     and module StepMap = StepMap

  module Commit: Ir_commit.STORE
    with type key = head
     and type value = commit
     and module Node = Node

end

module type MAKER =
  functor (C: Ir_commit.STORE) ->
    STORE with type step = C.Node.step
           and type contents = C.Node.contents
           and type node = C.node
           and type commit = C.value
           and type head = C.key
           and module Step = C.Node.Step
           and module StepMap = C.Node.StepMap
           and module Contents = C.Node.Contents
           and module Node = C.Node
           and module Commit = C

module Make (C: Ir_commit.STORE) = struct

  type step = C.Node.step
  type contents = C.Node.contents
  type node = C.node
  type commit = C.value
  type head = C.key

  module Step = C.Node.Step
  module StepMap = C.Node.StepMap
  module Contents = C.Node.Contents
  module Node = C.Node
  module Commit = C

end
