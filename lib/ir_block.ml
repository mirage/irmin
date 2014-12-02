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

  module Path: Ir_path.S
    with type step = step

  module Contents: Ir_contents.STORE_EXT
    with type value = contents

  module Node: Ir_node.STORE_EXT
    with type step = step
     and type value = node
     and type Contents.t = Contents.t
     and type Contents.key = Contents.key
     and type Contents.value = Contents.value
     and module Path = Path

  module Commit: Ir_commit.STORE_EXT
    with type key = head
     and type value = commit
     and module Node = Node

end

module Make
    (C: Ir_contents.STORE)
    (N: Ir_node.STORE with type Val.contents = C.key)
    (H: Ir_commit.STORE with type Val.node = N.key) =
struct

  type step = N.Path.step
  type contents = C.value
  type node = N.value
  type commit = H.value
  type head = H.key

  module Path = N.Path
  module Contents = Ir_contents.Make_ext(C)
  module Node = Ir_node.Make_ext(C)(N)
  module Commit = Ir_commit.Make_ext(C)(N)(H)

end
