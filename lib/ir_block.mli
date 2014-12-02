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
     and module Contents = Contents
     and module Path = Path

  module Commit: Ir_commit.STORE_EXT
    with type key = head
     and type value = commit
     and module Node = Node

end

module Make
    (C: Ir_contents.STORE)
    (N: Ir_node.STORE with type Val.contents = C.key)
    (S: Ir_commit.STORE with type Val.node = N.key):
  STORE with type step = N.Path.step
         and type contents = C.value
         and type node = N.value
         and type commit = S.value
         and type head = S.key
         and module Path = N.Path
         and module Contents = Ir_contents.Make_ext(C)
         and module Node = Ir_node.Make_ext(C)(N)
         and module Commit = Ir_commit.Make_ext(C)(N)(S)
