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

module type S = sig
  include Tc.S0
  type contents
  type nodes
  type commits
  type tags
  val create:
    ?contents:contents -> ?nodes:nodes ->
    ?commits:commits -> ?tags:tags ->
    unit -> t
  val contents: t -> contents
  val nodes: t -> nodes
  val commits: t -> commits
  val tags: t -> tags
end

module Make
    (Contents: Ir_contents.STORE)
    (Node: Ir_node.STORE)
    (Commit: Ir_commit.STORE)
    (Tag: Ir_tag.STORE):
  S with type contents = (Contents.key * Contents.value) list
     and type nodes = (Node.key * Node.value) list
     and type commits = (Commit.key * Commit.value) list
     and type tags = (Tag.key * Tag.value) list
