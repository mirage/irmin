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

  type contents
  type node
  type commit

  module Contents: Ir_contents.STORE with type value = contents
  module Node: Ir_node.STORE with type value = node and type contents = contents
  module Commit: Ir_commit.STORE
    with type value = commit
     and type node = node
     and type Node.contents = contents

  module Graph: Ir_graph.S
    with type V.t = (Contents.key, Node.key, Commit.key, unit) Ir_graph.vertex
  (** Graphs of blocks. *)

end

module type MAKER =
  functor (K: Ir_uid.S) ->
  functor (O: Ir_origin.S) ->
  functor (P: Ir_path.S) ->
  functor (C: Ir_contents.S) ->
    STORE with type contents = C.t
           and type Commit.origin = O.t
           and type Node.path = P.t

module Make (Contents: Ir_ao.MAKER) (Node: Ir_ao.MAKER) (Commit: Ir_ao.MAKER):
  MAKER

module Rec (S: STORE): Ir_contents.S with type t = S.Commit.key
(** Same as [Ir_contents.Rec] but for block stores. *)
