(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2017 Grégoire Henry <gregoire.henry@ocamlpro.com>
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

module Make (P: S.PRIVATE): sig
  include S.TREE with type key = P.Node.Path.t
                  and type step = P.Node.Path.step
                  and type metadata = P.Node.Val.metadata
                  and type contents = P.Contents.value

  val import: P.Repo.t -> P.Node.key -> node
  val export: P.Repo.t -> [> `Write] P.Contents.t -> [> `Write] P.Node.t ->
    node -> P.Node.key Lwt.t
  val dump: tree Fmt.t
  val equal: tree -> tree -> bool
  val node_t: node Type.t
  val tree_t: tree Type.t
  val hash: tree -> [`Contents of (P.Hash.t * metadata) | `Node of P.Hash.t]
  val of_private_node: P.Repo.t -> P.Node.value -> node
  val to_private_node: node -> P.Node.value option Lwt.t
end
