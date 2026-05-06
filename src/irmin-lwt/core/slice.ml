(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

include Slice_intf

module Make
    (Contents : Contents.Store)
    (Node : Node.Store)
    (Commit : Commit.Store) =
struct
  type contents = Contents.Hash.t * Contents.Val.t [@@deriving irmin]
  type node = Node.Hash.t * Node.Val.t [@@deriving irmin]
  type commit = Commit.Hash.t * Commit.Val.t [@@deriving irmin]

  type value = [ `Contents of contents | `Node of node | `Commit of commit ]
  [@@deriving irmin]

  type t = {
    mutable contents : contents list;
    mutable nodes : node list;
    mutable commits : commit list;
  }
  [@@deriving irmin]

  let empty () = Lwt.return { contents = []; nodes = []; commits = [] }

  let add t = function
    | `Contents c ->
        t.contents <- c :: t.contents;
        Lwt.return_unit
    | `Node n ->
        t.nodes <- n :: t.nodes;
        Lwt.return_unit
    | `Commit c ->
        t.commits <- c :: t.commits;
        Lwt.return_unit

  let iter t f =
    Lwt.join
      [
        Lwt_list.iter_p (fun c -> f (`Contents c)) t.contents;
        Lwt_list.iter_p (fun n -> f (`Node n)) t.nodes;
        Lwt_list.iter_p (fun c -> f (`Commit c)) t.commits;
      ]
end
