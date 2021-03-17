(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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
module IO_ = IO

module Make
    (IO : IO.S)
    (Hash : Hash.S)
    (Contents : Type.S)
    (Node : Node.S with type hash = Hash.t)
    (Commit : Commit.S with type hash = Hash.t) =
struct
  module IO_list = IO_.List (IO)
  open IO.Syntax

  type contents = Hash.t * Contents.t [@@deriving irmin]
  type node = Hash.t * Node.t [@@deriving irmin]
  type commit = Hash.t * Commit.t [@@deriving irmin]

  type value = [ `Contents of contents | `Node of node | `Commit of commit ]
  [@@deriving irmin]

  type t = {
    mutable contents : contents list;
    mutable nodes : node list;
    mutable commits : commit list;
  }
  [@@deriving irmin]

  let empty () = IO.return { contents = []; nodes = []; commits = [] }

  let add t = function
    | `Contents c ->
        t.contents <- c :: t.contents;
        IO.return ()
    | `Node n ->
        t.nodes <- n :: t.nodes;
        IO.return ()
    | `Commit c ->
        t.commits <- c :: t.commits;
        IO.return ()

  let iter t f =
    let+ () = IO_list.iter_p (fun c -> f (`Contents c)) t.contents
    and+ () = IO_list.iter_p (fun n -> f (`Node n)) t.nodes
    and+ () = IO_list.iter_p (fun c -> f (`Commit c)) t.commits in
    ()
end
