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
    (Tag: Ir_tag.STORE) =
struct

  type contents = (Contents.key * Contents.value) list
  type nodes = (Node.key * Node.value) list
  type commits = (Commit.key * Commit.value) list
  type tags = (Tag.key * Tag.value) list

  type t = {
    contents: (Contents.key * Contents.value) list;
    nodes   : (Node.key * Node.value) list;
    commits : (Commit.key * Commit.value) list;
    tags    : (Tag.key * Tag.value) list;
  }

  let create ?(contents=[]) ?(nodes=[]) ?(commits=[]) ?(tags=[]) () =
    { contents; nodes; commits; tags }

  let contents t = t.contents
  let nodes t = t.nodes
  let commits t = t.commits
  let tags t = t.tags

  module M (K: Tc.S0)(V: Tc.S0) = Tc.List( Tc.Pair(K)(V) )
  module Ct = M(Contents.Key)(Contents.Val)
  module No = M(Node.Key)(Node.Val)
  module Cm = M(Commit.Key)(Commit.Val)
  module Ta = M(Tag.Key)(Tag.Val)
  module T = Tc.Pair( Tc.Pair(Ct)(No) )( Tc.Pair(Cm)(Ta) )

  let explode t = (t.contents, t.nodes), (t.commits, t.tags)
  let implode ((contents, nodes), (commits, tags)) =
    { contents; nodes; commits; tags }

  let compare x y = T.compare (explode x) (explode y)
  let equal x y = T.equal (explode x) (explode y)
  let hash = Hashtbl.hash
  let write t buf = T.write (explode t) buf
  let read b = implode (T.read b)
  let size_of t = T.size_of (explode t)

  let to_sexp t =
    let open Sexplib.Type in
    List [
      List [ Atom "contents"; Ct.to_sexp t.contents ];
      List [ Atom "nodes"   ; No.to_sexp t.nodes ];
      List [ Atom "commmits"; Cm.to_sexp t.commits ];
      List [ Atom "tags"    ; Ta.to_sexp t.tags ];
    ]

  let to_json t =
    `O [
      ("contents", Ct.to_json t.contents);
      ("nodes"   , No.to_json t.nodes);
      ("commits" , Cm.to_json t.commits);
      ("tags"    , Ta.to_json t.tags);
    ]

  let of_json j =
    let contents = Ezjsonm.find j ["contents"] |> Ct.of_json in
    let nodes = Ezjsonm.find j ["nodes"] |> No.of_json in
    let commits = Ezjsonm.find j ["commits"] |> Cm.of_json in
    let tags = Ezjsonm.find j ["tags"] |> Ta.of_json in
    { contents; nodes; commits; tags }

end
