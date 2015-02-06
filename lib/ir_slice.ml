(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt

module Log = Log.Make(struct let section = "NODE" end)

module type S = sig
  include Tc.S0
  type contents
  type node
  type commit
  val create: unit -> t Lwt.t
  val add_contents: t -> contents -> unit Lwt.t
  val add_node: t -> node -> unit Lwt.t
  val add_commit: t -> commit -> unit Lwt.t
  val iter_contents: t -> (contents -> unit Lwt.t) -> unit Lwt.t
  val iter_nodes: t -> (node -> unit Lwt.t) -> unit Lwt.t
  val iter_commits: t -> (commit -> unit Lwt.t) -> unit Lwt.t
end

module Make
    (Contents: Ir_contents.STORE)
    (Node: Ir_node.STORE)
    (Commit: Ir_commit.STORE) =
struct

  type contents = Contents.key * Contents.value
  type node = Node.key * Node.value
  type commit = Commit.key * Commit.value

  type t = {
    mutable contents: (Contents.key * Contents.value) list;
    mutable nodes   : (Node.key * Node.value) list;
    mutable commits : (Commit.key * Commit.value) list;
  }

  let create () =
    return { contents = []; nodes = []; commits = [] }

  let add_contents t c = t.contents <- c :: t.contents; return_unit
  let add_node t n = t.nodes <- n :: t.nodes; return_unit
  let add_commit t c = t.commits <- c :: t.commits; return_unit

  let iter_contents t f = Lwt_list.iter_p f t.contents
  let iter_nodes t f = Lwt_list.iter_p f t.nodes
  let iter_commits t f = Lwt_list.iter_p f t.commits

  module M (K: Tc.S0)(V: Tc.S0) = Tc.List( Tc.Pair(K)(V) )
  module Ct = M(Contents.Key)(Contents.Val)
  module No = M(Node.Key)(Node.Val)
  module Cm = M(Commit.Key)(Commit.Val)
  module T = Tc.Triple (Ct)(No)(Cm)

  let explode t = (t.contents, t.nodes, t.commits)
  let implode (contents, nodes, commits) = { contents; nodes; commits }

  let t = Tc.biject (module T) implode explode
  let compare = Tc.compare t
  let equal = Tc.equal t
  let hash = Tc.hash t
  let write = Tc.write t
  let read = Tc.read t
  let size_of = Tc.size_of t

  let to_json t =
    `O [
      ("contents", Ct.to_json t.contents);
      ("nodes"   , No.to_json t.nodes);
      ("commits" , Cm.to_json t.commits);
    ]

  let of_json j =
    let contents = Ezjsonm.find j ["contents"] |> Ct.of_json in
    let nodes = Ezjsonm.find j ["nodes"] |> No.of_json in
    let commits = Ezjsonm.find j ["commits"] |> Cm.of_json in
    { contents; nodes; commits }


end
