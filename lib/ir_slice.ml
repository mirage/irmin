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

let src = Logs.Src.create "irmin.slice" ~doc:"Irmin bundles"
module Log = (val Logs.src_log src : Logs.LOG)

module Make
    (Contents: Ir_s.CONTENTS_STORE)
    (Node: Ir_s.NODE_STORE)
    (Commit: Ir_s.COMMIT_STORE) =
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

  module Enc (M: Tc.S0) = struct
    include M
    let size_of t =
      let cstruct = Tc.write_cstruct (module M) t in
      Tc.Cstruct.size_of cstruct
    let read buf =
      let cstruct = Tc.Cstruct.read buf in
      Tc.read_cstruct (module M) cstruct
    let write t buf =
      let cstruct = Tc.write_cstruct (module M) t in
      Tc.Cstruct.write cstruct buf
  end
  module M (K: Tc.S0)(V: Tc.S0) = Tc.List(Tc.Pair(K)(Enc(V)))
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
