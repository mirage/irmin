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

module Make
    (Contents : S.CONTENTS_STORE)
    (Node : S.NODE_STORE)
    (Commit : S.COMMIT_STORE) =
struct
  type contents = Contents.key * Contents.value

  type node = Node.key * Node.value

  type commit = Commit.key * Commit.value

  type value = [ `Contents of contents | `Node of node | `Commit of commit ]

  type t = {
    mutable contents : (Contents.key * Contents.value) list;
    mutable nodes : (Node.key * Node.value) list;
    mutable commits : (Commit.key * Commit.value) list;
  }

  let t =
    let open Type in
    record "slice" (fun contents nodes commits -> { contents; nodes; commits })
    |+ field "contents"
         (list (pair Contents.Key.t Contents.Val.t))
         (fun t -> t.contents)
    |+ field "nodes" (list (pair Node.Key.t Node.Val.t)) (fun t -> t.nodes)
    |+ field "commits"
         (list (pair Commit.Key.t Commit.Val.t))
         (fun t -> t.commits)
    |> sealr

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

  let contents_t = Type.pair Contents.Key.t Contents.Val.t

  let node_t = Type.pair Node.Key.t Node.Val.t

  let commit_t = Type.pair Commit.Key.t Commit.Val.t

  let value_t =
    let open Type in
    variant "slice" (fun contents node commit ->
      function
      | `Contents x -> contents x | `Node x -> node x | `Commit x -> commit x)
    |~ case1 "contents" contents_t (fun x -> `Contents x)
    |~ case1 "node" node_t (fun x -> `Node x)
    |~ case1 "commit" commit_t (fun x -> `Commit x)
    |> sealv
end
