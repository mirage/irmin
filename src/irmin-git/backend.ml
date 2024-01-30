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

open Import

module type G = sig
  include Git.S

  val v : ?dotgit:Fpath.t -> Fpath.t -> (t, error) result Lwt.t
end

module Make
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t)
    (Schema : Schema.S
                with type Hash.t = G.hash
                 and type Node.t = G.Value.Tree.t
                 and type Commit.t = G.Value.Commit.t) =
struct
  module Hash = Irmin.Hash.Make (G.Hash)
  module Schema = Schema
  module Key = Irmin.Key.Of_hash (Hash)
  module Commit_key = Key
  module Node_key = Key

  module Contents = struct
    module S = Contents.Make (G) (Schema.Contents)
    include Irmin.Contents.Store (S) (S.Hash) (S.Val)
  end

  module Node = struct
    module S = Node.Store (G) (Schema.Path)

    include
      Irmin.Node.Store (Contents) (S) (S.Key) (S.Val) (Metadata) (Schema.Path)
  end

  module Node_portable = Irmin.Node.Portable.Of_node (Node.Val)

  module Commit = struct
    module S = Commit.Store (G)
    include Irmin.Commit.Store (Schema.Info) (Node) (S) (S.Hash) (S.Val)
  end

  module Commit_portable = Irmin.Commit.Portable.Of_commit (Commit.S.Val)

  module Branch = struct
    module Key = Schema.Branch
    module Val = Commit_key
    module S = Atomic_write.Make (Schema.Branch) (G)
    include Atomic_write.Check_closed (S)

    let v ?lock ~head ~bare t = S.v ?lock ~head ~bare t |> v
  end

  module Slice = Irmin.Backend.Slice.Make (Contents) (Node) (Commit)

  module Repo = struct
    let handle_git_err = function
      | Ok x -> x
      | Error e -> Fmt.kstr failwith "%a" G.pp_error e

    type t = { config : Irmin.config; closed : bool ref; g : G.t; b : Branch.t }

    let branch_t t = t.b
    let contents_t t : 'a Contents.t = (t.closed, t.g)
    let node_t t : 'a Node.t = (contents_t t, (t.closed, t.g))
    let commit_t t : 'a Commit.t = (node_t t, (t.closed, t.g))
    let batch ?lock:_ t f = f (contents_t t) (node_t t) (commit_t t)

    type config = {
      root : string;
      dot_git : string option;
      level : int option;
      buffers : int option;
      head : G.Reference.t option;
      bare : bool;
    }

    let config c =
      let module C = Irmin.Backend.Conf in
      let root = C.find_root c |> Option.value ~default:"." in
      let dot_git = C.get c Conf.Key.dot_git in
      let level = C.get c Conf.Key.level in
      let head = C.get c Conf.Key.head in
      let bare = C.get c Conf.Key.bare in
      let buffers = C.get c Conf.Key.buffers in
      { root; dot_git; level; head; buffers; bare }

    let fopt f = function None -> None | Some x -> Some (f x)

    let v ~sw:_ conf =
      let { root; dot_git; head; bare; _ } = config conf in
      let dotgit = fopt Fpath.v dot_git in
      let root = Fpath.v root in
      let g = Lwt_eio.run_lwt @@ fun () -> G.v ?dotgit root in
      let g = handle_git_err g in
      let b = Branch.v ~head ~bare g in
      { g; b; closed = ref false; config = (conf :> Irmin.config) }

    let config t = t.config

    let close t =
      Branch.close t.b;
      t.closed := true
  end

  module Remote = struct
    include Remote.Make (G) (S) (Schema.Branch)

    let v repo = repo.Repo.g
  end

  let git_of_repo r = r.Repo.g

  let repo_of_git ?head ?(bare = true) ?lock g =
    let b = Branch.v ?lock ~head ~bare g in
    {
      Repo.config = Irmin.Backend.Conf.empty Conf.spec;
      closed = ref false;
      g;
      b;
    }
end
