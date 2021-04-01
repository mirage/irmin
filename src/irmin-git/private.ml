(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Branch.S) =
struct
  module Hash = Irmin.Hash.Make (G.Hash)

  module Contents = struct
    module S = Contents.Make (G) (C)
    include Irmin.Contents.Store (S)
  end

  module Node = struct
    module S = Node.Make (G) (P)
    include Irmin.Private.Node.Store (Contents) (P) (Metadata) (S)
  end

  module Commit = struct
    module S = Commit.Make (G)
    include Irmin.Private.Commit.Store (Node) (S)
  end

  module Branch = struct
    module Key = B
    module Val = Hash
    module S = Atomic_write.Make (B) (G)
    include Irmin.Atomic_write.Wrap_close (S)

    let v ?lock ~head ~bare t = S.v ?lock ~head ~bare t >|= v
  end

  module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)

  module Repo = struct
    let handle_git_err = function
      | Ok x -> Lwt.return x
      | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e

    type t = { config : Irmin.config; closed : bool ref; g : G.t; b : Branch.t }

    let branch_t t = t.b
    let contents_t t : 'a Contents.t = Contents.S.v ~closed:t.closed t.g
    let node_t t : 'a Node.t = (contents_t t, Node.S.v ~closed:t.closed t.g)
    let commit_t t : 'a Commit.t = (node_t t, Commit.S.v ~closed:t.closed t.g)
    let batch t f = f (contents_t t) (node_t t) (commit_t t)

    type config = {
      root : string;
      dot_git : string option;
      level : int option;
      buffers : int option;
      head : G.Reference.t option;
      bare : bool;
    }

    let config c =
      let root =
        match Irmin.Private.Conf.get c Conf.root with
        | None -> "."
        | Some d -> d
      in
      let dot_git = Irmin.Private.Conf.get c Conf.dot_git in
      let level = Irmin.Private.Conf.get c Conf.level in
      let head = Irmin.Private.Conf.get c Conf.head in
      let bare = Irmin.Private.Conf.get c Conf.bare in
      let buffers = Irmin.Private.Conf.get c Conf.buffers in
      { root; dot_git; level; head; buffers; bare }

    let fopt f = function None -> None | Some x -> Some (f x)

    let v conf =
      let { root; dot_git; head; bare; _ } = config conf in
      let dotgit = fopt Fpath.v dot_git in
      let root = Fpath.v root in
      let* g = G.v ?dotgit root >>= handle_git_err in
      let+ b = Branch.v ~head ~bare g in
      { g; b; closed = ref false; config = conf }

    let close t = Branch.close t.b >|= fun () -> t.closed := true
  end

  module Remote = struct
    include Remote.Make (G) (S) (B)

    let v repo = Lwt.return repo.Repo.g
  end

  let git_of_repo r = r.Repo.g

  let repo_of_git ?head ?(bare = true) ?lock g =
    let+ b = Branch.v ?lock ~head ~bare g in
    { Repo.config = Irmin.Private.Conf.empty; closed = ref false; g; b }
end
