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

include Irmin_git_intf
open! Import
module Conf = Conf
module Metadata = Metadata
module Branch = Branch
module Reference = Reference
module Schema = Schema

let config = Conf.init

type reference = Reference.t [@@deriving irmin]

module Maker_ext
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t) =
struct
  type endpoint = Mimic.ctx * Smart_git.Endpoint.t

  module Make
      (Schema : Schema.S
                  with type Hash.t = G.hash
                   and type Node.t = G.Value.Tree.t
                   and type Commit.t = G.Value.Commit.t) =
  struct
    module P = Private.Make (G) (S) (Schema)
    include Irmin.Of_private (P)

    let git_of_repo = P.git_of_repo
    let repo_of_git = P.repo_of_git

    let git_commit (repo : Repo.t) (h : commit) : G.Value.Commit.t option Lwt.t
        =
      let h = Commit.hash h in
      G.read (git_of_repo repo) h >|= function
      | Ok (Git.Value.Commit c) -> Some c
      | _ -> None

    module Git = G
  end
end

module Mem = struct
  include Git.Mem.Store

  let confs = Hashtbl.create 10
  let find_conf c = Hashtbl.find_opt confs c

  let add_conf c t =
    Hashtbl.replace confs c t;
    t

  let v' ?dotgit root = v ?dotgit root

  let v ?dotgit root =
    let conf = (dotgit, root) in
    match find_conf conf with
    | Some x -> Lwt.return x
    | None -> v' ?dotgit root >|= add_conf conf
end

module Maker
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t) =
struct
  module Maker = Maker_ext (G) (S)

  type endpoint = Maker.endpoint

  module Make
      (Sc : Schema.S
              with type Hash.t = G.hash
               and type Node.t = G.Value.Tree.t
               and type Commit.t = G.Value.Commit.t) =
    Maker.Make (Sc)
end

module No_sync = struct
  type error =
    [ `Not_found | `Msg of string | `Exn of exn | `Cycle | `Invalid_flow ]

  let pp_error _ _ = assert false

  let fetch ?push_stdout:_ ?push_stderr:_ ?threads:_ ~ctx:_ _ _ ?version:_
      ?capabilities:_ ?deepen:_ _ =
    assert false

  let push ~ctx:_ _ _ ?version:_ ?capabilities:_ _ = assert false
end

module Content_addressable (G : Git.S) = struct
  module G = struct
    include G

    let v ?dotgit:_ _root = assert false
  end

  module type S = Irmin.Content_addressable.S with type key = G.Hash.t

  module Maker = Maker_ext (G) (No_sync)

  module Make (V : Irmin.Type.S) = struct
    module V = struct
      include V

      let merge = Irmin.Merge.default Irmin.Type.(option V.t)
    end

    module Schema = Schema.Make (G) (V) (Reference)
    module M = Maker.Make (Schema)
    module X = M.Private.Contents

    let state t =
      let+ r = M.repo_of_git (snd t) in
      M.Private.Repo.contents_t r

    type 'a t = bool ref * G.t
    type key = X.key
    type value = X.value

    let with_state0 f t =
      let* t = state t in
      f t

    let with_state1 f t x =
      let* t = state t in
      f t x

    let add = with_state1 X.add
    let pp_key = Irmin.Type.pp X.Key.t
    let equal_key = Irmin.Type.(unstage (equal X.Key.t))

    let unsafe_add t k v =
      let+ k' = with_state1 X.add t v in
      if equal_key k k' then ()
      else
        Fmt.failwith
          "[Git.unsafe_append] %a is not a valid key. Expecting %a instead.\n"
          pp_key k pp_key k'

    let find = with_state1 X.find
    let mem = with_state1 X.mem
    let clear = with_state0 X.clear
    let close = with_state0 X.close
    let batch t f = f t
  end
end

module Atomic_write (G : Git.S) = struct
  module type S = Irmin.Atomic_write.S with type value = G.Hash.t

  module Make (K : Irmin.Branch.S) = struct
    module K = struct
      include K

      let master =
        match Irmin.Type.of_string K.t "master" with
        | Ok x -> x
        | Error (`Msg e) -> failwith e
    end

    module AW = Atomic_write.Make (Branch.Make (K)) (G)
    include Atomic_write.Check_closed (AW)
  end
end

module KV
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t) =
struct
  module Maker = Maker (G) (S)
  module Branch = Branch.Make (Irmin.Branch.String)

  type endpoint = Maker.endpoint
  type metadata = Metadata.t
  type branch = Branch.t

  module Make (C : Irmin.Contents.S) = Maker.Make (Schema.Make (G) (C) (Branch))
end

module Ref
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t) =
struct
  module Maker = Maker_ext (G) (S)

  type endpoint = Maker.endpoint
  type branch = reference

  module Make (C : Irmin.Contents.S) =
    Maker.Make (Schema.Make (G) (C) (Reference))
end

include Conf

module Generic_KV
    (CA : Irmin.Content_addressable.Maker)
    (AW : Irmin.Atomic_write.Maker) =
struct
  module G = Mem

  type endpoint = unit
  type metadata = Metadata.t

  module Schema (C : Irmin.Contents.S) = struct
    module Metadata = Metadata
    module Contents = C
    module Path = Irmin.Path.String_list
    module Branch = Branch.Make (Irmin.Branch.String)
    module Hash = Irmin.Hash.Make (Mem.Hash)
    module Node = Node.Make (G) (Path)
    module Commit = Commit.Make (G)
    module Info = Irmin.Info.Default
  end

  module Make (C : Irmin.Contents.S) = struct
    module Sc = Schema (C)

    (* We use a dummy store to get the serialisation functions. This is
       probably not necessary and we could use Git.Value.Raw instead. *)
    module Dummy = struct
      module G = Mem
      module Maker = Maker (G) (No_sync)
      module S = Maker.Make (Sc)
      include S.Private
    end

    module CA = Irmin.Content_addressable.Check_closed (CA)
    module AW = Irmin.Atomic_write.Check_closed (AW)

    module X = struct
      module Schema = Sc
      module Hash = Dummy.Hash
      module Info = Irmin.Info.Default

      module Contents = struct
        module V = Dummy.Contents.Val
        module CA = CA (Hash) (V)
        include Irmin.Contents.Store (CA) (Hash) (V)
      end

      module Node = struct
        module V = Dummy.Node.Val
        module CA = CA (Hash) (V)

        include
          Irmin.Node.Store (Contents) (CA) (Hash) (V) (Dummy.Node.Metadata)
            (Schema.Path)
      end

      module Commit = struct
        module V = Dummy.Commit.Val
        module CA = CA (Hash) (V)
        include Irmin.Commit.Store (Info) (Node) (CA) (Hash) (V)
      end

      module Branch = struct
        module Key = Dummy.Branch.Key
        module Val = Dummy.Branch.Val
        include AW (Key) (Val)
      end

      module Slice = Dummy.Slice
      module Remote = Irmin.Private.Remote.None (Branch.Val) (Branch.Key)

      module Repo = struct
        (* FIXME: remove duplication with irmin.mli *)
        type t = {
          config : Irmin.config;
          contents : read Contents.t;
          nodes : read Node.t;
          commits : read Commit.t;
          branch : Branch.t;
        }

        let contents_t t = t.contents
        let node_t t = t.nodes
        let commit_t t = t.commits
        let branch_t t = t.branch

        let batch t f =
          Contents.CA.batch t.contents @@ fun c ->
          Node.CA.batch (snd t.nodes) @@ fun n ->
          Commit.CA.batch (snd t.commits) @@ fun ct ->
          let contents_t = c in
          let node_t = (contents_t, n) in
          let commit_t = (node_t, ct) in
          f contents_t node_t commit_t

        let v config =
          let* contents = Contents.CA.v config in
          let* nodes = Node.CA.v config in
          let* commits = Commit.CA.v config in
          let nodes = (contents, nodes) in
          let commits = (nodes, commits) in
          let+ branch = Branch.v config in
          { contents; nodes; commits; branch; config }

        let close t =
          Contents.CA.close t.contents >>= fun () ->
          Node.CA.close (snd t.nodes) >>= fun () ->
          Commit.CA.close (snd t.commits) >>= fun () -> Branch.close t.branch
      end
    end

    include Irmin.Of_private (X)
  end
end

(* Enforce that {!KV} is a sub-type of {!Irmin.KV_maker}. *)
module KV_is_a_KV_maker : Irmin.KV_maker = KV (Mem) (No_sync)

(* Enforce that {!Generic_KV} is a sub-type of {!Irmin.KV_maker}. *)
module Generic_KV_is_a_KV_maker : Irmin.KV_maker =
  Generic_KV (Irmin_mem.Content_addressable) (Irmin_mem.Atomic_write)
