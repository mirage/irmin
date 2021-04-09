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

open! Import
module Type = Repr
module Diff = Diff
module Read_only = Read_only
module Append_only = Append_only
module Content_addressable = Content_addressable
module Atomic_write = Atomic_write
module Contents = Contents
module Merge = Merge
module Branch = Branch
module Info = Info
module Dot = Dot.Make
module Hash = Hash
module Path = Path
module Perms = Perms

exception Closed = Store_properties.Closed

module Maker_ext
    (CA : Content_addressable.Maker)
    (AW : Atomic_write.Maker)
    (N : Node.S)
    (CT : Commit.S with type hash = N.hash) =
struct
  module Make
      (M : Metadata.S with type t = N.metadata)
      (C : Contents.S)
      (P : Path.S with type step = N.step)
      (B : Branch.S)
      (H : Hash.S with type t = N.hash) =
  struct
    module CA = Content_addressable.Check_closed (CA)
    module AW = Atomic_write.Check_closed (AW)

    module X = struct
      module Hash = H

      module Contents = struct
        module CA = struct
          module Key = Hash
          module Val = C
          include CA.Make (Key) (Val)
        end

        include Contents.Store (CA)
      end

      module Node = struct
        module CA = struct
          module Key = Hash
          module Val = N
          include CA.Make (Key) (Val)
        end

        include Node.Store (Contents) (P) (M) (CA)
      end

      module Commit = struct
        module CA = struct
          module Key = Hash
          module Val = CT
          include CA.Make (Key) (Val)
        end

        include Commit.Store (Node) (CA)
      end

      module Branch = struct
        module Key = B
        module Val = H
        include AW.Make (Key) (Val)
      end

      module Slice = Slice.Make (Contents) (Node) (Commit)
      module Remote = Remote.None (H) (B)

      module Repo = struct
        type t = {
          config : Conf.t;
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

    include Store.Make (X)
  end
end

module Maker (CA : Content_addressable.Maker) (AW : Atomic_write.Maker) = struct
  module Make
      (M : Metadata.S)
      (C : Contents.S)
      (P : Path.S)
      (B : Branch.S)
      (H : Hash.S) =
  struct
    module N = Node.Make (H) (P) (M)
    module CT = Commit.Make (H)
    module Maker = Maker_ext (CA) (AW) (N) (CT)
    include Maker.Make (M) (C) (P) (B) (H)
  end
end

module Of_private = Store.Make

module type Tree = Tree.S
module type S = Store.S

type config = Conf.t
type 'a diff = 'a Diff.t

module type Maker = Store.Maker

module type KV =
  S
    with type key = string list
     and type step = string
     and type branch = string
     and type Private.Remote.endpoint = unit

module type KV_maker = sig
  type metadata

  module Make (C : Contents.S) :
    KV with type contents = C.t and type metadata = metadata
end

module Private = struct
  module Conf = Conf
  module Node = Node
  module Commit = Commit
  module Slice = Slice
  module Remote = Remote

  module type S = Private.S

  module Watch = Watch
  module Lock = Lock
  module Lru = Lru
end

let version = Version.current

module Sync = Sync

type remote = Remote.t = ..

let remote_store (type t) (module M : S with type t = t) (t : t) =
  let module X : Store.S with type t = t = M in
  Sync.remote_store (module X) t

module Metadata = Metadata
module Json_tree = Store.Json_tree
module Export_for_backends = Export_for_backends
