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

open! Import
module Type = Repr
module Metrics = Metrics
module Diff = Diff
module Read_only = Read_only
module Append_only = Append_only
module Indexable = Indexable
module Content_addressable = Content_addressable
module Atomic_write = Atomic_write
module Contents = Contents
module Merge = Merge
module Branch = Branch
module Node = Node
module Commit = Commit
module Info = Info
module Schema = Schema
module Dot = Dot.Make
module Hash = Hash
module Path = Path
module Perms = Perms
module Key = Key
module Irmin_node = Node

exception Closed = Store_properties.Closed

module type Maker_generic_key_args = sig
  module Contents_store : Indexable.Maker_concrete_key2
  module Node_store : Indexable.Maker_concrete_key1
  module Commit_store : Indexable.Maker_concrete_key1
  module Branch_store : Atomic_write.Maker
end

module Maker_generic_key (Backend : Maker_generic_key_args) = struct
  type endpoint = unit
  type ('h, 'v) contents_key = ('h, 'v) Backend.Contents_store.key
  type 'h node_key = 'h Backend.Node_store.key
  type 'h commit_key = 'h Backend.Commit_store.key

  module Make (S : Schema.S) = struct
    module X = struct
      module Schema = S
      module Hash = S.Hash
      module Contents_key = Backend.Contents_store.Key (S.Hash) (S.Contents)
      module Node_key = Backend.Node_store.Key (S.Hash)
      module Commit_key = Backend.Commit_store.Key (S.Hash)

      module Contents = struct
        module Backend = Backend.Contents_store.Make (S.Hash) (S.Contents)
        include Contents.Store_indexable (Backend) (S.Hash) (S.Contents)
      end

      module Node = struct
        module Value =
          Node.Generic_key.Make (S.Hash) (S.Path) (S.Metadata) (Contents_key)
            (Node_key)

        module Backend = Backend.Node_store.Make (S.Hash) (Value)

        include
          Node.Generic_key.Store (Contents) (Backend) (S.Hash) (Value)
            (S.Metadata)
            (S.Path)
      end

      module Node_portable = Node.Value.Portable

      module Commit = struct
        module Commit_maker = Commit.Generic_key.Maker (Schema.Info)
        module Value = Commit_maker.Make (S.Hash) (Node_key) (Commit_key)
        module Backend = Backend.Commit_store.Make (S.Hash) (Value)

        include
          Commit.Generic_key.Store (S.Info) (Node) (Backend) (S.Hash) (Value)
      end

      module Commit_portable = Commit.Value.Portable

      module Branch = struct
        module Val = Commit.Key
        include Backend.Branch_store (S.Branch) (Val)
        module Key = S.Branch
      end

      module Slice = Slice.Make (Contents) (Node) (Commit)
      module Remote = Remote.None (Commit_key) (S.Branch)

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
        let config t = t.config

        let batch t f =
          Contents.Backend.batch t.contents @@ fun c ->
          Node.Backend.batch (snd t.nodes) @@ fun n ->
          Commit.Backend.batch (snd t.commits) @@ fun ct ->
          let contents_t = c in
          let node_t = (contents_t, n) in
          let commit_t = (node_t, ct) in
          f contents_t node_t commit_t

        let v config =
          let* contents = Contents.Backend.v config in
          let* nodes = Node.Backend.v config in
          let* commits = Commit.Backend.v config in
          let nodes = (contents, nodes) in
          let commits = (nodes, commits) in
          let+ branch = Branch.v config in
          { contents; nodes; commits; branch; config }

        let close t =
          Contents.Backend.close t.contents >>= fun () ->
          Node.Backend.close (snd t.nodes) >>= fun () ->
          Commit.Backend.close (snd t.commits) >>= fun () ->
          Branch.close t.branch
      end
    end

    include Store.Make (X)
  end
end

module Maker (CA : Content_addressable.Maker) (AW : Atomic_write.Maker) = struct
  module Indexable_store = struct
    type 'h key = 'h

    module Key = Key.Of_hash

    module Make (Hash : Hash.S) (Value : Type.S) = struct
      module CA = Content_addressable.Check_closed (CA) (Hash) (Value)
      include Indexable.Of_content_addressable (Hash) (CA)

      let v = CA.v
    end
  end

  module Maker_args = struct
    module Contents_store = Indexable.Maker_concrete_key2_of_1 (Indexable_store)
    module Node_store = Indexable_store
    module Commit_store = Indexable_store
    module Branch_store = Atomic_write.Check_closed (AW)
  end

  include Maker_generic_key (Maker_args)
end

module KV_maker (CA : Content_addressable.Maker) (AW : Atomic_write.Maker) =
struct
  type metadata = unit
  type hash = Schema.default_hash
  type info = Info.default

  module Maker = Maker (CA) (AW)
  include Maker
  module Make (C : Contents.S) = Maker.Make (Schema.KV (C))
end

module Of_backend = Store.Make

module type Tree = Tree.S
module type S = Store.S

type config = Conf.t
type 'a diff = 'a Diff.t

module type Maker = Store.Maker
module type KV = Store.KV
module type KV_maker = Store.KV_maker

module Generic_key = struct
  include Store.Generic_key

  module type Maker_args = Maker_generic_key_args

  module Maker = Maker_generic_key
end

module Backend = struct
  module Conf = Conf
  module Slice = Slice
  module Remote = Remote

  module type S = Backend.S

  module Watch = Watch
  module Lock = Lock
  module Lru = Lru
end

let version = Version.current

module Sync = Sync

type remote = Remote.t = ..

let remote_store (type t) (module M : Generic_key.S with type t = t) (t : t) =
  let module X : Store.Generic_key.S with type t = t = M in
  Sync.remote_store (module X) t

module Metadata = Metadata
module Json_tree = Store.Json_tree
module Export_for_backends = Export_for_backends
module Storage = Storage

module Of_storage (M : Storage.Make) (H : Hash.S) (V : Contents.S) = struct
  module CA = Storage.Content_addressable (M)
  module AW = Storage.Atomic_write (M)
  module Maker = Maker (CA) (AW)

  include Maker.Make (struct
    module Hash = H
    module Contents = V
    module Info = Info.Default
    module Metadata = Metadata.None
    module Path = Path.String_list
    module Branch = Branch.String
    module Node = Node.Make (Hash) (Path) (Metadata)
    module Commit = Commit.Make (Hash)
  end)
end
