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

module Maker (CA : Content_addressable.Maker) (AW : Atomic_write.Maker) = struct
  type endpoint = unit
  type 'h contents_key = 'h
  type 'h node_key = 'h
  type 'h commit_key = 'h

  module Make (S : Schema.S) = struct
    module CA (Hash : Hash.S) (Value : Type.S) : sig
      include
        Indexable.S
          with type hash = Hash.t
           and type key = Hash.t
           and type value = Value.t

      include Store_properties.Of_config with type 'a t := 'a t
      (** @inline *)
    end = struct
      module CA = Content_addressable.Check_closed (CA) (Hash) (Value)
      include Indexable.Of_content_addressable (Hash) (CA)

      let v = CA.v
    end

    module AW = Atomic_write.Check_closed (AW)

    module X = struct
      module Schema = S
      module Hash = S.Hash
      module XKey = Key.Of_hash (S.Hash)
      module Commit_key = XKey
      module Node_key = XKey

      module Contents = struct
        module CA =
          CA (S.Hash) (* XXX: Try putting this back to [Hash] *) (S.Contents)

        include Contents.Store (CA) (S.Hash) (S.Contents)
      end

      module Node = struct
        module Value =
          Node.Make_generic_key (S.Hash) (S.Path) (S.Metadata) (XKey) (Node_key)

        module CA = CA (S.Hash) (Value)

        include
          Node.Store (Contents) (CA) (Node_key) (S.Hash) (Value) (S.Metadata)
            (S.Path)
      end

      module Node_portable =
        Irmin_node.Portable.Of_node (XKey) (Node_key) (Node.Val)

      module Commit = struct
        module Commit_maker = Commit.Maker (Schema.Info)
        module Value = Commit_maker.Make (S.Hash) (Node_key) (Commit_key)
        module CA = CA (S.Hash) (Value)
        include Commit.Store (S.Info) (Node) (CA) (S.Hash) (Value)
      end

      module Branch = struct
        module Val = Commit.Key
        include AW (S.Branch) (Val)
        module Key = S.Branch
      end

      module Slice = Slice.Make (Contents) (Node) (Commit)
      module Remote = Remote.None (S.Hash) (S.Branch)

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

module KV_maker (CA : Content_addressable.Maker) (AW : Atomic_write.Maker) =
struct
  type metadata = unit
  type hash = Schema.default_hash

  module Maker = Maker (CA) (AW)
  include Maker
  module Make (C : Contents.S) = Maker.Make (Schema.KV (C))
end

module Of_private = Store.Make

module type Tree = Tree.S
module type S = Store.S

type config = Conf.t
type 'a diff = 'a Diff.t

module type Maker = Store.Maker
module type KV = Store.KV
module type KV_maker = Store.KV_maker

module Generic_key = Store.Generic_key

module Private = struct
  module Conf = Conf
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

let remote_store (type t) (module M : Generic_key.S with type t = t) (t : t) =
  let module X : Store.Generic_key.S with type t = t = M in
  Sync.remote_store (module X) t

module Metadata = Metadata
module Json_tree = Store.Json_tree
module Export_for_backends = Export_for_backends
