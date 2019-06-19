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

open Lwt.Infix
module Type = Type
module Diff = Diff
module Content_addressable = Store.Content_addressable

module Contents = struct
  include Contents

  module type S = S.CONTENTS

  module type STORE = S.CONTENTS_STORE
end

module Merge = Merge

module Branch = struct
  include Branch

  module type S = S.BRANCH

  module type STORE = S.BRANCH_STORE
end

module Info = Info
module Dot = Dot.Make

module Hash = struct
  include Hash

  module type S = S.HASH
end

module Path = struct
  include Path

  module type S = S.PATH
end

module Make_ext
    (CA : S.CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW : S.ATOMIC_WRITE_STORE_MAKER)
    (M : S.METADATA)
    (C : Contents.S)
    (P : Path.S)
    (B : Branch.S)
    (H : Hash.S)
    (N : S.NODE
         with type metadata = M.t
          and type hash = H.t
          and type step = P.step)
    (CT : S.COMMIT with type hash = H.t) =
struct
  module X = struct
    module Hash = H

    module Contents = struct
      module CA = struct
        module Key = Hash
        module Val = C
        include CA (Key) (Val)
      end

      include Contents.Store (CA)
    end

    module Node = struct
      module CA = struct
        module Key = Hash
        module Val = N
        include CA (Key) (Val)
      end

      include Node.Store (Contents) (P) (M) (CA)
    end

    module Commit = struct
      module CA = struct
        module Key = Hash
        module Val = CT
        include CA (Key) (Val)
      end

      include Commit.Store (Node) (CA)
    end

    module Branch = struct
      module Key = B
      module Val = H
      include AW (Key) (Val)
    end

    module Slice = Slice.Make (Contents) (Node) (Commit)
    module Sync = Sync.None (H) (B)

    module Repo = struct
      type t = {
        config : Conf.t;
        contents : [ `Read ] Contents.t;
        nodes : [ `Read ] Node.t;
        commits : [ `Read ] Commit.t;
        branch : Branch.t
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
        Contents.CA.v config >>= fun contents ->
        Node.CA.v config >>= fun nodes ->
        Commit.CA.v config >>= fun commits ->
        let nodes = (contents, nodes) in
        let commits = (nodes, commits) in
        Branch.v config >|= fun branch ->
        { contents; nodes; commits; branch; config }
    end
  end

  include Store.Make (X)
end

module Make
    (CA : S.CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW : S.ATOMIC_WRITE_STORE_MAKER)
    (M : S.METADATA)
    (C : S.CONTENTS)
    (P : S.PATH)
    (B : S.BRANCH)
    (H : S.HASH) =
struct
  module N = Node.Make (H) (P) (M)
  module CT = Commit.Make (H)
  include Make_ext (CA) (AW) (M) (C) (P) (B) (H) (N) (CT)
end

module Of_private = Store.Make

module type CONTENT_ADDRESSABLE_STORE = S.CONTENT_ADDRESSABLE_STORE

module type APPEND_ONLY_STORE = S.APPEND_ONLY_STORE

module type ATOMIC_WRITE_STORE = S.ATOMIC_WRITE_STORE

module type TREE = S.TREE

module type S = S.STORE

type config = Conf.t

type 'a diff = 'a Diff.t

module type CONTENT_ADDRESSABLE_STORE_MAKER = S.CONTENT_ADDRESSABLE_STORE_MAKER

module type APPEND_ONLY_STORE_MAKER = S.APPEND_ONLY_STORE_MAKER

module type ATOMIC_WRITE_STORE_MAKER = S.ATOMIC_WRITE_STORE_MAKER

module type S_MAKER = S.MAKER

module type KV =
  S with type key = string list and type step = string and type branch = string

module type KV_MAKER = functor (C : Contents.S) -> KV with type contents = C.t

module Private = struct
  module Conf = Conf

  module Node = struct
    include Node

    module type S = S.NODE

    module type GRAPH = S.NODE_GRAPH

    module type STORE = S.NODE_STORE
  end

  module Commit = struct
    include Commit

    module type S = S.COMMIT

    module type STORE = S.COMMIT_STORE

    module type HISTORY = S.COMMIT_HISTORY
  end

  module Slice = struct
    include Slice

    module type S = S.SLICE
  end

  module Sync = struct
    include Sync

    module type S = S.SYNC
  end

  module type S = S.PRIVATE

  module Watch = Watch
  module Lock = Lock
end

let version = Version.current

module type SYNC = S.SYNC_STORE

module Sync = Sync_ext.Make

type remote = S.remote = ..

let remote_store (type t) (module M : S with type t = t) (t : t) =
  let module X : S.STORE with type t = t = M in
  Sync_ext.remote_store (module X) t

module Metadata = struct
  module type S = S.METADATA

  module None = Node.No_metadata
end

module Json_tree = Contents.Json_tree
