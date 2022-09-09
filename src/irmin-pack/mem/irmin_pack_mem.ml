(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

module Atomic_write (K : Irmin.Type.S) (V : Irmin.Hash.S) = struct
  module AW = Irmin_mem.Atomic_write (K) (V)
  include AW

  let v () = AW.v (Irmin_mem.config ())
  let flush _t = ()
end

module Indexable_mem
    (Hash : Irmin.Hash.S)
    (Value : Irmin_pack.Pack_value.S
               with type hash := Hash.t
                and type key = Hash.t) =
struct
  module Pack = Indexable.Maker (Hash)
  module Indexable_mem = Pack.Make (Value)
  include Irmin_pack.Indexable.Closeable (Indexable_mem)

  let v x = Indexable_mem.v x >|= make_closeable
end

module Maker (Config : Irmin_pack.Conf.S) = struct
  type endpoint = unit

  include Irmin.Key.Store_spec.Hash_keyed

  module Make (Schema : Irmin.Schema.Extended) = struct
    module H = Schema.Hash
    module C = Schema.Contents
    module P = Schema.Path
    module M = Schema.Metadata
    module B = Schema.Branch
    module Pack = Indexable.Maker (H)

    module XKey = struct
      include Irmin.Key.Of_hash (H)

      let unfindable_of_hash x = x
    end

    module X = struct
      module Schema = Schema
      module Hash = H
      module Info = Schema.Info

      module Contents = struct
        module Pack_value =
          Irmin_pack.Pack_value.Of_contents (Config) (H) (XKey) (C)

        module Indexable = Indexable_mem (H) (Pack_value)
        include Irmin.Contents.Store_indexable (Indexable) (H) (C)
      end

      module Node = struct
        module Value = Schema.Node (XKey) (XKey)

        module Indexable = struct
          module Inter =
            Irmin_pack.Inode.Make_internal (Config) (H) (XKey) (Value)

          module CA = Pack.Make (Inter.Raw)
          include Irmin_pack.Inode.Make (H) (XKey) (Value) (Inter) (CA)

          let v = CA.v
        end

        include
          Irmin.Node.Generic_key.Store (Contents) (Indexable) (H)
            (Indexable.Val)
            (M)
            (P)
      end

      module Node_portable = Node.Indexable.Val.Portable

      module Commit = struct
        module Value = struct
          include Schema.Commit (Node.Key) (XKey)
          module Info = Schema.Info

          type hash = Hash.t [@@deriving irmin]
        end

        module Pack_value = Irmin_pack.Pack_value.Of_commit (H) (XKey) (Value)
        module Indexable = Indexable_mem (H) (Pack_value)

        include
          Irmin.Commit.Generic_key.Store (Info) (Node) (Indexable) (H) (Value)
      end

      module Commit_portable = Irmin.Commit.Portable.Of_commit (Commit.Value)

      module Branch = struct
        module Key = B

        module Val = struct
          include H
          include Commit.Key
        end

        module AW = Atomic_write (Key) (Val)
        include Irmin_pack.Atomic_write.Closeable (AW)

        let v () = AW.v () >|= make_closeable
      end

      module Slice = Irmin.Backend.Slice.Make (Contents) (Node) (Commit)
      module Remote = Irmin.Backend.Remote.None (H) (B)

      module Repo = struct
        type t = {
          config : Irmin.Backend.Conf.t;
          contents : read Contents.Indexable.t;
          node : read Node.Indexable.t;
          commit : read Commit.Indexable.t;
          branch : Branch.t;
        }

        let contents_t t : 'a Contents.t = t.contents
        let node_t t : 'a Node.t = (contents_t t, t.node)
        let commit_t t : 'a Commit.t = (node_t t, t.commit)
        let branch_t t = t.branch
        let config t = t.config

        let batch t f =
          Commit.Indexable.batch t.commit (fun commit ->
              Node.Indexable.batch t.node (fun node ->
                  Contents.Indexable.batch t.contents (fun contents ->
                      let contents : 'a Contents.t = contents in
                      let node : 'a Node.t = (contents, node) in
                      let commit : 'a Commit.t = (node, commit) in
                      f contents node commit)))

        let v config =
          let root = Irmin_pack.Conf.root config in
          let* contents = Contents.Indexable.v root in
          let* node = Node.Indexable.v root in
          let* commit = Commit.Indexable.v root in
          let+ branch = Branch.v () in
          { contents; node; commit; branch; config }

        let close t =
          Contents.Indexable.close (contents_t t) >>= fun () ->
          Node.Indexable.close (snd (node_t t)) >>= fun () ->
          Commit.Indexable.close (snd (commit_t t)) >>= fun () ->
          Branch.close t.branch
      end
    end

    include Irmin.Of_backend (X)
  end
end
