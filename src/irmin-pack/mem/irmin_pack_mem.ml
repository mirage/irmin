(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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
  let clear_keep_generation _ = Lwt.return_unit
end

module CA_mem
    (Hash : Irmin.Hash.S)
    (Value : Irmin_pack.Pack_value.S with type hash := Hash.t) =
struct
  module Pack = Content_addressable.Maker (Hash)
  module CA_mem = Pack.Make (Value)
  include Irmin_pack.Content_addressable.Closeable (CA_mem)

  let v x = CA_mem.v x >|= make_closeable
end

module Maker (Config : Irmin_pack.Conf.S) = struct
  type endpoint = unit

  module Make (Schema : Irmin.Schema.S) = struct
    module H = Schema.Hash
    module C = Schema.Contents
    module P = Schema.Path
    module M = Schema.Metadata
    module B = Schema.Branch
    module Pack = Content_addressable.Maker (H)

    module X = struct
      module Schema = Schema
      module Hash = H
      module Info = Schema.Info

      module Contents = struct
        module Pack_value = Irmin_pack.Pack_value.Of_contents (H) (C)
        module CA = CA_mem (H) (Pack_value)
        include Irmin.Contents.Store (CA) (H) (C)
      end

      module Node = struct
        module CA = struct
          module Inter =
            Irmin_pack.Inode.Make_internal (Config) (H) (Schema.Node)

          module CA = Pack.Make (Inter.Raw)
          include Irmin_pack.Inode.Make (H) (Schema.Node) (Inter) (CA)

          let v = CA.v
        end

        include Irmin.Node.Store (Contents) (CA) (H) (CA.Val) (M) (P)
      end

      module Commit = struct
        module Pack_value =
          Irmin_pack.Pack_value.Of_commit
            (H)
            (struct
              module Info = Schema.Info
              include Schema.Commit
            end)

        module CA = CA_mem (H) (Pack_value)
        include Irmin.Commit.Store (Info) (Node) (CA) (H) (Schema.Commit)
      end

      module Branch = struct
        module Key = B
        module Val = H
        module AW = Atomic_write (Key) (Val)
        include Irmin_pack.Atomic_write.Closeable (AW)

        let v () = AW.v () >|= make_closeable
      end

      module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
      module Remote = Irmin.Private.Remote.None (H) (B)

      module Repo = struct
        type t = {
          config : Irmin.Private.Conf.t;
          contents : read Contents.CA.t;
          node : read Node.CA.t;
          commit : read Commit.CA.t;
          branch : Branch.t;
        }

        let contents_t t : 'a Contents.t = t.contents
        let node_t t : 'a Node.t = (contents_t t, t.node)
        let commit_t t : 'a Commit.t = (node_t t, t.commit)
        let branch_t t = t.branch

        let batch t f =
          Commit.CA.batch t.commit (fun commit ->
              Node.CA.batch t.node (fun node ->
                  Contents.CA.batch t.contents (fun contents ->
                      let contents : 'a Contents.t = contents in
                      let node : 'a Node.t = (contents, node) in
                      let commit : 'a Commit.t = (node, commit) in
                      f contents node commit)))

        let v config =
          let root = Irmin_pack.Conf.(get config root_key) in
          let* contents = Contents.CA.v root in
          let* node = Node.CA.v root in
          let* commit = Commit.CA.v root in
          let+ branch = Branch.v () in
          { contents; node; commit; branch; config }

        let close t =
          Contents.CA.close (contents_t t) >>= fun () ->
          Node.CA.close (snd (node_t t)) >>= fun () ->
          Commit.CA.close (snd (commit_t t)) >>= fun () -> Branch.close t.branch

        (* An in-memory store is always in sync. *)
        let sync _ = ()
        let flush _ = ()

        (* Stores share instances so one clear is enough. *)
        let clear t = Contents.CA.clear (contents_t t)
      end
    end

    include Irmin.Of_private (X)

    let integrity_check_inodes ?heads:_ _ =
      Lwt.return
        (Error (`Msg "Not supported: integrity checking of in-memory inodes"))

    let sync = X.Repo.sync
    let clear = X.Repo.clear
    let migrate = Irmin_pack.migrate
    let flush = X.Repo.flush
    let integrity_check ?ppf:_ ~auto_repair:_ _t = Ok `No_error
    let traverse_pack_file _ _ = ()
  end
end
