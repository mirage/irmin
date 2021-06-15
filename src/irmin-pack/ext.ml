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
module IO = IO.Unix

module Maker
    (V : Version.S)
    (Config : Conf.S)
    (Node : Irmin.Private.Node.Maker)
    (Commit : Irmin.Private.Commit.Maker) =
struct
  type endpoint = unit
  type info = Commit.Info.t

  module Make
      (M : Irmin.Metadata.S)
      (C : Irmin.Contents.S)
      (P : Irmin.Path.S)
      (B : Irmin.Branch.S)
      (H : Irmin.Hash.S) =
  struct
    module Index = Pack_index.Make (H)
    module Pack = Pack_store.Maker (V) (Index) (H)
    module Dict = Pack_dict.Make (V)

    module X = struct
      module Hash = H
      module Info = Commit.Info

      type 'a value = { hash : H.t; kind : Pack_value.Kind.t; v : 'a }
      [@@deriving irmin]

      module Contents = struct
        module Pack_value = Pack_value.Of_contents (H) (C)
        module CA = Pack.Make (Pack_value)
        include Irmin.Contents.Store (CA) (H) (C)
      end

      module Node = struct
        module Node = Node (H) (P) (M)

        module CA = struct
          module Inter = Inode.Make_internal (Config) (H) (Node)
          include Inode.Make_persistent (H) (Node) (Inter) (Pack)
        end

        include Irmin.Private.Node.Store (Contents) (CA) (H) (CA.Val) (M) (P)
      end

      module Commit = struct
        module Commit = Commit.Make (H)
        module Pack_value = Pack_value.Of_commit (H) (Commit)
        module CA = Pack.Make (Pack_value)
        include Irmin.Private.Commit.Store (Info) (Node) (CA) (H) (Commit)
      end

      module Branch = struct
        module Key = B
        module Val = H
        module AW = Atomic_write.Make_persistent (V) (Key) (Val)
        include Atomic_write.Closeable (AW)

        let v ?fresh ?readonly path =
          AW.v ?fresh ?readonly path >|= make_closeable
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
          index : Index.t;
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

        let unsafe_v config =
          let root = Conf.root config in
          let fresh = Conf.fresh config in
          let lru_size = Conf.lru_size config in
          let readonly = Conf.readonly config in
          let log_size = Conf.index_log_size config in
          let throttle = Conf.merge_throttle config in
          let f = ref (fun () -> ()) in
          let index =
            Index.v
              ~flush_callback:(fun () -> !f ())
                (* backpatching to add pack flush before an index flush *)
              ~fresh ~readonly ~throttle ~log_size root
          in
          let* contents =
            Contents.CA.v ~fresh ~readonly ~lru_size ~index root
          in
          let* node = Node.CA.v ~fresh ~readonly ~lru_size ~index root in
          let* commit = Commit.CA.v ~fresh ~readonly ~lru_size ~index root in
          let+ branch = Branch.v ~fresh ~readonly root in
          (* Stores share instances in memory, one flush is enough. In case of a
             system crash, the flush_callback might not make with the disk. In
             this case, when the store is reopened, [integrity_check] needs to be
             called to repair the store. *)
          (f := fun () -> Contents.CA.flush ~index:false contents);
          { contents; node; commit; branch; config; index }

        let close t =
          Index.close t.index;
          Contents.CA.close (contents_t t) >>= fun () ->
          Node.CA.close (snd (node_t t)) >>= fun () ->
          Commit.CA.close (snd (commit_t t)) >>= fun () -> Branch.close t.branch

        let v config =
          Lwt.catch
            (fun () -> unsafe_v config)
            (function
              | Version.Invalid { expected; found } as e
                when expected = V.version ->
                  Log.err (fun m ->
                      m "[%s] Attempted to open store of unsupported version %a"
                        (Conf.root config) Version.pp found);
                  Lwt.fail e
              | e -> Lwt.fail e)

        (** Stores share instances in memory, one sync is enough. However each
            store has its own lru and all have to be cleared. *)
        let sync t =
          let on_generation_change () =
            Node.CA.clear_caches (snd (node_t t));
            Commit.CA.clear_caches (snd (commit_t t))
          in
          Contents.CA.sync ~on_generation_change (contents_t t)

        (** Stores share instances so one clear is enough. *)
        let clear t = Contents.CA.clear (contents_t t)

        let flush t =
          Contents.CA.flush (contents_t t);
          Branch.flush t.branch
      end
    end

    let integrity_check ?ppf ~auto_repair t =
      let module Checks = Checks.Index (Index) in
      let contents = X.Repo.contents_t t in
      let nodes = X.Repo.node_t t |> snd in
      let commits = X.Repo.commit_t t |> snd in
      let check ~kind ~offset ~length k =
        match kind with
        | `Contents -> X.Contents.CA.integrity_check ~offset ~length k contents
        | `Node -> X.Node.CA.integrity_check ~offset ~length k nodes
        | `Commit -> X.Commit.CA.integrity_check ~offset ~length k commits
      in
      Checks.integrity_check ?ppf ~auto_repair ~check t.index

    include Irmin.Of_private (X)

    let integrity_check_inodes ?heads t =
      Log.debug (fun l -> l "Check integrity for inodes");
      let bar, (_, progress_nodes, progress_commits) =
        Utils.Progress.increment ()
      in
      let errors = ref [] in
      let nodes = X.Repo.node_t t |> snd in
      let node k =
        progress_nodes ();
        X.Node.CA.integrity_check_inodes nodes k >|= function
        | Ok () -> ()
        | Error msg -> errors := msg :: !errors
      in
      let commit _ =
        progress_commits ();
        Lwt.return_unit
      in
      let* heads =
        match heads with None -> Repo.heads t | Some m -> Lwt.return m
      in
      let hashes = List.map (fun x -> `Commit (Commit.hash x)) heads in
      let+ () =
        Repo.iter ~cache_size:1_000_000 ~min:[] ~max:hashes ~node ~commit t
      in
      Utils.Progress.finalise bar;
      let pp_commits = Fmt.list ~sep:Fmt.comma Commit.pp_hash in
      if !errors = [] then
        Fmt.kstrf (fun x -> Ok (`Msg x)) "Ok for heads %a" pp_commits heads
      else
        Fmt.kstrf
          (fun x -> Error (`Msg x))
          "Inconsistent inodes found for heads %a: %a" pp_commits heads
          Fmt.(list ~sep:comma string)
          !errors

    let sync = X.Repo.sync
    let clear = X.Repo.clear
    let migrate = Migrate.run
    let flush = X.Repo.flush

    module Reconstruct_index = Reconstruct_index.Make (struct
      module Version = V
      module Hash = H
      module Index = Index
      module Inode = X.Node.CA
      module Dict = Dict
      module Contents = X.Contents.Pack_value
      module Commit = X.Commit.Pack_value
    end)

    let reconstruct_index = Reconstruct_index.run
  end
end
