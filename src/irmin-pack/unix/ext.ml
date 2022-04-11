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

module Maker (Config : Conf.S) = struct
  type endpoint = unit

  include Pack_key.Store_spec

  module Make (Schema : Irmin.Schema.Extended) = struct
    open struct
      module P = Schema.Path
      module M = Schema.Metadata
      module C = Schema.Contents
      module B = Schema.Branch
    end

    module H = Schema.Hash
    module Index = Pack_index.Make (H)
    module Pack = Pack_store.Maker (Index) (H)
    module Dict = Pack_dict
    module XKey = Pack_key.Make (H)

    module X = struct
      module Hash = H

      type 'a value = { hash : H.t; kind : Pack_value.Kind.t; v : 'a }
      [@@deriving irmin]

      module Contents = struct
        module Pack_value = Pack_value.Of_contents (Config) (H) (XKey) (C)

        module CA = struct
          include Pack.Make (Pack_value)

          type index = Index.t
        end

        include Irmin.Contents.Store_indexable (CA) (H) (C)
      end

      module Node = struct
        module Value = Schema.Node (XKey) (XKey)

        module CA = struct
          module Inter =
            Irmin_pack.Inode.Make_internal (Config) (H) (XKey) (Value)

          include Inode.Make_persistent (H) (Value) (Inter) (Pack)

          type index = Index.t
        end

        include
          Irmin.Node.Generic_key.Store (Contents) (CA) (H) (CA.Val) (M) (P)
      end

      module Node_portable = Node.CA.Val.Portable

      module Schema = struct
        include Schema
        module Node = Node
      end

      module Commit = struct
        module Value = struct
          include Schema.Commit (Node.Key) (XKey)
          module Info = Schema.Info

          type hash = Hash.t [@@deriving irmin]
        end

        module Pack_value = Pack_value.Of_commit (H) (XKey) (Value)
        module CA = Pack.Make (Pack_value)

        include
          Irmin.Commit.Generic_key.Store (Schema.Info) (Node) (CA) (H) (Value)
      end

      module Commit_portable = struct
        module Hash_key = Irmin.Key.Of_hash (Hash)
        include Schema.Commit (Hash_key) (Hash_key)

        let of_commit : Commit.Value.t -> t =
         fun t ->
          let info = Commit.Value.info t
          and node = Commit.Value.node t |> XKey.to_hash
          and parents = Commit.Value.parents t |> List.map XKey.to_hash in
          v ~info ~node ~parents

        module Info = Schema.Info

        type hash = Hash.t [@@deriving irmin]
      end

      module Branch = struct
        module Key = B
        module Val = XKey
        module AW = Atomic_write.Make_persistent (Key) (Val)
        include Atomic_write.Closeable (AW)

        let v ?fresh ?readonly path =
          AW.v ?fresh ?readonly path >|= make_closeable
      end

      module Slice = Irmin.Backend.Slice.Make (Contents) (Node) (Commit)
      module Remote = Irmin.Backend.Remote.None (Commit.Key) (B)

      module Repo = struct
        type t = {
          config : Irmin.Backend.Conf.t;
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
          let root = Conf.root config
          and fresh = Conf.fresh config
          and lru_size = Conf.lru_size config
          and readonly = Conf.readonly config
          and log_size = Conf.index_log_size config
          and throttle = Conf.merge_throttle config
          and indexing_strategy = Conf.indexing_strategy config in
          let f = ref (fun () -> ()) in
          let index =
            Index.v
              ~flush_callback:(fun () -> !f ())
                (* backpatching to add pack flush before an index flush *)
              ~fresh ~readonly ~throttle ~log_size root
          in
          let* contents =
            Contents.CA.v ~fresh ~readonly ~lru_size ~index ~indexing_strategy
              root
          in
          let* node =
            Node.CA.v ~fresh ~readonly ~lru_size ~index ~indexing_strategy root
          in
          let* commit =
            Commit.CA.v ~fresh ~readonly ~lru_size ~index ~indexing_strategy
              root
          in
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
                when expected = Version.latest ->
                  [%log.err
                    "[%s] Attempted to open store of unsupported version %a"
                      (Conf.root config) Version.pp found];
                  Lwt.fail e
              | e -> Lwt.fail e)

        (** Stores share instances in memory, one sync is enough. *)
        let sync t = Contents.CA.sync (contents_t t)

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

    include Irmin.Of_backend (X)

    let integrity_check_inodes ?heads t =
      [%log.debug "Check integrity for inodes"];
      let counter, (_, progress_nodes, progress_commits) =
        Utils.Object_counter.start ()
      in
      let errors = ref [] in
      let nodes = X.Repo.node_t t |> snd in
      let pred_node repo key =
        Lwt.catch
          (fun () -> Repo.default_pred_node repo key)
          (fun _ ->
            errors := "Error in repo iter" :: !errors;
            Lwt.return [])
      in

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
      let hashes = List.map (fun x -> `Commit (Commit.key x)) heads in
      let+ () =
        Repo.iter ~cache_size:1_000_000 ~min:[] ~max:hashes ~pred_node ~node
          ~commit t
      in
      Utils.Object_counter.finalise counter;
      let pp_commits = Fmt.list ~sep:Fmt.comma Commit.pp_hash in
      if !errors = [] then
        Fmt.kstr (fun x -> Ok (`Msg x)) "Ok for heads %a" pp_commits heads
      else
        Fmt.kstr
          (fun x -> Error (`Msg x))
          "Inconsistent inodes found for heads %a: %a" pp_commits heads
          Fmt.(list ~sep:comma string)
          !errors

    module Stats = struct
      let pp_key = Irmin.Type.pp XKey.t

      let traverse_inodes ~dump_blob_paths_to commit repo =
        let module Stats = Checks.Stats (struct
          type nonrec step = step

          let step_t = step_t

          module Hash = Hash
        end) in
        let t = Stats.v () in
        let pred_node repo k =
          X.Node.find (X.Repo.node_t repo) k >|= function
          | None -> Fmt.failwith "key %a not found" pp_key k
          | Some v ->
              let width = X.Node.Val.length v in
              let nb_children = X.Node.CA.Val.nb_children v in
              let preds = X.Node.CA.Val.pred v in
              let () =
                preds
                |> List.map (function
                     | s, `Contents h -> (s, `Contents (XKey.to_hash h))
                     | s, `Inode h -> (s, `Inode (XKey.to_hash h))
                     | s, `Node h -> (s, `Node (XKey.to_hash h)))
                |> Stats.visit_node t (XKey.to_hash k) ~width ~nb_children
              in
              List.rev_map
                (function
                  | s, `Inode x ->
                      assert (s = None);
                      `Node x
                  | _, `Node x -> `Node x
                  | _, `Contents x -> `Contents x)
                preds
        in
        (* We are traversing only one commit. *)
        let pred_commit repo k =
          X.Commit.find (X.Repo.commit_t repo) k >|= function
          | None -> []
          | Some c ->
              let node = X.Commit.Val.node c in
              Stats.visit_commit t (XKey.to_hash node);
              [ `Node node ]
        in
        let pred_contents _repo k =
          Stats.visit_contents t (XKey.to_hash k);
          Lwt.return []
        in
        (* We want to discover all paths to a node, so we don't cache nodes
           during traversal. *)
        let* () =
          Repo.breadth_first_traversal ~cache_size:0 ~pred_node ~pred_commit
            ~pred_contents ~max:[ commit ] repo
        in
        Stats.pp_results ~dump_blob_paths_to t;
        Lwt.return_unit

      let run ~dump_blob_paths_to ~commit repo =
        Printexc.record_backtrace true;
        let key = `Commit (Commit.key commit) in
        traverse_inodes ~dump_blob_paths_to key repo
    end

    let stats = Stats.run
    let sync = X.Repo.sync
    let flush = X.Repo.flush

    module Traverse_pack_file = Traverse_pack_file.Make (struct
      module Hash = H
      module Index = Index
      module Inode = X.Node.CA
      module Dict = Dict
      module Contents = X.Contents.Pack_value
      module Commit = X.Commit.Pack_value
    end)

    let traverse_pack_file = Traverse_pack_file.run

    module Snapshot = struct
      include X.Node.CA.Snapshot

      type t = Inode of inode | Blob of Backend.Contents.Val.t
      [@@deriving irmin]

      module S = Snapshot.Make (struct
        module Hash = H
        module Inode = X.Node.CA
        module Contents_pack = X.Contents.CA
      end)

      include S

      module Export = struct
        let iter ?on_disk repo f ~root_key =
          [%log.debug "Iterate over a tree"];
          let contents = X.Repo.contents_t repo in
          let nodes = X.Repo.node_t repo |> snd in
          let root = Conf.root repo.config in
          let log_size = Conf.index_log_size repo.config in
          let export = S.Export.v root log_size contents nodes in
          let f_contents x = f (Blob x) in
          let f_nodes x = f (Inode x) in
          match root_key with
          | `Contents _ -> Fmt.failwith "[root_key] cannot be of type contents"
          | `Node key ->
              let* total =
                Export.run ?on_disk export f_contents f_nodes
                  (key, Pack_value.Kind.Inode_v2_root)
              in
              Export.close export;
              Lwt.return total
      end

      let export = Export.iter

      module Import = struct
        type process = Import.t

        let v ?on_disk repo =
          let contents = X.Repo.contents_t repo in
          let nodes = X.Repo.node_t repo |> snd in
          let log_size = Conf.index_log_size repo.config in
          Import.v ?on_disk log_size contents nodes

        let save_elt process elt =
          match elt with
          | Blob x -> Import.save_contents process x
          | Inode x -> Import.save_inodes process x

        let close process = Import.close process
      end
    end

    (* following for layers *)
    include struct

      let get_pack_store_io' : repo -> Pack_store_IO.t = fun repo -> 
        let contents : read X.Contents.t = repo.contents in
        let contents : read X.Contents.CA.t = contents in
        let io : Pack_store_IO.t = X.Contents.CA.get_pack_store_io contents in
        io

      (* let get_pack_store_io: (repo -> Pack_store_IO.t) option = Some get_pack_store_io' *)

      let get_config (repo:repo) : Irmin.Backend.Conf.t = repo.config

      (* Why does the following piece of implementation belong here?
         Pack_store_IO.trigger_gc knows nothing about [type repo] etc; so in order to
         implement a function [: repo -> string -> unit] we need to be outside
         Pack_store_IO; at least in this file, we have access to all the implementation
         parts that we could need. *)
      let trigger_gc (repo:repo) commit_hash_s =
        let io = get_pack_store_io' repo in
        let args = Pack_store_IO.Trigger_gc.{
            commit_hash_s;
            create_reachable=(fun ~reachable_fn -> 
                (* FIXME following needs to be replaced with something better *)
                let exe = "/tmp/create_reach.exe" in
                (* use config to get the path to the context *)
                let path_to_ctxt = 
                  let config = get_config repo in
                  (* NOTE Conf is Irmin_pack.Conf *)
                  Conf.root config 
                in
                let cmd = String.concat " " [exe; path_to_ctxt; commit_hash_s; reachable_fn] in
                (* FIXME prev needs quoting? *)
                let _ = Printf.printf "About to run command %S\n%!" cmd in    
                let ret = Sys.command cmd in
                let _ = assert(ret = 0) in
                ())}
        in
        Pack_store_IO.trigger_gc io args;
        Pack_store.clear_all_caches ();
        ()

    end

  end
end
