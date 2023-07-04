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
include Store_intf

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
    module Io = Io.Unix
    module Index = Pack_index.Make (H)
    module Errs = Io_errors.Make (Io)
    module File_manager = File_manager.Make (Io) (Index) (Errs)
    module Dict = Dict.Make (File_manager)
    module Dispatcher = Dispatcher.Make (File_manager)
    module XKey = Pack_key.Make (H)

    module X = struct
      module Hash = H

      type 'a value = { hash : H.t; kind : Pack_value.Kind.t; v : 'a }
      [@@deriving irmin]

      module Contents = struct
        module Pack_value = Pack_value.Of_contents (Config) (H) (XKey) (C)

        module CA =
          Pack_store.Make (File_manager) (Dict) (Dispatcher) (H) (Pack_value)
            (Errs)

        include Irmin.Contents.Store_indexable (CA) (H) (C)
      end

      module Node = struct
        module Value = Schema.Node (XKey) (XKey)

        module CA = struct
          module Inter =
            Irmin_pack.Inode.Make_internal (Config) (H) (XKey) (Value)

          module Pack' =
            Pack_store.Make (File_manager) (Dict) (Dispatcher) (H) (Inter.Raw)
              (Errs)

          include Inode.Make_persistent (H) (Value) (Inter) (Pack')
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

        module CA =
          Pack_store.Make (File_manager) (Dict) (Dispatcher) (H) (Pack_value)
            (Errs)

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

      module Gc = Gc.Make (struct
        module Async = Async.Unix
        module Fm = File_manager
        module Errs = Errs
        module Dict = Dict
        module Dispatcher = Dispatcher
        module Hash = Schema.Hash
        module Contents_store = Contents.CA
        module Node_value = Node.CA.Inter.Val
        module Node_store = Node.CA
        module Commit_value = Commit.Value
        module Commit_store = Commit.CA

        type hash = Node_value.hash
        type key = Node_value.node_key [@@deriving irmin]
      end)

      module Repo = struct
        type running_gc = { gc : Gc.t; use_auto_finalisation : bool }

        type t = {
          lru : Lru.t;
          config : Irmin.Backend.Conf.t;
          contents : read Contents.CA.t;
          node : read Node.CA.t;
          commit : read Commit.CA.t;
          branch : Branch.t;
          fm : File_manager.t;
          dict : Dict.t;
          dispatcher : Dispatcher.t;
          mutable during_batch : bool;
          mutable running_gc : running_gc option;
        }

        let pp_key = Irmin.Type.pp XKey.t
        let contents_t t : 'a Contents.t = t.contents
        let node_t t : 'a Node.t = (contents_t t, t.node)
        let commit_t t : 'a Commit.t = (node_t t, t.commit)
        let branch_t t = t.branch
        let config t = t.config

        let v config =
          let root = Irmin_pack.Conf.root config in
          let fresh = Irmin_pack.Conf.fresh config in
          let fm =
            let readonly = Irmin_pack.Conf.readonly config in
            if readonly then File_manager.open_ro config |> Errs.raise_if_error
            else
              match (Io.classify_path root, fresh) with
              | `No_such_file_or_directory, _ ->
                  File_manager.create_rw ~overwrite:false config
                  |> Errs.raise_if_error
              | `Directory, true ->
                  File_manager.create_rw ~overwrite:true config
                  |> Errs.raise_if_error
              | `Directory, false ->
                  File_manager.open_rw config |> Errs.raise_if_error
              | (`File | `Other), _ -> Errs.raise_error (`Not_a_directory root)
          in
          let dict = Dict.v fm |> Errs.raise_if_error in
          let dispatcher = Dispatcher.v fm |> Errs.raise_if_error in
          let lru = Lru.create config in
          let contents = Contents.CA.v ~config ~fm ~dict ~dispatcher ~lru in
          let node = Node.CA.v ~config ~fm ~dict ~dispatcher ~lru in
          let commit = Commit.CA.v ~config ~fm ~dict ~dispatcher ~lru in
          let+ branch =
            let root = Conf.root config in
            let fresh = Conf.fresh config in
            let readonly = Conf.readonly config in
            let path = Irmin_pack.Layout.V4.branch ~root in
            Branch.v ~fresh ~readonly path
          in
          let during_batch = false in
          let running_gc = None in
          {
            config;
            contents;
            node;
            commit;
            branch;
            fm;
            dict;
            during_batch;
            running_gc;
            dispatcher;
            lru;
          }

        let flush t = File_manager.flush ?hook:None t.fm |> Errs.raise_if_error
        let fsync t = File_manager.fsync t.fm |> Errs.raise_if_error
        let reload t = File_manager.reload t.fm |> Errs.raise_if_error

        module Gc = struct
          let is_allowed { fm; _ } = File_manager.gc_allowed fm
          let behaviour { fm; _ } = File_manager.gc_behaviour fm

          let cancel t =
            match t.running_gc with
            | Some { gc; _ } ->
                let cancelled = Gc.cancel gc in
                t.running_gc <- None;
                cancelled
            | None -> false

          let direct_commit_key t key =
            let state : _ Pack_key.state = Pack_key.inspect key in
            match state with
            | Direct _ -> Ok key
            | Indexed h -> (
                match Commit.CA.index_direct_with_kind t.commit h with
                | None ->
                    Error
                      (`Commit_key_is_dangling
                        (Irmin.Type.to_string XKey.t key))
                | Some (k, _kind) -> Ok k)

          let start ~unlink ~use_auto_finalisation ~output t commit_key =
            let open Result_syntax in
            [%log.info "GC: Starting on %a" pp_key commit_key];
            let* () =
              if t.during_batch then Error `Gc_forbidden_during_batch else Ok ()
            in
            let* commit_key = direct_commit_key t commit_key in
            let root = Conf.root t.config in
            let* () =
              if not (is_allowed t) then
                Error (`Gc_disallowed "Store does not support GC")
              else Ok ()
            in
            let current_generation = File_manager.generation t.fm in
            let next_generation = current_generation + 1 in
            let lower_root = Conf.lower_root t.config in
            let* gc =
              Gc.v ~root ~lower_root ~generation:next_generation ~unlink
                ~dispatcher:t.dispatcher ~fm:t.fm ~contents:t.contents
                ~node:t.node ~commit:t.commit ~output commit_key
            in
            t.running_gc <- Some { gc; use_auto_finalisation };
            Ok ()

          let start_exn ?(unlink = true) ?(output = `Root)
              ~use_auto_finalisation t commit_key =
            match t.running_gc with
            | Some _ ->
                [%log.info "Repo is alreadying running GC. Skipping."];
                Lwt.return false
            | None -> (
                let result =
                  start ~unlink ~use_auto_finalisation ~output t commit_key
                in
                match result with
                | Ok _ -> Lwt.return true
                | Error e -> Errs.raise_error e)

          let finalise_exn ?(wait = false) t =
            let* result =
              match t.running_gc with
              | None -> Lwt.return_ok `Idle
              | Some { gc; _ } ->
                  if t.during_batch then
                    Lwt.return_error `Gc_forbidden_during_batch
                  else Gc.finalise ~wait gc
            in
            match result with
            | Ok (`Finalised _ as x) ->
                t.running_gc <- None;
                Lwt.return x
            | Ok waited -> Lwt.return waited
            | Error e ->
                t.running_gc <- None;
                Errs.raise_error e

          let is_finished t = Option.is_none t.running_gc

          let on_finalise t f =
            match t.running_gc with
            | None -> ()
            | Some { gc; _ } -> Gc.on_finalise gc f

          let try_auto_finalise_exn t =
            match t.running_gc with
            | None | Some { use_auto_finalisation = false; _ } ->
                Lwt.return_unit
            | Some { use_auto_finalisation = true; _ } ->
                let* _ = finalise_exn ~wait:false t in
                Lwt.return_unit

          let latest_gc_target t =
            let pl = File_manager.(Control.payload (control t.fm)) in
            match pl.status with
            | From_v1_v2_post_upgrade _ | Used_non_minimal_indexing_strategy
            | No_gc_yet ->
                None
            | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13
            | T14 | T15 ->
                assert false
            | Gced { latest_gc_target_offset = offset; _ }
              when offset = Int63.zero ->
                None
            | Gced { latest_gc_target_offset = offset; _ } -> (
                let entry =
                  Commit.CA.read_and_decode_entry_prefix ~off:offset
                    t.dispatcher
                in
                match Commit.CA.Entry_prefix.total_entry_length entry with
                | None ->
                    (* Commits on which this operation is supported have a
                       length in their header. *)
                    assert false
                | Some length ->
                    let key = Pack_key.v_direct ~offset ~length entry.hash in
                    Some key)

          let create_one_commit_store t commit_key path =
            let () =
              match Io.classify_path path with
              | `Directory -> ()
              | `No_such_file_or_directory ->
                  Io.mkdir path |> Errs.raise_if_error
              | _ -> Errs.raise_error `Invalid_layout
            in
            let commit_key =
              direct_commit_key t commit_key |> Errs.raise_if_error
            in
            (* The GC action here does not matter, since we'll not fully
               finalise it *)
            let* launched =
              start_exn ~use_auto_finalisation:false ~output:(`External path) t
                commit_key
            in
            let () =
              if not launched then Errs.raise_error `Forbidden_during_gc
            in
            let* gced =
              match t.running_gc with
              | None -> assert false
              | Some { gc; _ } -> Gc.finalise_without_swap gc
            in
            let config = Irmin.Backend.Conf.add t.config Conf.Key.root path in
            let () =
              File_manager.create_one_commit_store t.fm config gced commit_key
              |> Errs.raise_if_error
            in
            let branch_path = Irmin_pack.Layout.V4.branch ~root:path in
            let* branch_store =
              Branch.v ~fresh:true ~readonly:false branch_path
            in
            let* () = Branch.close branch_store in
            Lwt.return_unit
        end

        let is_split_allowed = Gc.is_allowed

        let split t =
          let open Result_syntax in
          let readonly = Irmin_pack.Conf.readonly t.config in
          let* () =
            if not (is_split_allowed t) then Error `Split_disallowed else Ok ()
          in
          let* () = if readonly then Error `Ro_not_allowed else Ok () in
          let* () =
            if t.during_batch then Error `Split_forbidden_during_batch
            else Ok ()
          in
          File_manager.split t.fm

        let split_exn repo = split repo |> Errs.raise_if_error

        let add_volume_exn t =
          let () =
            if Irmin_pack.Conf.readonly t.config then
              Errs.raise_error `Ro_not_allowed;
            if Gc.is_finished t = false then
              Errs.raise_error `Add_volume_forbidden_during_gc
          in
          File_manager.add_volume t.fm |> Errs.raise_if_error

        let batch t f =
          [%log.debug "[pack] batch start"];
          let readonly = Irmin_pack.Conf.readonly t.config in
          if readonly then Errs.raise_error `Ro_not_allowed
          else
            let c0 = Mtime_clock.counter () in
            let try_finalise () = Gc.try_auto_finalise_exn t in
            let* _ = try_finalise () in
            t.during_batch <- true;
            let contents = Contents.CA.cast t.contents in
            let node = Node.CA.Pack.cast t.node in
            let commit = Commit.CA.cast t.commit in
            let contents : 'a Contents.t = contents in
            let node : 'a Node.t = (contents, node) in
            let commit : 'a Commit.t = (node, commit) in
            let on_success res =
              let s = Mtime_clock.count c0 |> Mtime.span_to_s in
              [%log.info "[pack] batch completed in %.6fs" s];
              t.during_batch <- false;
              File_manager.flush t.fm |> Errs.raise_if_error;
              let* _ = try_finalise () in
              Lwt.return res
            in
            let on_fail exn =
              t.during_batch <- false;
              [%log.info
                "[pack] batch failed. calling flush. (%s)"
                  (Printexc.to_string exn)];
              let () =
                match File_manager.flush t.fm with
                | Ok () -> ()
                | Error err ->
                    [%log.err
                      "[pack] batch failed and flush failed. Silencing flush \
                       fail. (%a)"
                      (Irmin.Type.pp Errs.t) err]
              in
              (* Kill gc process in at_exit. *)
              raise exn
            in
            Lwt.try_bind (fun () -> f contents node commit) on_success on_fail

        let close t =
          (* Step 1 - Kill the gc process if it is running *)
          let _ = Gc.cancel t in
          (* Step 2 - Close the files *)
          let () = File_manager.close t.fm |> Errs.raise_if_error in
          Branch.close t.branch >>= fun () ->
          (* Step 3 - Close the in-memory abstractions *)
          Dict.close t.dict;
          Contents.CA.close (contents_t t) >>= fun () ->
          Node.CA.close (snd (node_t t)) >>= fun () ->
          Commit.CA.close (snd (commit_t t))
      end
    end

    include Irmin.Of_backend (X)
    module Integrity_checks = Checks.Integrity_checks (XKey) (X) (Index)

    let integrity_check_inodes ?heads t =
      let* heads =
        match heads with None -> Repo.heads t | Some m -> Lwt.return m
      in
      let hashes = List.map (fun x -> `Commit (Commit.key x)) heads in
      let iter ~pred_node ~node ~commit t =
        Repo.iter ~cache_size:1_000_000 ~min:[] ~max:hashes ~pred_node ~node
          ~commit t
      in
      let nodes = X.Repo.node_t t |> snd in
      let check = X.Node.CA.integrity_check_inodes nodes in
      let pred = Repo.default_pred_node in
      Integrity_checks.check_inodes ~iter ~pred ~check t

    let integrity_check_always ?ppf ~auto_repair t =
      let contents = X.Repo.contents_t t in
      let nodes = X.Repo.node_t t |> snd in
      let commits = X.Repo.commit_t t |> snd in
      let check ~kind ~offset ~length k =
        match kind with
        | `Contents -> X.Contents.CA.integrity_check ~offset ~length k contents
        | `Node -> X.Node.CA.integrity_check ~offset ~length k nodes
        | `Commit -> X.Commit.CA.integrity_check ~offset ~length k commits
      in
      let index = File_manager.index t.fm in
      Integrity_checks.check_always ?ppf ~auto_repair ~check index

    let integrity_check_minimal ?ppf ?heads t =
      let* heads =
        match heads with None -> Repo.heads t | Some m -> Lwt.return m
      in
      let hashes = List.map (fun x -> `Commit (Commit.key x)) heads in
      let iter ~contents ~node ~pred_node ~pred_commit repo =
        Repo.iter ~cache_size:1_000_000 ~min:[] ~max:hashes ~contents ~node
          ~pred_node ~pred_commit repo
      in
      let contents = X.Repo.contents_t t in
      let pred = X.Node.CA.Val.pred in
      let recompute_hash = X.Node.CA.Val.recompute_hash in
      let check ~offset ~length k =
        X.Contents.CA.integrity_check ~offset ~length k contents
      in
      Integrity_checks.check_minimal ?ppf ~pred ~iter ~check ~recompute_hash t

    let integrity_check ?ppf ?heads ~auto_repair t =
      let is_minimal =
        t.X.Repo.config
        |> Conf.indexing_strategy
        |> Irmin_pack.Indexing_strategy.is_minimal
      in
      let result =
        if is_minimal then integrity_check_minimal ?ppf ?heads t
        else integrity_check_always ?ppf ~auto_repair t |> Lwt.return
      in
      result

    module Stats_computation = struct
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

    let stats = Stats_computation.run
    let reload = X.Repo.reload
    let flush = X.Repo.flush
    let fsync = X.Repo.fsync
    let is_split_allowed = X.Repo.is_split_allowed
    let split = X.Repo.split_exn
    let add_volume = X.Repo.add_volume_exn
    let create_one_commit_store = X.Repo.Gc.create_one_commit_store

    module Gc = struct
      type msg = [ `Msg of string ]

      type process_state =
        [ `Idle | `Running | `Finalised of Stats.Latest_gc.stats ]

      let catch_errors context error =
        let err =
          match error with
          | Errors.Pack_error error ->
              Fmt.str "Pack_error: %a" Errors.pp_base_error error
          | Irmin.Closed -> "Irmin.Closed"
          | Irmin_pack.RO_not_allowed -> "Irmin_pack.RO_not_allowed"
          | Unix.Unix_error (err, s1, s2) ->
              let pp = Irmin.Type.pp Io.misc_error_t in
              Fmt.str "Unix_error: %a" pp (err, s1, s2)
          | exn -> raise exn
        in
        let error_msg = Fmt.str "[%s] resulted in error: %s" context err in
        Lwt.return_error (`Msg error_msg)

      let map_errors context (error : Errs.t) =
        let err_msg =
          Fmt.str "[%s] resulted in error: %a" context (Irmin.Type.pp Errs.t)
            error
        in
        `Msg err_msg

      let finalise_exn = X.Repo.Gc.finalise_exn

      let start_exn ?unlink t =
        X.Repo.Gc.start_exn ?unlink ~use_auto_finalisation:false t

      let start repo commit_key =
        try
          let* started =
            X.Repo.Gc.start_exn ~unlink:true ~use_auto_finalisation:true repo
              commit_key
          in
          Lwt.return_ok started
        with exn -> catch_errors "Start GC" exn

      let is_finished = X.Repo.Gc.is_finished
      let behaviour = X.Repo.Gc.behaviour

      let wait repo =
        try
          let* result = finalise_exn ~wait:true repo in
          match result with
          | `Running ->
              assert false (* [~wait:true] should never return [Running] *)
          | `Idle -> Lwt.return_ok None
          | `Finalised stats -> Lwt.return_ok @@ Some stats
        with exn -> catch_errors "Wait for GC" exn

      let run ?(finished = fun _ -> Lwt.return_unit) repo commit_key =
        let* started = start repo commit_key in
        match started with
        | Ok r ->
            if r then
              X.Repo.Gc.on_finalise repo (fun finalisation_r ->
                  match finalisation_r with
                  | Ok _ as stats -> finished stats
                  | Error err ->
                      let err_msg = map_errors "Finalise GC" err in
                      finished @@ Error err_msg);
            Lwt.return_ok r
        | Error _ as e -> Lwt.return e

      let is_allowed = X.Repo.Gc.is_allowed
      let cancel repo = X.Repo.Gc.cancel repo
      let latest_gc_target = X.Repo.Gc.latest_gc_target
    end

    module Traverse_pack_file = Traverse_pack_file.Make (struct
      module File_manager = File_manager
      module Dispatcher = Dispatcher
      module Hash = H
      module Index = Index
      module Inode = X.Node.CA
      module Dict = Dict
      module Contents = X.Contents.Pack_value
      module Commit = X.Commit.Pack_value
    end)

    let traverse_pack_file = Traverse_pack_file.run
    let test_traverse_pack_file = Traverse_pack_file.test

    module Snapshot = struct
      include X.Node.CA.Snapshot

      type t = Inode of inode | Blob of Backend.Contents.Val.t
      [@@deriving irmin]

      module S = Snapshot.Make (struct
        module Hash = H
        module Inode = X.Node.CA
        module Contents_pack = X.Contents.CA
        module Fm = File_manager
        module Dispatcher = Dispatcher
      end)

      include S

      module Export = struct
        let iter ?on_disk repo f ~root_key =
          [%log.debug "Iterate over a tree"];
          let contents = X.Repo.contents_t repo in
          let nodes = X.Repo.node_t repo |> snd in
          let export = S.Export.v repo.config contents nodes in
          let f_contents x = f (Blob x) in
          let f_nodes x = f (Inode x) in
          match root_key with
          | `Contents _ -> Fmt.failwith "[root_key] cannot be of type contents"
          | `Node key ->
              let* total =
                Export.run ?on_disk export f_contents f_nodes
                  (key, Pack_value.Kind.Inode_v2_root)
              in
              Export.close export |> Errs.raise_if_error;
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

        let close process repo =
          flush repo;
          fsync repo;
          Import.close process
      end
    end

    module Internal = struct
      module Io = Io
      module Errs = Errs
      module Index = Index
      module File_manager = File_manager

      let file_manager (repo : X.Repo.t) = repo.fm

      module Dict = Dict

      let dict (repo : X.Repo.t) = repo.dict

      module Dispatcher = Dispatcher

      let dispatcher (repo : X.Repo.t) = repo.dispatcher

      module XKey = XKey

      let suffix_commit_mem repo key =
        X.Commit.CA.unsafe_find ~check_integrity:false
          (snd (X.Repo.commit_t repo))
          key
        |> Option.is_some

      let suffix_node_mem repo key =
        X.Node.CA.unsafe_find ~check_integrity:false
          (snd (X.Repo.node_t repo))
          key
        |> Option.is_some

      let suffix_contents_mem repo key =
        X.Contents.CA.unsafe_find ~check_integrity:false
          (X.Repo.contents_t repo) key
        |> Option.is_some

      let kill_gc (repo : X.Repo.t) =
        match (repo.running_gc : X.Repo.running_gc option) with
        | None -> false
        | Some { gc; _ } -> (
            try X.Gc.cancel gc
            with Unix.Unix_error (Unix.ESRCH, "kill", _) -> false)
    end
  end
end
