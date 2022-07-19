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
    module Io = Io.Unix
    module Errs = Io_errors.Make (Io)
    module Control = Control_file.Make (Io)
    module Aof = Append_only_file.Make (Io)
    module File_manager = File_manager.Make (Control) (Aof) (Aof) (Index) (Errs)
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
        module Fm = File_manager
        module Errs = Errs
        module Dict = Dict
        module Dispatcher = Dispatcher
        module Hash = Schema.Hash
        module Node_value = Node.CA.Inter.Val
        module Node_store = Node.CA
        module Commit_value = Commit.Value
        module Commit_store = Commit.CA

        type hash = Node_value.hash
        type key = Node_value.node_key [@@deriving irmin]
      end)

      type gc_stats = { elapsed : float }

      type during_gc = {
        next_generation : int;
        task : Io.task;
        unlink : bool;
        offset : int63;
        elapsed : unit -> float;
        resolver : (gc_stats option, Errs.t) result Lwt.u;
        promise : (gc_stats option, Errs.t) result Lwt.t;
      }

      module Repo = struct
        type t = {
          config : Irmin.Backend.Conf.t;
          contents : read Contents.CA.t;
          node : read Node.CA.t;
          commit : read Commit.CA.t;
          branch : Branch.t;
          fm : File_manager.t;
          dict : Dict.t;
          dispatcher : Dispatcher.t;
          mutable during_batch : bool;
          mutable during_gc : during_gc option;
        }

        let pp_key = Irmin.Type.pp XKey.t
        let contents_t t : 'a Contents.t = t.contents
        let node_t t : 'a Node.t = (contents_t t, t.node)
        let commit_t t : 'a Commit.t = (node_t t, t.commit)
        let branch_t t = t.branch
        let config t = t.config

        let batch t f =
          t.during_batch <- true;
          let contents = Contents.CA.cast t.contents in
          let node = Node.CA.Pack.cast t.node in
          let commit = Commit.CA.cast t.commit in
          let contents : 'a Contents.t = contents in
          let node : 'a Node.t = (contents, node) in
          let commit : 'a Commit.t = (node, commit) in
          let on_success res =
            t.during_batch <- false;
            File_manager.flush t.fm |> Errs.raise_if_error;
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
                    Errs.pp err]
            in
            (* Kill gc process in at_exit. *)
            raise exn
          in
          Lwt.try_bind (fun () -> f contents node commit) on_success on_fail

        let v config =
          let root = Irmin_pack.Conf.root config in
          let fm =
            let readonly = Irmin_pack.Conf.readonly config in
            if readonly then File_manager.open_ro config |> Errs.raise_if_error
            else
              let fresh = Irmin_pack.Conf.fresh config in
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
          let dispatcher = Dispatcher.v ~root fm |> Errs.raise_if_error in
          let contents = Contents.CA.v ~config ~fm ~dict ~dispatcher in
          let node = Node.CA.v ~config ~fm ~dict ~dispatcher in
          let commit = Commit.CA.v ~config ~fm ~dict ~dispatcher in
          let+ branch =
            let root = Conf.root config in
            let fresh = Conf.fresh config in
            let readonly = Conf.readonly config in
            let path = Irmin_pack.Layout.V3.branch ~root in
            Branch.v ~fresh ~readonly path
          in
          let during_batch = false in
          let during_gc = None in
          {
            config;
            contents;
            node;
            commit;
            branch;
            fm;
            dict;
            during_batch;
            during_gc;
            dispatcher;
          }

        let close t =
          (* Step 1 - Kill the gc process if it is running *)
          let () =
            match t.during_gc with
            | Some { task; _ } ->
                Io.cancel task;
                t.during_gc <- None
            | None -> ()
          in
          (* Step 2 - Close the files *)
          let () = File_manager.close t.fm |> Errs.raise_if_error in
          Branch.close t.branch >>= fun () ->
          (* Step 3 - Close the in-memory abstractions *)
          Dict.close t.dict;
          Contents.CA.close (contents_t t) >>= fun () ->
          Node.CA.close (snd (node_t t)) >>= fun () ->
          Commit.CA.close (snd (commit_t t))

        let flush_with_hook ~hook t =
          File_manager.flush ~hook t.fm |> Errs.raise_if_error

        let reload_with_hook ~hook t =
          File_manager.reload ~hook t.fm |> Errs.raise_if_error

        let flush t = File_manager.flush ?hook:None t.fm |> Errs.raise_if_error
        let reload t = File_manager.reload t.fm |> Errs.raise_if_error

        module Gc = struct
          let unlink_result_file ~root ~generation =
            let result_file =
              Irmin_pack.Layout.V3.gc_result ~root ~generation
            in
            match Io.unlink result_file with
            | Ok () -> ()
            | Error (`Sys_error msg as err) ->
                if msg <> Fmt.str "%s: No such file or directory" result_file
                then
                  [%log.warn
                    "Unlinking temporary files from previous failed gc. Failed \
                     with error %a"
                    Errs.pp err]

          let start ~unlink t commit_key =
            let open Result_syntax in
            [%log.info "GC: Starting on %a" pp_key commit_key];
            let* () =
              if t.during_batch then Error `Gc_forbidden_during_batch else Ok ()
            in
            let* commit_key =
              let state : _ Irmin_pack.Pack_key.state =
                Irmin_pack.Pack_key.inspect commit_key
              in
              match state with
              | Direct _ -> Ok commit_key
              | Indexed h -> (
                  match Commit.CA.index_direct_with_kind t.commit h with
                  | None ->
                      Error
                        (`Commit_key_is_dangling
                          (Irmin.Type.to_string XKey.t commit_key))
                  | Some (k, _kind) -> Ok k)
            in
            let offset =
              let state : _ Irmin_pack.Pack_key.state =
                Irmin_pack.Pack_key.inspect commit_key
              in
              match state with
              | Direct x -> x.offset
              | Indexed _ -> assert false
            in
            let root = Conf.root t.config in
            let* () =
              if not (File_manager.gc_allowed t.fm) then Error `Gc_disallowed
              else Ok ()
            in
            let current_generation = File_manager.generation t.fm in
            let next_generation = current_generation + 1 in
            (* Unlink next gc's result file, in case it is on disk, for instance
               after a failed gc. *)
            unlink_result_file ~root ~generation:next_generation;
            let task =
              Io.async (fun () ->
                  let (_ : int63) =
                    Gc.run_and_output_result root commit_key
                      ~generation:next_generation
                  in
                  ())
            in
            let elapsed =
              let c0 = Mtime_clock.counter () in
              fun () -> Mtime_clock.count c0 |> Mtime.Span.to_ms
            in
            let promise, resolver = Lwt.wait () in
            t.during_gc <-
              Some
                {
                  next_generation;
                  task;
                  unlink;
                  offset;
                  elapsed;
                  promise;
                  resolver;
                };
            Ok ()

          let open_new_suffix ~root ~generation ~end_offset =
            let open Result_syntax in
            let path = Irmin_pack.Layout.V3.suffix ~root ~generation in
            (* As the new suffix is necessarily in V3, the dead_header_size is
               0. *)
            let dead_header_size = 0 in
            let auto_flush_threshold = 1_000_000 in
            let suffix_ref = ref None in
            let auto_flush_callback () =
              match !suffix_ref with
              | None -> assert false
              | Some x -> Aof.flush x |> Errs.raise_if_error
            in
            let* suffix =
              Aof.open_rw ~path ~end_offset ~dead_header_size
                ~auto_flush_callback ~auto_flush_threshold
            in
            suffix_ref := Some suffix;
            Ok suffix

          let transfer_latest_newies ~generation ~right_start_offset
              ~copy_end_offset ~root t =
            [%log.debug "Gc in main: transfer latest newies"];
            let open Result_syntax in
            let open Int63.Syntax in
            let old_end_offset = Dispatcher.end_offset t.dispatcher in
            let remaining = old_end_offset - copy_end_offset in
            (* When opening the suffix in append_only we need to provide a
               (real) suffix offset, computed from the global ones. *)
            let suffix_end_offset = copy_end_offset - right_start_offset in
            let* new_suffix =
              open_new_suffix ~root ~generation ~end_offset:suffix_end_offset
            in
            Errors.finalise (fun _ ->
                Aof.close new_suffix
                |> Errs.log_if_error "GC: Close suffix after copy latest newies")
            @@ fun () ->
            let buffer = Bytes.create 8192 in
            let read_exn = Dispatcher.read_exn t.dispatcher in
            let append_exn = Aof.append_exn new_suffix in
            let flush_and_raise () =
              Aof.flush new_suffix |> Errs.raise_if_error
            in
            let* () =
              Errs.catch (fun () ->
                  Gc.transfer_append_exn ~read_exn ~append_exn
                    ~off:copy_end_offset ~len:remaining buffer;
                  flush_and_raise ())
            in
            Ok old_end_offset

          let swap_and_purge ~generation ~right_start_offset ~right_end_offset t
              =
            let open Result_syntax in
            let* () =
              File_manager.swap t.fm ~generation ~right_start_offset
                ~right_end_offset
            in
            (* No need to purge dict here, as it is global to the store. *)
            (* No need to purge index here. It is global too, but some hashes may
               not point to valid offsets anymore. Pack_store will just say that
               such keys are not member of the store. *)
            Contents.CA.purge_lru t.contents;
            Node.CA.purge_lru t.node;
            Commit.CA.purge_lru t.commit;
            [%log.info "GC: end"];
            Ok ()

          let pp_status = Irmin.Type.pp Io.status_t
          let pp_gc_error = Irmin.Type.pp File_manager.read_gc_output_error_t

          let unlink_all ~root ~generation =
            let result =
              let open Result_syntax in
              (* Unlink previous suffix. *)
              let suffix =
                Irmin_pack.Layout.V3.suffix ~root ~generation:(generation - 1)
              in
              let* () = Io.unlink suffix in
              let* () =
                if generation >= 2 then
                  (* Unlink previous prefix. *)
                  let prefix =
                    Irmin_pack.Layout.V3.prefix ~root
                      ~generation:(generation - 1)
                  in
                  let* () = Io.unlink prefix in
                  (* Unlink previous mapping. *)
                  let mapping =
                    Irmin_pack.Layout.V3.mapping ~root
                      ~generation:(generation - 1)
                  in
                  let* () = Io.unlink mapping in
                  Ok ()
                else Ok ()
              in
              (* Unlink current gc's result.*)
              let result = Irmin_pack.Layout.V3.gc_result ~root ~generation in
              Io.unlink result
            in
            match result with
            | Error e ->
                [%log.warn
                  "Unlinking temporary files after gc, failed with error %a"
                    Errs.pp e]
            | Ok () -> ()

          let gc_errors status gc_output =
            let extend_error s = function
              | `Gc_process_error str ->
                  `Gc_process_error (Fmt.str "%s %s" s str)
              | `Corrupted_gc_result_file str ->
                  `Gc_process_died_without_result_file (Fmt.str "%s %s" s str)
            in
            match (status, gc_output) with
            | `Failure s, Error e -> Error (extend_error s e)
            | `Cancelled, Error e -> Error (extend_error "cancelled" e)
            | `Success, Error e -> Error (extend_error "success" e)
            | `Cancelled, Ok _ -> Error (`Gc_process_error "cancelled")
            | `Failure s, Ok _ -> Error (`Gc_process_error s)
            | `Success, Ok _ -> assert false
            | `Running, _ -> assert false

          let finalise ?hook ~wait t =
            match t.during_gc with
            | None -> Lwt.return_ok `Idle
            | Some
                { next_generation; task; unlink; offset; elapsed; resolver; _ }
              -> (
                let go status =
                  let* () =
                    match hook with
                    | Some h -> h `Before_latest_newies
                    | None -> Lwt.return_unit
                  in
                  let root = Conf.root t.config in
                  let gc_output =
                    File_manager.read_gc_output ~root
                      ~generation:next_generation
                  in
                  let result =
                    let open Result_syntax in
                    match (status, gc_output) with
                    | `Success, Ok copy_end_offset ->
                        let* new_suffix_end_offset =
                          transfer_latest_newies ~generation:next_generation
                            ~right_start_offset:offset ~copy_end_offset ~root t
                        in
                        let* () =
                          swap_and_purge ~generation:next_generation
                            ~right_start_offset:offset
                            ~right_end_offset:new_suffix_end_offset t
                        in
                        let elapsed = elapsed () in
                        let stats = { elapsed } in
                        let () = Lwt.wakeup_later resolver (Ok (Some stats)) in
                        if unlink then
                          unlink_all ~root ~generation:next_generation;
                        Ok (`Finalised stats)
                    | _ ->
                        let err = gc_errors status gc_output in
                        let () = Lwt.wakeup_later resolver err in
                        err
                  in
                  t.during_gc <- None;
                  Lwt.return result
                in
                if t.during_batch then
                  Lwt.return_error `Gc_forbidden_during_batch
                else
                  match wait with
                  | false -> (
                      match Io.status task with
                      | `Running -> Lwt.return_ok `Running
                      | status -> go status)
                  | true ->
                      let* status = Io.await task in
                      go status)

          let start_or_skip ~unlink t commit_key =
            let open Lwt_result.Syntax in
            match t.during_gc with
            | None ->
                let* () = start ~unlink t commit_key |> Lwt.return in
                Lwt.return_ok true
            | Some _ ->
                [%log.info "Repo is alreadying running GC. Skipping."];
                Lwt.return_ok false

          let start_exn ?(unlink = true) t commit_key =
            let* result = start_or_skip ~unlink t commit_key in
            match result with
            | Ok launched -> Lwt.return launched
            | Error e -> Errs.raise_error e

          let finalise_exn ?hook ?(wait = false) t =
            let* result = finalise ?hook ~wait t in
            match result with
            | Ok waited -> Lwt.return waited
            | Error e -> Errs.raise_error e

          let is_finished t = Option.is_none t.during_gc

          let on_finalise t f =
            match t.during_gc with
            | None -> ()
            | Some x -> Lwt.on_success x.promise f
        end
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
      let index = File_manager.index t.fm in
      Checks.integrity_check ?ppf ~auto_repair ~check index

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
    let reload = X.Repo.reload
    let flush = X.Repo.flush

    module Gc = struct
      type msg = [ `Msg of string ]
      type stats = X.gc_stats = { elapsed : float }
      type process_state = [ `Idle | `Running | `Finalised of stats ]

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
        let err = Fmt.str "%a" Errs.pp error in
        let err_msg = Fmt.str "[%s] resulted in error: %s" context err in
        `Msg err_msg

      let finalise_exn_with_hook = X.Repo.Gc.finalise_exn
      let finalise_exn = finalise_exn_with_hook ?hook:None
      let start_exn = X.Repo.Gc.start_exn

      let start repo commit_key =
        try
          let* started = start_exn ~unlink:true repo commit_key in
          Lwt.return_ok started
        with exn -> catch_errors "Start GC" exn

      let is_finished = X.Repo.Gc.is_finished

      let wait repo =
        try
          let* result = finalise_exn ~wait:true repo in
          match result with
          | `Running ->
              assert false (* [~wait:true] should never return [Running] *)
          | `Idle -> Lwt.return_ok None
          | `Finalised stats -> Lwt.return_ok @@ Some stats
        with exn -> catch_errors "Wait for GC" exn

      let run ?(finished = fun _ -> ()) repo commit_key =
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
    end

    module Traverse_pack_file = Traverse_pack_file.Make (struct
      module File_manager = File_manager
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
          Import.close process
      end
    end
  end
end
