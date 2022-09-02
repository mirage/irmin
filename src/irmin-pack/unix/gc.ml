(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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
include Gc_intf

(** Code for running in an async GC worker thread. *)
module Worker = struct
  module Payload = Control_file.Latest_payload

  let buffer_size = 8192

  exception Pack_error = Errors.Pack_error

  module type S = sig
    module Args : Args

    val run_and_output_result : generation:int -> string -> Args.key -> int63

    val transfer_append_exn :
      read_exn:(off:int63 -> len:int -> bytes -> unit) ->
      append_exn:(string -> unit) ->
      off:int63 ->
      len:int63 ->
      bytes ->
      unit
  end

  module Make (Args : Args) : S with module Args := Args = struct
    open Args
    module Io = Fm.Io
    module Mapping_file = Fm.Mapping_file

    module Ao = struct
      include Append_only_file.Make (Fm.Io)

      let create_rw_exn ~path ~auto_flush_callback =
        create_rw ~path ~overwrite:true ~auto_flush_threshold:1_000_000
          ~auto_flush_callback
        |> Errs.raise_if_error
    end

    module X = struct
      type t = int63 [@@deriving irmin]

      let equal = Irmin.Type.(unstage (equal t))
      let hash = Irmin.Type.(unstage (short_hash t))
      let hash (t : t) : int = hash t
    end

    module Table = Hashtbl.Make (X)

    let string_of_key = Irmin.Type.to_string key_t

    let transfer_append_exn ~read_exn ~append_exn ~(off : int63) ~(len : int63)
        buffer =
      let buffer_size = Bytes.length buffer |> Int63.of_int in
      let rec aux off len_remaining =
        let open Int63.Syntax in
        let min a b = if a < b then a else b in
        let len = min buffer_size len_remaining in
        let len' = Int63.to_int len in
        read_exn ~off ~len:len' buffer;
        let () =
          if len = buffer_size then append_exn (Bytes.to_string buffer)
          else append_exn (Bytes.sub_string buffer 0 len')
        in
        let len_remaining = len_remaining - len in
        if len_remaining > Int63.zero then aux (off + len) len_remaining
      in
      aux off len

    (** [iter_from_node_key node_key _ _ ~f] calls [f] with the key of the node
        and iterates over its children.

        [f k] returns [Follow] or [No_follow], indicating the iteration
        algorithm if the children of [k] should be traversed or skiped. *)
    let iter node_key node_store ~f k =
      let marks = Table.create 1024 in
      let mark offset = Table.add marks offset () in
      let has_mark offset = Table.mem marks offset in
      let rec iter_from_node_key_exn node_key node_store ~f k =
        match
          Node_store.unsafe_find ~check_integrity:false node_store node_key
        with
        | None -> raise (Pack_error (`Dangling_key (string_of_key node_key)))
        | Some node ->
            iter_from_node_children_exn node_store ~f (Node_value.pred node) k
      and iter_from_node_children_exn node_store ~f children k =
        match children with
        | [] -> k ()
        | (_step, kinded_key) :: tl -> (
            let k () = iter_from_node_children_exn node_store ~f tl k in
            match kinded_key with
            | `Contents key ->
                let (_ : int63) = f key in
                k ()
            | `Inode key | `Node key ->
                let offset = f key in
                if has_mark offset then k ()
                else (
                  mark offset;
                  iter_from_node_key_exn key node_store ~f k))
      in
      iter_from_node_key_exn node_key node_store ~f k

    (* Dangling_parent_commit are the parents of the gced commit. They are kept on
       disk in order to correctly deserialised the gced commit. *)
    let magic_parent =
      Pack_value.Kind.to_magic Pack_value.Kind.Dangling_parent_commit

    (* Transfer the commit with a different magic. Note that this is modifying
       existing written data. *)
    let transfer_parent_commit_exn ~read_exn ~write_exn ~mapping key =
      let off, len =
        match Irmin_pack.Pack_key.inspect key with
        | Indexed _ ->
            (* As this is the second time we are reading this key, this case is
               unreachable. *)
            assert false
        | Direct { offset; length; _ } -> (offset, length)
      in
      let buffer = Bytes.create len in
      read_exn ~off ~len buffer;
      let entry = Mapping_file.find_nearest_leq mapping off in
      match entry with
      | None -> assert false
      | Some { poff; _ } ->
          Bytes.set buffer Hash.hash_size magic_parent;
          (* Bytes.unsafe_to_string usage: We assume read_exn returns unique ownership of buffer
             to this function. Then at the call to Bytes.unsafe_to_string we give up unique
             ownership to buffer (we do not modify it thereafter) in return for ownership of the
             resulting string, which we pass to write_exn. This usage is safe. *)
          write_exn ~off:poff ~len (Bytes.unsafe_to_string buffer)

    let create_new_suffix ~root ~generation =
      let path = Irmin_pack.Layout.V3.suffix ~root ~generation in
      let auto_flush_callback x = Ao.flush x |> Errs.raise_if_error in
      Ao.create_rw_exn ~path ~auto_flush_callback

    let run ~generation root commit_key =
      let open Result_syntax in
      let config =
        Irmin_pack.Conf.init ~fresh:false ~readonly:true ~lru_size:0 root
      in

      (* Step 1. Open the files *)
      [%log.debug "GC: opening files in RO mode"];
      let fm = Fm.open_ro config |> Errs.raise_if_error in
      Errors.finalise_exn (fun _outcome ->
          Fm.close fm |> Errs.log_if_error "GC: Close File_manager")
      @@ fun () ->
      let dict = Dict.v fm |> Errs.raise_if_error in
      let dispatcher = Dispatcher.v ~root fm |> Errs.raise_if_error in
      let node_store = Node_store.v ~config ~fm ~dict ~dispatcher in
      let commit_store = Commit_store.v ~config ~fm ~dict ~dispatcher in

      (* Step 2. Load commit which will make [commit_key] [Direct] if it's not
         already the case. *)
      let commit =
        match
          Commit_store.unsafe_find ~check_integrity:false commit_store
            commit_key
        with
        | None ->
            Errs.raise_error
              (`Commit_key_is_dangling (string_of_key commit_key))
        | Some commit -> commit
      in
      let commit_offset, _ =
        let state : _ Irmin_pack.Pack_key.state =
          Irmin_pack.Pack_key.inspect commit_key
        in
        match state with
        | Indexed _ -> assert false
        | Direct x -> (x.offset, x.length)
      in

      (* Step 3. Create the new mapping. *)
      let mapping =
        (* Step 3.1 Start [Mapping_file] routine which will create the
           reachable file. *)
        (fun f ->
          Mapping_file.create ~root ~generation ~register_entries:f
          |> Errs.raise_if_error)
        @@ fun ~register_entry ->
        (* Step 3.2 Put the commit parents in the reachable file.
           The parent(s) of [commit_key] must be included in the iteration
           because, when decoding the [Commit_value.t] at [commit_key], the
           parents will have to be read in order to produce a key for them. *)
        let register_object_exn key =
          match Irmin_pack.Pack_key.inspect key with
          | Indexed _ ->
              raise
                (Pack_error (`Commit_parent_key_is_indexed (string_of_key key)))
          | Direct { offset; length; _ } ->
              register_entry ~off:offset ~len:length
        in
        List.iter register_object_exn (Commit_value.parents commit);

        (* Step 3.3 Put the nodes and contents in the reachable file. *)
        let register_object_exn key =
          match Irmin_pack.Pack_key.inspect key with
          | Indexed _ ->
              raise
                (Pack_error
                   (`Node_or_contents_key_is_indexed (string_of_key key)))
          | Direct { offset; length; _ } ->
              register_entry ~off:offset ~len:length;
              offset
        in
        let node_key = Commit_value.node commit in
        let (_ : int63) = register_object_exn node_key in
        iter node_key node_store ~f:register_object_exn (fun () -> ());

        (* Step 3.4 Return and let the [Mapping_file] routine create the mapping
           file. *)
        ()
      in

      let () =
        (* Step 4. Create the new prefix. *)
        let prefix =
          let path = Irmin_pack.Layout.V3.prefix ~root ~generation in
          let auto_flush_callback x = Ao.flush x |> Errs.raise_if_error in
          Ao.create_rw_exn ~path ~auto_flush_callback
        in
        let () =
          Errors.finalise_exn (fun _outcome ->
              Ao.close prefix |> Errs.log_if_error "GC: Close prefix")
          @@ fun () ->
          ();

          (* Step 5. Transfer to the new prefix, flush and close. *)
          [%log.debug "GC: transfering to the new prefix"];
          let buffer = Bytes.create buffer_size in
          (* Step 5.1. Transfer all. *)
          let read_exn = Dispatcher.read_in_prefix_and_suffix_exn dispatcher in
          let append_exn = Ao.append_exn prefix in
          let f ~off ~len =
            let len = Int63.of_int len in
            transfer_append_exn ~read_exn ~append_exn ~off ~len buffer
          in
          let () = Mapping_file.iter_exn mapping f in
          Ao.flush prefix |> Errs.raise_if_error
        in
        (* Step 5.2. Transfer again the parent commits but with a modified
           magic. Reopen the new prefix, this time _not_ in append-only
           as we have to modify data inside the file. *)
        let read_exn ~off ~len buf =
          let accessor = Dispatcher.create_accessor_exn dispatcher ~off ~len in
          Dispatcher.read_exn dispatcher accessor buf
        in
        let prefix =
          let path = Irmin_pack.Layout.V3.prefix ~root ~generation in
          Io.open_ ~path ~readonly:false |> Errs.raise_if_error
        in
        Errors.finalise_exn (fun _outcome ->
            Io.fsync prefix
            >>= (fun _ -> Io.close prefix)
            |> Errs.log_if_error "GC: Close prefix after parent rewrite")
        @@ fun () ->
        let write_exn = Io.write_exn prefix in
        List.iter
          (fun key ->
            transfer_parent_commit_exn ~read_exn ~write_exn ~mapping key)
          (Commit_value.parents commit)
      in

      (* Step 6. Create the new suffix and prepare 2 functions for read and write
         operations. *)
      let buffer = Bytes.create buffer_size in
      [%log.debug "GC: creating new suffix"];
      let suffix = create_new_suffix ~root ~generation in
      Errors.finalise_exn (fun _outcome ->
          Ao.fsync suffix
          >>= (fun _ -> Ao.close suffix)
          |> Errs.log_if_error "GC: Close suffix")
      @@ fun () ->
      let read_exn ~off ~len buf =
        let accessor = Dispatcher.create_accessor_exn dispatcher ~off ~len in
        Dispatcher.read_exn dispatcher accessor buf
      in
      let append_exn = Ao.append_exn suffix in
      let transfer_exn = transfer_append_exn ~read_exn ~append_exn buffer in

      (* Step 7. Transfer to the next suffix. *)
      [%log.debug "GC: transfering to the new suffix"];
      let num_iterations = 7 in
      (* [transfer_loop] is needed because after garbage collection there may be new objects
         at the end of the suffix file that need to be copied over *)
      let rec transfer_loop i ~off =
        if i = 0 then off
        else
          let () = Fm.reload fm |> Errs.raise_if_error in
          let pl : Payload.t = Fm.Control.payload (Fm.control fm) in
          let end_offset =
            Dispatcher.offset_of_suffix_off dispatcher
              pl.entry_offset_suffix_end
          in
          let len = Int63.Syntax.(end_offset - off) in
          [%log.debug
            "GC: transfer_loop iteration %d, offset %a, length %a"
              (num_iterations - i + 1)
              Int63.pp off Int63.pp len];
          let () = transfer_exn ~off ~len in
          (* Check how many bytes are left, [4096*5] is selected because it is roughly the
             number of bytes that requires a read from the block device on ext4 *)
          if Int63.to_int len < 4096 * 5 then end_offset
          else
            let off = Int63.Syntax.(off + len) in
            transfer_loop ~off (i - 1)
      in
      let offs = transfer_loop ~off:commit_offset num_iterations in
      Ao.flush suffix |> Errs.raise_if_error;

      (* Step 8. Inform the caller of the end_offset copied. *)
      offs

    (* No one catches errors when this function terminates. Write the result in a
       file and terminate the process with an exception, if needed. *)
    let run_and_output_result ~generation root commit_key =
      let result = Errs.catch (fun () -> run ~generation root commit_key) in
      let write_result = Fm.write_gc_output ~root ~generation result in
      write_result |> Errs.raise_if_error;
      result |> Errs.raise_if_error
  end
end

(** Maker for a module that can manage GC processes. *)
module Make (Args : Args) = struct
  module Args = Args
  open Args
  module Io = Fm.Io
  module Ao = Append_only_file.Make (Io)
  module Worker = Worker.Make (Args)

  type t = {
    root : string;
    generation : int;
    task : Async.t;
    unlink : bool;
    offset : int63;
    elapsed : unit -> float;
    resolver : (stats, Errs.t) result Lwt.u;
    promise : (stats, Errs.t) result Lwt.t;
    dispatcher : Dispatcher.t;
    fm : Fm.t;
    contents : read Contents_store.t;
    node : read Node_store.t;
    commit : read Commit_store.t;
    mutable stats : stats option;
  }

  let v ~root ~generation ~unlink ~offset ~dispatcher ~fm ~contents ~node
      ~commit commit_key =
    let unlink_result_file () =
      let result_file = Irmin_pack.Layout.V3.gc_result ~root ~generation in
      match Io.unlink result_file with
      | Ok () -> ()
      | Error (`Sys_error msg as err) ->
          if msg <> Fmt.str "%s: No such file or directory" result_file then
            [%log.warn
              "Unlinking temporary files from previous failed gc. Failed with \
               error %a"
              Errs.pp err]
    in
    (* Unlink next gc's result file, in case it is on disk, for instance
       after a failed gc. *)
    unlink_result_file ();
    (* function to track durations *)
    let elapsed =
      let c0 = Mtime_clock.counter () in
      fun () -> Mtime_clock.count c0 |> Mtime.Span.to_s
    in
    (* internal promise for gc *)
    let promise, resolver = Lwt.wait () in
    (* start worker task *)
    let task =
      Async.async (fun () ->
          let (_ : int63) =
            Worker.run_and_output_result root commit_key ~generation
          in
          ())
    in
    {
      root;
      generation;
      unlink;
      offset;
      task;
      promise;
      resolver;
      elapsed;
      dispatcher;
      fm;
      contents;
      node;
      commit;
      stats = None;
    }

  let open_new_suffix ~end_offset { root; generation; _ } =
    let open Result_syntax in
    let path = Irmin_pack.Layout.V3.suffix ~root ~generation in
    (* As the new suffix is necessarily in V3, the dead_header_size is
       0. *)
    let dead_header_size = 0 in
    let auto_flush_threshold = 1_000_000 in
    let auto_flush_callback x = Ao.flush x |> Errs.raise_if_error in
    let* suffix =
      Ao.open_rw ~path ~end_offset ~dead_header_size ~auto_flush_callback
        ~auto_flush_threshold
    in
    Ok suffix

  let transfer_latest_newies ~right_start_offset ~copy_end_offset t =
    [%log.debug "Gc in main: transfer latest newies"];
    let open Result_syntax in
    let open Int63.Syntax in
    let old_end_offset = Dispatcher.end_offset t.dispatcher in
    let remaining = old_end_offset - copy_end_offset in
    (* When opening the suffix in append_only we need to provide a
       (real) suffix offset, computed from the global ones. *)
    let suffix_end_offset = copy_end_offset - right_start_offset in
    let* new_suffix = open_new_suffix ~end_offset:suffix_end_offset t in
    Errors.finalise (fun _ ->
        Ao.close new_suffix
        |> Errs.log_if_error "GC: Close suffix after copy latest newies")
    @@ fun () ->
    let buffer = Bytes.create 8192 in
    let read_exn ~off ~len buf =
      let accessor = Dispatcher.create_accessor_exn t.dispatcher ~off ~len in
      Dispatcher.read_exn t.dispatcher accessor buf
    in
    let append_exn = Ao.append_exn new_suffix in
    let flush_and_raise () = Ao.flush new_suffix |> Errs.raise_if_error in
    let* () =
      Errs.catch (fun () ->
          Worker.transfer_append_exn ~read_exn ~append_exn ~off:copy_end_offset
            ~len:remaining buffer;
          flush_and_raise ())
    in
    Ok old_end_offset

  let swap_and_purge ~right_start_offset ~right_end_offset t =
    let open Result_syntax in
    let c0 = Mtime_clock.counter () in

    let* () =
      Fm.swap t.fm ~generation:t.generation ~right_start_offset
        ~right_end_offset
    in
    let span1 = Mtime_clock.count c0 |> Mtime.Span.to_s in

    (* No need to purge dict here, as it is global to the store. *)
    (* No need to purge index here. It is global too, but some hashes may
       not point to valid offsets anymore. Pack_store will just say that
       such keys are not member of the store. *)
    Contents_store.purge_lru t.contents;
    Node_store.purge_lru t.node;
    Commit_store.purge_lru t.commit;
    let span2 = Mtime_clock.count c0 |> Mtime.Span.to_s in
    [%log.debug "Gc swap and purge: %.6fs, %.6fs" span1 (span2 -. span1)];
    [%log.info "GC: end"];
    Ok ()

  let unlink_all { root; generation; _ } =
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
            Irmin_pack.Layout.V3.prefix ~root ~generation:(generation - 1)
          in
          let* () = Io.unlink prefix in
          (* Unlink previous mapping. *)
          let mapping =
            Irmin_pack.Layout.V3.mapping ~root ~generation:(generation - 1)
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
          "Unlinking temporary files after gc, failed with error %a" Errs.pp e]
    | Ok () -> ()

  let gc_errors status gc_output =
    let extend_error s = function
      | `Gc_process_error str -> `Gc_process_error (Fmt.str "%s %s" s str)
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

  let finalise ~wait t =
    match t.stats with
    | Some stats -> Lwt.return_ok (`Finalised stats)
    | None -> (
        let go status =
          let start = t.elapsed () in
          let s =
            ref
              {
                duration = 0.;
                finalisation_duration = 0.;
                read_gc_output_duration = 0.;
                transfer_latest_newies_duration = 0.;
                swap_duration = 0.;
                unlink_duration = 0.;
              }
          in
          let time on_end f =
            let counter = Mtime_clock.counter () in
            let res = f () in
            on_end (Mtime_clock.count counter |> Mtime.Span.to_s);
            res
          in

          let gc_output =
            time (fun t -> s := { !s with read_gc_output_duration = t })
            @@ fun () -> Fm.read_gc_output ~root:t.root ~generation:t.generation
          in

          let result =
            let open Result_syntax in
            match (status, gc_output) with
            | `Success, Ok copy_end_offset ->
                let* new_suffix_end_offset =
                  time (fun t ->
                      s := { !s with transfer_latest_newies_duration = t })
                  @@ fun () ->
                  transfer_latest_newies ~right_start_offset:t.offset
                    ~copy_end_offset t
                in
                let* () =
                  time (fun t -> s := { !s with swap_duration = t })
                  @@ fun () ->
                  swap_and_purge ~right_start_offset:t.offset
                    ~right_end_offset:new_suffix_end_offset t
                in
                (if t.unlink then
                 time (fun t -> s := { !s with unlink_duration = t })
                 @@ fun () -> unlink_all t);

                let duration = t.elapsed () in
                s :=
                  {
                    !s with
                    duration;
                    finalisation_duration = duration -. start;
                  };
                t.stats <- Some !s;

                [%log.debug
                  "Gc ended. %a, newies bytes:%a" pp_stats !s Int63.pp
                    (Int63.sub new_suffix_end_offset copy_end_offset)];
                let () = Lwt.wakeup_later t.resolver (Ok !s) in
                Ok (`Finalised !s)
            | _ ->
                let err = gc_errors status gc_output in
                let () = Lwt.wakeup_later t.resolver err in
                err
          in
          Lwt.return result
        in
        if wait then
          let* status = Async.await t.task in
          go status
        else
          match Async.status t.task with
          | `Running -> Lwt.return_ok `Running
          | status -> go status)

  let on_finalise t f =
    (* Ignore returned promise since the purpose of this
       function is to add asynchronous callbacks to the GC
       process -- this promise binding is an internal
       implementation detail. This is safe since the callback
       [f] is attached to [t.running_gc.promise], which is
       referenced for the lifetime of a GC process. *)
    let _ = Lwt.bind t.promise f in
    ()

  let cancel t = Async.cancel t.task
end
