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
module Payload = Control_file.Payload.Upper.Latest

exception Pack_error = Errors.Pack_error

module Make (Args : Gc_args.S) = struct
  open Args
  module Io = Fm.Io
  module Lower = Fm.Lower
  module Sparse = Dispatcher.Fm.Sparse
  module Ao = Append_only_file.Make (Fm.Io) (Errs)

  let string_of_key = Irmin.Type.to_string key_t

  module Live = struct
    type t = (int63 * int) list

    let empty = []

    let add ~off ~len (t : t) : t =
      let off_end = Int63.(Syntax.(off + of_int len)) in
      match t with
      | (off', len') :: rest when off_end = off' -> (off, len + len') :: rest
      | _ -> (off, len) :: t
  end

  module Priority_queue = struct
    module Offset_rev = struct
      type t = int63

      let equal = Int63.equal
      let hash (t : t) = Hashtbl.hash t
      let compare a b = Int63.compare b a
    end

    module Table = Hashtbl.Make (Offset_rev)
    module Pq = Binary_heap.Make (Offset_rev)

    type 'a t = { pq : Pq.t; marks : 'a Table.t }

    let create size =
      { pq = Pq.create ~dummy:Int63.zero size; marks = Table.create size }

    let is_empty t = Pq.is_empty t.pq

    let pop { pq; marks } =
      let elt = Pq.pop_minimum pq in
      let payload = Table.find marks elt in
      Table.remove marks elt;
      (elt, payload)

    let add { pq; marks } elt payload =
      if not (Table.mem marks elt) then (
        Table.add marks elt payload;
        Pq.add pq elt)
  end

  (** [iter_reachable commit node_store ~f] calls [f ~off ~len] once for each
      [offset] and [length] of the reachable tree objects and immediate parent
      commits from [commit] in [node_store]. *)
  let iter_reachable commit node_store ~f =
    let todos = Priority_queue.create 1024 in
    let rec loop () =
      if not (Priority_queue.is_empty todos) then (
        let offset, has_children = Priority_queue.pop todos in
        let node_key = Node_store.key_of_offset node_store offset in
        let length = Node_store.get_length node_store node_key in
        f ~off:offset ~len:length;
        if has_children then iter_node node_key;
        loop ())
    and iter_node node_key =
      match Node_store.unsafe_find_no_prefetch node_store node_key with
      | None -> raise (Pack_error (`Dangling_key (string_of_key node_key)))
      | Some node ->
          List.iter
            (fun (_step, kinded_key) -> schedule_kinded kinded_key)
            (Node_value.pred node)
    and schedule_kinded kinded_key =
      let key, has_children =
        match kinded_key with
        | `Contents key -> (key, false)
        | `Inode key | `Node key -> (key, true)
      in
      schedule key has_children
    and schedule key has_children =
      match Node_store.get_offset node_store key with
      | offset -> Priority_queue.add todos offset has_children
      | exception Pack_store.Dangling_hash -> ()
    in
    (* Include the commit parents in the reachable file.
       The parent(s) of [commit] must be included in the iteration
       because, when decoding the [Commit_value.t] at [commit], the
       parents will have to be read in order to produce a key for them. *)
    List.iter (fun key -> schedule key false) (Commit_value.parents commit);
    schedule (Commit_value.node commit) true;
    loop ()

  (* Dangling_parent_commit are the parents of the gced commit. They are kept on
     disk in order to correctly deserialised the gced commit. *)
  let magic_parent =
    Pack_value.Kind.to_magic Pack_value.Kind.Dangling_parent_commit
    |> String.make 1

  (* Transfer the commit with a different magic. Note that this is modifying
     existing written data. *)
  let transfer_parent_commit_exn ~write_exn key =
    match Pack_key.inspect key with
    | Indexed _ ->
        (* Its possible that some parents are referenced by hash. *)
        ()
    | Direct { offset = off; _ } ->
        (* Targeted write to change the parent commit kind to dangling. *)
        let off = Int63.(Syntax.(off + of_int Hash.hash_size)) in
        write_exn ~off ~len:1 magic_parent

  let prefix_file_sizes ~root ~generation =
    let open Result_syntax in
    let* prefix_size =
      if generation = 0 then Ok Int63.zero
      else Irmin_pack.Layout.V3.prefix ~root ~generation |> Io.size_of_path
    in
    let+ mapping_size =
      if generation = 0 then Ok Int63.zero
      else Irmin_pack.Layout.V3.mapping ~root ~generation |> Io.size_of_path
    in
    (mapping_size, prefix_size)

  let report_old_file_sizes ~root ~generation stats =
    let open Result_syntax in
    let+ mapping_size, prefix_size = prefix_file_sizes ~root ~generation in
    stats := Gc_stats.Worker.add_file_size !stats "old_prefix" prefix_size;
    stats := Gc_stats.Worker.add_file_size !stats "old_mapping" mapping_size

  let report_new_file_sizes ~root ~generation stats =
    let open Result_syntax in
    let+ mapping_size, prefix_size = prefix_file_sizes ~root ~generation in
    stats := Gc_stats.Worker.add_file_size !stats "prefix" prefix_size;
    stats := Gc_stats.Worker.add_file_size !stats "mapping" mapping_size

  type suffix_params = {
    start_offset : int63;
    chunk_start_idx : int;
    dead_bytes : int63;
  }
  [@@deriving irmin]

  type gc_results = {
    suffix_params : suffix_params;
    removable_chunk_idxs : int list;
    modified_volume : string option;
    stats : Stats.Latest_gc.worker;
  }
  [@@deriving irmin]

  type gc_output = (gc_results, Args.Errs.t) result [@@deriving irmin]

  let run ~lower_root ~generation ~new_files_path root commit_key
      new_suffix_start_offset =
    let open Result_syntax in
    let config =
      Irmin_pack.Conf.init ~fresh:false ~readonly:true ~lru_size:0 ~lower_root
        root
    in

    (* Step 1. Open the files *)
    [%log.debug "GC: opening files in RO mode"];
    let stats = ref (Gc_stats.Worker.create "open files") in
    let () =
      report_old_file_sizes ~root ~generation:(generation - 1) stats |> ignore
    in

    let fm = Fm.open_ro config |> Errs.raise_if_error in
    Errors.finalise_exn (fun _outcome ->
        Fm.close fm |> Errs.log_if_error "GC: Close File_manager")
    @@ fun () ->
    let dict = Dict.v fm |> Errs.raise_if_error in
    let dispatcher = Dispatcher.v fm |> Errs.raise_if_error in
    let node_store = Node_store.v ~config ~fm ~dict ~dispatcher in
    let commit_store = Commit_store.v ~config ~fm ~dict ~dispatcher in

    (* Step 2. Load commit which will make [commit_key] [Direct] if it's not
       already the case. *)
    stats := Gc_stats.Worker.finish_current_step !stats "load commit";
    let commit =
      match
        Commit_store.unsafe_find ~check_integrity:false commit_store commit_key
      with
      | None ->
          Errs.raise_error (`Commit_key_is_dangling (string_of_key commit_key))
      | Some commit -> commit
    in

    (* Step 3. Compute the list of [offset, length] ranges of live objects
       reachable from the GC commit. *)
    let live_entries =
      stats := Gc_stats.Worker.finish_current_step !stats "mapping: start";
      (* Step 3.1 The compact list of reachable [offset, length] *)
      let live_entries = ref Live.empty in
      let register_entry ~off ~len =
        stats := Gc_stats.Worker.incr_objects_traversed !stats;
        live_entries := Live.add ~off ~len !live_entries
      in
      (* Step 3.2 If the commit parents are referenced by offset, then include
         the commit parents in the reachable list. The parent(s) of [commit_key]
         must be included in the iteration because, when decoding the
         [Commit_value.t] at [commit_key], the parents will have to be read in
         order to produce a key for them. If the parent is referenced by hash,
         there is no need to read it from disk, as their key is of type Indexed
         hash. *)
      stats :=
        Gc_stats.Worker.finish_current_step !stats
          "mapping: commits to reachable";

      (* Step 3.3 Put the commit in the reachable file. *)
      let off = Node_store.get_offset node_store commit_key in
      let len = Node_store.get_length node_store commit_key in
      register_entry ~off ~len;

      (* Step 3.4 Put the nodes and contents in the reachable list. *)
      stats :=
        Gc_stats.Worker.finish_current_step !stats
          "mapping: objects to reachable";
      iter_reachable commit node_store ~f:register_entry;

      (* Step 3.5 Return *)
      stats :=
        Gc_stats.Worker.finish_current_step !stats "mapping: of reachable";
      !live_entries
    in

    let () =
      (* Step 4. Create the new prefix. *)
      stats := Gc_stats.Worker.finish_current_step !stats "prefix: start";
      let mapping =
        Irmin_pack.Layout.V4.mapping ~root:new_files_path ~generation
      in
      let data = Irmin_pack.Layout.V4.prefix ~root:new_files_path ~generation in
      let () =
        let prefix = Sparse.Ao.create ~mapping ~data |> Errs.raise_if_error in
        (* Step 5. Transfer to the new prefix, flush and close. *)
        [%log.debug "GC: transfering to the new prefix"];
        stats := Gc_stats.Worker.finish_current_step !stats "prefix: transfer";
        Errors.finalise_exn (fun _outcome ->
            Sparse.Ao.flush prefix
            >>= (fun _ -> Sparse.Ao.close prefix)
            |> Errs.log_if_error "GC: Close prefix after data copy")
        @@ fun () ->
        (* Step 5.1. Transfer all. *)
        List.iter
          (fun (off, len) ->
            let len = Int63.of_int len in
            let str = Dispatcher.read_seq_exn dispatcher ~off ~len in
            Sparse.Ao.append_seq_exn prefix ~off str)
          live_entries
      in
      (* Step 5.2. Update the parent commits to be dangling: reopen the new
         prefix, this time in write-only as we have to
         modify data inside the file. *)
      stats :=
        Gc_stats.Worker.finish_current_step !stats
          "prefix: rewrite commit parents";
      let prefix = Sparse.Wo.open_wo ~mapping ~data |> Errs.raise_if_error in
      Errors.finalise_exn (fun _outcome ->
          Sparse.Wo.fsync prefix
          >>= (fun _ -> Sparse.Wo.close prefix)
          |> Errs.log_if_error "GC: Close prefix after parent rewrite")
      @@ fun () ->
      let write_exn = Sparse.Wo.write_exn prefix in
      List.iter
        (fun key -> transfer_parent_commit_exn ~write_exn key)
        (Commit_value.parents commit)
    in
    let () = report_new_file_sizes ~root ~generation stats |> ignore in

    (* Step 6. Calculate post-GC suffix parameters. *)
    let suffix_params, removable_chunk_idxs =
      stats :=
        Gc_stats.Worker.finish_current_step !stats
          "suffix: calculate new values";
      let suffix = Fm.suffix fm in
      let soff = Dispatcher.soff_of_offset dispatcher new_suffix_start_offset in
      (* Step 6.1. Calculate chunks that we have GCed. *)
      let open struct
        type chunk = { idx : int; end_suffix_off : int63 }
      end in
      let removable_chunks =
        match Fm.Suffix.chunk_num suffix with
        | 1 -> [] (* We never remove a single chunk. *)
        | _ ->
            Fm.Suffix.fold_chunks
              (fun ~acc ~idx ~start_suffix_off ~end_suffix_off ~is_appendable ->
                (* Remove chunks that end at or before our new split point.
                   This will leave the chunk that starts with (or contains) the
                   split point but remove all previous chunks.
                   We never remove empty or appendable chunks. *)
                let is_empty = start_suffix_off = end_suffix_off in
                let ends_with_or_is_before_soff = end_suffix_off <= soff in
                let is_removable =
                  (not is_appendable)
                  && (not is_empty)
                  && ends_with_or_is_before_soff
                in
                if is_removable then { idx; end_suffix_off } :: acc else acc)
              [] suffix
      in
      (* Step 6.2. Calculate the new chunk starting idx. *)
      let chunk_start_idx =
        match removable_chunks with
        | [] -> Fm.Suffix.start_idx suffix
        | last_removed_chunk :: _ -> succ last_removed_chunk.idx
      in
      (* Step 6.3. Calculate new dead bytes at the beginning of the suffix. *)
      let suffix_dead_bytes =
        match removable_chunks with
        (* If no chunks are GCed, the dead bytes are equivalent to the physical
           offset of new suffix offset. *)
        | [] -> Dispatcher.soff_of_offset dispatcher new_suffix_start_offset
        (* Otherwise, it is the difference between the last chunk removed's end offset
           and the new start offset. *)
        | last_removed_chunk :: _ ->
            let removed_end_offset =
              last_removed_chunk.end_suffix_off
              |> Dispatcher.offset_of_soff dispatcher
            in
            Int63.Syntax.(new_suffix_start_offset - removed_end_offset)
      in
      (* Step 6.4. Assertions and record construction. *)
      assert (Int63.Syntax.(suffix_dead_bytes >= Int63.zero));
      let removable_chunk_idxs =
        removable_chunks |> List.map (fun c -> c.idx)
      in
      ( {
          start_offset = new_suffix_start_offset;
          dead_bytes = suffix_dead_bytes;
          chunk_start_idx;
        },
        removable_chunk_idxs )
    in

    (* Step 7. If we have a lower, archive the removed chunks *)
    let modified_volume =
      match Fm.gc_destination fm with
      | `Delete -> None
      | `Archive lower ->
          [%log.debug "GC: archiving into lower"];
          (* off should not include old dead bytes *)
          let off = Dispatcher.suffix_start_offset dispatcher in
          let discarded_data =
            (* len should include new dead bytes *)
            let len = Int63.Syntax.(new_suffix_start_offset - off) in
            Dispatcher.read_seq_exn dispatcher ~off ~len
          in
          Lower.set_readonly lower false;
          let vol =
            Lower.archive_seq_exn ~upper_root:root ~generation
              ~to_archive:discarded_data lower ~off
          in
          Lower.set_readonly lower true;
          Some vol
    in

    (* Step 8. Finalise stats and return. *)
    let stats = Gc_stats.Worker.finalise !stats in
    { suffix_params; removable_chunk_idxs; modified_volume; stats }

  let write_gc_output ~root ~generation output =
    let open Result_syntax in
    let path = Irmin_pack.Layout.V4.gc_result ~root ~generation in
    let* io = Io.create ~path ~overwrite:true in
    let out = Irmin.Type.to_json_string gc_output_t output in
    let* () = Io.write_string io ~off:Int63.zero out in
    let* () = Io.fsync io in
    let* () = Io.close io in
    Io.fsync_dir root

  (* No one catches errors when this function terminates. Write the result in a
     file and terminate. *)
  let run_and_output_result ~lower_root ~generation ~new_files_path root
      commit_key new_suffix_start_offset =
    let result =
      Errs.catch (fun () ->
          run ~lower_root ~generation ~new_files_path root commit_key
            new_suffix_start_offset)
    in
    Errs.log_if_error "gc run" result;
    let write_result = write_gc_output ~root ~generation result in
    write_result |> Errs.log_if_error "writing gc output"
  (* No need to raise or log if [result] is [Error _], we've written it in
     the file. *)
end
