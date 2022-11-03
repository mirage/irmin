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
module Payload = Control_file.Latest_payload

exception Pack_error = Errors.Pack_error

module Make (Args : Gc_args.S) = struct
  open Args
  module Io = Fm.Io
  module Mapping_file = Dispatcher.Mapping_file

  module Ao = struct
    include Append_only_file.Make (Fm.Io) (Errs)

    let create_rw_exn ~path =
      create_rw ~path ~overwrite:true ~auto_flush_threshold:1_000_000
        ~auto_flush_procedure:`Internal
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

  (** [iter_from_node_key node_key _ _ ~f] calls [f] with the key of the node
      and iterates over its children.

      [f k] returns [Follow] or [No_follow], indicating the iteration algorithm
      if the children of [k] should be traversed or skiped. *)
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
      match Pack_key.inspect key with
      | Indexed _ ->
          (* As this is the second time we are reading this key, this case is
             unreachable. *)
          assert false
      | Direct { offset; length; _ } -> (offset, length)
    in
    let buffer = Bytes.create len in
    read_exn ~off ~len buffer;
    let accessor = Dispatcher.create_accessor_to_prefix_exn mapping ~off ~len in
    Bytes.set buffer Hash.hash_size magic_parent;
    (* Bytes.unsafe_to_string usage: We assume read_exn returns unique ownership of buffer
       to this function. Then at the call to Bytes.unsafe_to_string we give up unique
       ownership to buffer (we do not modify it thereafter) in return for ownership of the
       resulting string, which we pass to write_exn. This usage is safe. *)
    write_exn ~off:accessor.poff ~len (Bytes.unsafe_to_string buffer)

  let report_old_file_sizes ~root ~generation stats =
    let open Result_syntax in
    let* prefix_size =
      if generation = 0 then Ok Int63.zero
      else Irmin_pack.Layout.V3.prefix ~root ~generation |> Io.size_of_path
    in
    let+ mapping_size =
      if generation = 0 then Ok Int63.zero
      else Irmin_pack.Layout.V3.mapping ~root ~generation |> Io.size_of_path
    in
    stats := Gc_stats.Worker.add_file_size !stats "old_prefix" prefix_size;
    stats := Gc_stats.Worker.add_file_size !stats "old_mapping" mapping_size

  type suffix_params = {
    start_offset : int63;
    chunk_start_idx : int;
    dead_bytes : int63;
  }
  [@@deriving irmin]

  type gc_results = {
    suffix_params : suffix_params;
    removable_chunk_idxs : int list;
    stats : Stats.Latest_gc.worker;
  }
  [@@deriving irmin]

  type gc_output = (gc_results, Args.Errs.t) result [@@deriving irmin]

  let run ~generation ~new_files_path root commit_key new_suffix_start_offset =
    let open Result_syntax in
    let config =
      Irmin_pack.Conf.init ~fresh:false ~readonly:true ~lru_size:0 root
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

    (* Step 3. Create the new mapping. *)
    let mapping =
      (* Step 3.1 Start [Mapping_file] routine which will create the
         reachable file. *)
      stats := Gc_stats.Worker.finish_current_step !stats "mapping: start";
      let report_file_sizes (reachable_size, sorted_size, mapping_size) =
        stats := Gc_stats.Worker.add_file_size !stats "reachable" reachable_size;
        stats := Gc_stats.Worker.add_file_size !stats "sorted" sorted_size;
        stats := Gc_stats.Worker.add_file_size !stats "mapping" mapping_size
      in
      (fun f ->
        Mapping_file.create ~report_file_sizes ~root:new_files_path ~generation
          ~register_entries:f ()
        |> Errs.raise_if_error)
      @@ fun ~register_entry ->
      (* Step 3.2 Put the commit parents in the reachable file.
         The parent(s) of [commit_key] must be included in the iteration
         because, when decoding the [Commit_value.t] at [commit_key], the
         parents will have to be read in order to produce a key for them. *)
      stats :=
        Gc_stats.Worker.finish_current_step !stats
          "mapping: commits to reachable";
      let register_object_exn key =
        match Pack_key.inspect key with
        | Indexed _ ->
            raise
              (Pack_error (`Commit_parent_key_is_indexed (string_of_key key)))
        | Direct { offset; length; _ } ->
            stats := Gc_stats.Worker.incr_objects_traversed !stats;
            register_entry ~off:offset ~len:length
      in
      List.iter register_object_exn (Commit_value.parents commit);

      (* Step 3.3 Put the commit in the reachable file. *)
      register_object_exn commit_key;

      (* Step 3.4 Put the nodes and contents in the reachable file. *)
      stats :=
        Gc_stats.Worker.finish_current_step !stats
          "mapping: objects to reachable";
      let register_object_exn key =
        match Pack_key.inspect key with
        | Indexed _ ->
            raise
              (Pack_error (`Node_or_contents_key_is_indexed (string_of_key key)))
        | Direct { offset; length; _ } ->
            stats := Gc_stats.Worker.incr_objects_traversed !stats;
            register_entry ~off:offset ~len:length;
            offset
      in
      let node_key = Commit_value.node commit in
      let (_ : int63) = register_object_exn node_key in
      iter node_key node_store ~f:register_object_exn (fun () -> ());

      (* Step 3.5 Return and let the [Mapping_file] routine create the mapping
         file. *)
      stats :=
        Gc_stats.Worker.finish_current_step !stats "mapping: of reachable";
      ()
    in

    let () =
      (* Step 4. Create the new prefix. *)
      stats := Gc_stats.Worker.finish_current_step !stats "prefix: start";
      let prefix =
        let path =
          Irmin_pack.Layout.V4.prefix ~root:new_files_path ~generation
        in
        Ao.create_rw_exn ~path
      in
      let () =
        Errors.finalise_exn (fun _outcome ->
            stats :=
              Gc_stats.Worker.add_file_size !stats "prefix" (Ao.end_poff prefix);
            Ao.close prefix |> Errs.log_if_error "GC: Close prefix")
        @@ fun () ->
        (* Step 5. Transfer to the new prefix, flush and close. *)
        [%log.debug "GC: transfering to the new prefix"];
        stats := Gc_stats.Worker.finish_current_step !stats "prefix: transfer";
        (* Step 5.1. Transfer all. *)
        let append_exn = Ao.append_exn prefix in
        let f ~off ~len =
          let len = Int63.of_int len in
          Dispatcher.read_bytes_exn dispatcher ~f:append_exn ~off ~len
        in
        let () = Mapping_file.iter_exn mapping f in
        Ao.flush prefix |> Errs.raise_if_error
      in
      (* Step 5.2. Transfer again the parent commits but with a modified
         magic. Reopen the new prefix, this time _not_ in append-only
         as we have to modify data inside the file. *)
      stats :=
        Gc_stats.Worker.finish_current_step !stats
          "prefix: rewrite commit parents";
      let read_exn ~off ~len buf =
        let accessor = Dispatcher.create_accessor_exn dispatcher ~off ~len in
        Dispatcher.read_exn dispatcher accessor buf
      in
      let prefix =
        let path =
          Irmin_pack.Layout.V4.prefix ~root:new_files_path ~generation
        in
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

    (* Step 7. Finalise stats and return. *)
    let stats = Gc_stats.Worker.finalise !stats in
    { suffix_params; removable_chunk_idxs; stats }

  let write_gc_output ~root ~generation output =
    let open Result_syntax in
    let path = Irmin_pack.Layout.V4.gc_result ~root ~generation in
    let* io = Io.create ~path ~overwrite:true in
    let out = Irmin.Type.to_json_string gc_output_t output in
    let* () = Io.write_string io ~off:Int63.zero out in
    let* () = Io.fsync io in
    Io.close io

  (* No one catches errors when this function terminates. Write the result in a
     file and terminate. *)
  let run_and_output_result ~generation ~new_files_path root commit_key
      new_suffix_start_offset =
    let result =
      Errs.catch (fun () ->
          run ~generation ~new_files_path root commit_key
            new_suffix_start_offset)
    in
    let write_result = write_gc_output ~root ~generation result in
    write_result |> Errs.log_if_error "writing gc output"
  (* No need to raise or log if [result] is [Error _], we've written it in
     the file. *)
end
