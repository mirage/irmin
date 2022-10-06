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

module Steps_timer = struct
  type duration = Stats.Latest_gc.duration = {
    wall : float;
    sys : float;
    user : float;
  }

  type t = { timer : duration; prev_stepname : string }

  let get_wtime () =
    (Mtime_clock.now () |> Mtime.to_uint64_ns |> Int64.to_float) /. 1e9

  let get_stime () = Rusage.((get Self).stime)
  let get_utime () = Rusage.((get Self).utime)

  let create first_stepname =
    let wall = get_wtime () in
    let sys = get_stime () in
    let user = get_utime () in
    let timer = { wall; sys; user } in
    { timer; prev_stepname = first_stepname }

  let progress prev next_stepname =
    let wall = get_wtime () in
    let sys = get_stime () in
    let user = get_utime () in
    let next = { wall; sys; user } in

    let wall = next.wall -. prev.timer.wall in
    let sys = next.sys -. prev.timer.sys in
    let user = next.user -. prev.timer.user in
    let delta = (prev.prev_stepname, { wall; sys; user }) in

    let next = { timer = next; prev_stepname = next_stepname } in
    (next, delta)
end

module Main_stats : sig
  type t

  val create :
    string ->
    commit_offset:int63 ->
    before_suffix_start_offset:int63 ->
    before_suffix_end_offset:int63 ->
    after_suffix_start_offset:int63 ->
    t

  val finish_current_step : t -> string -> t

  val finalise :
    t ->
    Stats.Latest_gc.worker ->
    after_suffix_end_offset:int63 ->
    Stats.Latest_gc.stats
end = struct
  module S = Stats.Latest_gc

  type t = { stats : S.stats; timer : Steps_timer.t }
  (** [t] is the running state while computing the stats *)

  let create first_stepname ~commit_offset ~before_suffix_start_offset
      ~before_suffix_end_offset ~after_suffix_start_offset =
    let stats = Irmin.Type.(random S.stats_t |> unstage) () in
    (* [repr] provides doesn't provide a generator that fills a type with
       zeroes but it provides a random generator. Let's use it for our initial
       value. *)
    let stats =
      S.
        {
          stats with
          steps = [];
          commit_offset;
          before_suffix_start_offset;
          before_suffix_end_offset;
          after_suffix_start_offset;
        }
    in
    let timer = Steps_timer.create first_stepname in
    { stats; timer }

  let finish_current_step t next_stepname =
    let timer, prev_step = Steps_timer.progress t.timer next_stepname in
    let stats = { t.stats with steps = prev_step :: t.stats.steps } in
    { stats; timer }

  let finalise t worker ~after_suffix_end_offset =
    let t = finish_current_step t "will not appear in the stats" in
    {
      t.stats with
      S.worker;
      after_suffix_end_offset;
      steps = List.rev t.stats.steps;
    }
end

module Worker_stats : sig
  type t

  val create : string -> t
  val incr_objects_traversed : t -> t
  val add_suffix_transfer : t -> int63 -> t
  val add_file_size : t -> string -> int63 -> t
  val finish_current_step : t -> string -> t
  val finalise : t -> Stats.Latest_gc.worker
end = struct
  module S = Stats.Latest_gc

  type t = {
    stats : S.worker;
    current_stepname : string;
    prev_wtime : float;
    prev_stime : float;
    prev_utime : float;
    prev_rusage : S.rusage;
    prev_ocaml_gc : S.ocaml_gc;
  }
  (** [t] is the running state while computing the stats *)

  let is_darwin =
    lazy
      (try
         match Unix.open_process_in "uname" |> input_line with
         | "Darwin" -> true
         | _ -> false
       with Unix.Unix_error _ -> false)

  let get_wtime () =
    (Mtime_clock.now () |> Mtime.to_uint64_ns |> Int64.to_float) /. 1e9

  let get_stime () = Rusage.((get Self).stime)
  let get_utime () = Rusage.((get Self).utime)

  let get_rusage : unit -> S.rusage =
   fun () ->
    let Rusage.{ maxrss; minflt; majflt; inblock; oublock; nvcsw; nivcsw; _ } =
      Rusage.(get Self)
    in
    let maxrss =
      if Lazy.force is_darwin then Int64.div maxrss 1000L else maxrss
    in
    S.{ maxrss; minflt; majflt; inblock; oublock; nvcsw; nivcsw }

  let get_ocaml_gc : unit -> S.ocaml_gc =
   fun () ->
    let open Stdlib.Gc in
    let v = quick_stat () in
    S.
      {
        minor_words = v.minor_words;
        promoted_words = v.promoted_words;
        major_words = v.major_words;
        minor_collections = v.minor_collections;
        major_collections = v.major_collections;
        heap_words = v.heap_words;
        compactions = v.compactions;
        top_heap_words = v.top_heap_words;
        stack_size = v.stack_size;
      }

  let create : string -> t =
   fun first_stepname ->
    (* Reseting all irmin-pack stats. We'll reset again at every step.

       Since the GC worker lives alone in a fork, these global variable mutations
       will not interfere with the rest of the world. *)
    Stats.reset_stats ();
    Irmin_pack.Stats.reset_stats ();

    let wtime = get_wtime () in
    let stime = get_stime () in
    let utime = get_utime () in
    let rusage = get_rusage () in
    let ocaml_gc = get_ocaml_gc () in

    let stats =
      S.
        {
          initial_maxrss = rusage.maxrss;
          initial_heap_words = ocaml_gc.heap_words;
          initial_top_heap_words = ocaml_gc.top_heap_words;
          initial_stack_size = ocaml_gc.stack_size;
          steps = [];
          files = [];
          objects_traversed = Int63.zero;
          suffix_transfers = [];
        }
    in
    {
      stats;
      current_stepname = first_stepname;
      prev_utime = utime;
      prev_wtime = wtime;
      prev_stime = stime;
      prev_rusage = rusage;
      prev_ocaml_gc = ocaml_gc;
    }

  let incr_objects_traversed t =
    let stats =
      { t.stats with objects_traversed = Int63.succ t.stats.objects_traversed }
    in
    { t with stats }

  let add_suffix_transfer t count =
    let stats =
      { t.stats with suffix_transfers = count :: t.stats.suffix_transfers }
    in
    { t with stats }

  let finish_current_step t next_stepname =
    let wtime = get_wtime () in
    let stime = get_stime () in
    let utime = get_utime () in
    let duration =
      let wall = wtime -. t.prev_wtime in
      let sys = stime -. t.prev_stime in
      let user = utime -. t.prev_utime in
      S.{ wall; sys; user }
    in

    let prev_rusage, rusage =
      let x = t.prev_rusage in
      let y = get_rusage () in
      let ( - ) = Int64.sub in
      ( y,
        S.
          {
            y with
            minflt = y.minflt - x.minflt;
            majflt = y.majflt - x.majflt;
            inblock = y.inblock - x.inblock;
            oublock = y.oublock - x.oublock;
            nvcsw = y.nvcsw - x.nvcsw;
            nivcsw = y.nivcsw - x.nivcsw;
          } )
    in
    let prev_ocaml_gc, ocaml_gc =
      let x = t.prev_ocaml_gc in
      let y = get_ocaml_gc () in
      ( y,
        S.
          {
            y with
            minor_words = y.minor_words -. x.minor_words;
            promoted_words = y.promoted_words -. x.promoted_words;
            major_words = y.major_words -. x.major_words;
            minor_collections = y.minor_collections - x.minor_collections;
            major_collections = y.major_collections - x.major_collections;
            compactions = y.compactions - x.compactions;
          } )
    in

    (* [clone] duplicates a value. Used below to snapshot mutable values. *)
    let clone typerepr v =
      match
        Irmin.Type.to_string typerepr v |> Irmin.Type.of_string typerepr
      with
      | Error _ -> assert false
      | Ok v -> v
    in
    let pack_store =
      Stats.((get ()).pack_store |> Pack_store.export |> clone Pack_store.t)
    in
    Stats.report_index ();
    let index = Stats.((get ()).index |> Index.export |> clone Index.t) in
    let inode =
      Irmin_pack.Stats.((get ()).inode |> Inode.export |> clone Inode.t)
    in
    Stats.reset_stats ();
    Irmin_pack.Stats.reset_stats ();

    let step = S.{ duration; rusage; ocaml_gc; index; pack_store; inode } in

    (* The [steps] list is built in reverse order and reversed in [finalise] *)
    let steps = (t.current_stepname, step) :: t.stats.steps in

    let stats = { t.stats with steps } in
    {
      current_stepname = next_stepname;
      stats;
      prev_wtime = wtime;
      prev_stime = stime;
      prev_utime = utime;
      prev_rusage;
      prev_ocaml_gc;
    }

  let add_file_size t file_name size =
    let stats = { t.stats with files = (file_name, size) :: t.stats.files } in
    { t with stats }

  let finalise : t -> S.worker =
   fun t ->
    let t = finish_current_step t "will not appear in the stats" in
    {
      t.stats with
      steps = List.rev t.stats.steps;
      suffix_transfers = List.rev t.stats.suffix_transfers;
    }
end

(** Code for running in an async GC worker thread. *)
module Worker = struct
  module Payload = Control_file.Latest_payload

  let buffer_size = 8192

  exception Pack_error = Errors.Pack_error

  module type S = sig
    module Args : Args

    val run_and_output_result : generation:int -> string -> Args.key -> unit

    val transfer_append_exn :
      dispatcher:Args.Dispatcher.t ->
      append_exn:(string -> unit) ->
      off:int63 ->
      len:int63 ->
      bytes ->
      unit

    type gc_output = (Stats.Latest_gc.worker, Args.Errs.t) result
    [@@deriving irmin]
  end

  module Make (Args : Args) : S with module Args := Args = struct
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

    let transfer_append_exn ~dispatcher ~append_exn ~(off : int63)
        ~(len : int63) buffer =
      let read_exn = Dispatcher.read_in_prefix_and_suffix_exn dispatcher in
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
        match Pack_key.inspect key with
        | Indexed _ ->
            (* As this is the second time we are reading this key, this case is
               unreachable. *)
            assert false
        | Direct { offset; length; _ } -> (offset, length)
      in
      let buffer = Bytes.create len in
      read_exn ~off ~len buffer;
      let accessor =
        Dispatcher.create_accessor_to_prefix_exn mapping ~off ~len
      in
      Bytes.set buffer Hash.hash_size magic_parent;
      (* Bytes.unsafe_to_string usage: We assume read_exn returns unique ownership of buffer
         to this function. Then at the call to Bytes.unsafe_to_string we give up unique
         ownership to buffer (we do not modify it thereafter) in return for ownership of the
         resulting string, which we pass to write_exn. This usage is safe. *)
      write_exn ~off:accessor.poff ~len (Bytes.unsafe_to_string buffer)

    let create_new_suffix ~root ~generation =
      let path = Irmin_pack.Layout.V3.suffix ~root ~generation in
      Ao.create_rw_exn ~path

    let run ~generation root commit_key =
      let open Result_syntax in
      let config =
        Irmin_pack.Conf.init ~fresh:false ~readonly:true ~lru_size:0 root
      in

      (* Step 1. Open the files *)
      [%log.debug "GC: opening files in RO mode"];
      let stats = ref (Worker_stats.create "open files") in
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
      stats := Worker_stats.finish_current_step !stats "load commit";
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
        let state : _ Pack_key.state = Pack_key.inspect commit_key in
        match state with
        | Indexed _ -> assert false
        | Direct x -> (x.offset, x.length)
      in

      (* Step 3. Create the new mapping. *)
      let mapping =
        (* Step 3.1 Start [Mapping_file] routine which will create the
           reachable file. *)
        stats := Worker_stats.finish_current_step !stats "mapping: start";
        let report_file_sizes (reachable_size, sorted_size, mapping_size) =
          stats := Worker_stats.add_file_size !stats "reachable" reachable_size;
          stats := Worker_stats.add_file_size !stats "sorted" sorted_size;
          stats := Worker_stats.add_file_size !stats "mapping" mapping_size
        in
        (fun f ->
          Mapping_file.create ~report_file_sizes ~root ~generation
            ~register_entries:f ()
          |> Errs.raise_if_error)
        @@ fun ~register_entry ->
        (* Step 3.2 Put the commit parents in the reachable file.
           The parent(s) of [commit_key] must be included in the iteration
           because, when decoding the [Commit_value.t] at [commit_key], the
           parents will have to be read in order to produce a key for them. *)
        stats :=
          Worker_stats.finish_current_step !stats "mapping: commits to sorted";
        let register_object_exn key =
          match Pack_key.inspect key with
          | Indexed _ ->
              raise
                (Pack_error (`Commit_parent_key_is_indexed (string_of_key key)))
          | Direct { offset; length; _ } ->
              stats := Worker_stats.incr_objects_traversed !stats;
              register_entry ~off:offset ~len:length
        in
        List.iter register_object_exn (Commit_value.parents commit);

        (* Step 3.3 Put the nodes and contents in the reachable file. *)
        stats :=
          Worker_stats.finish_current_step !stats "mapping: objects to sorted";
        let register_object_exn key =
          match Pack_key.inspect key with
          | Indexed _ ->
              raise
                (Pack_error
                   (`Node_or_contents_key_is_indexed (string_of_key key)))
          | Direct { offset; length; _ } ->
              stats := Worker_stats.incr_objects_traversed !stats;
              register_entry ~off:offset ~len:length;
              offset
        in
        let node_key = Commit_value.node commit in
        let (_ : int63) = register_object_exn node_key in
        iter node_key node_store ~f:register_object_exn (fun () -> ());

        (* Step 3.4 Return and let the [Mapping_file] routine create the mapping
           file. *)
        stats := Worker_stats.finish_current_step !stats "mapping: of sorted";
        ()
      in

      let () =
        (* Step 4. Create the new prefix. *)
        stats := Worker_stats.finish_current_step !stats "prefix: start";
        let prefix =
          let path = Irmin_pack.Layout.V3.prefix ~root ~generation in
          Ao.create_rw_exn ~path
        in
        let () =
          Errors.finalise_exn (fun _outcome ->
              stats :=
                Worker_stats.add_file_size !stats "prefix" (Ao.end_poff prefix);
              Ao.close prefix |> Errs.log_if_error "GC: Close prefix")
          @@ fun () ->
          ();

          (* Step 5. Transfer to the new prefix, flush and close. *)
          [%log.debug "GC: transfering to the new prefix"];
          stats := Worker_stats.finish_current_step !stats "prefix: transfer";
          let buffer = Bytes.create buffer_size in
          (* Step 5.1. Transfer all. *)
          let append_exn = Ao.append_exn prefix in
          let f ~off ~len =
            let len = Int63.of_int len in
            transfer_append_exn ~dispatcher ~append_exn ~off ~len buffer
          in
          let () = Mapping_file.iter_exn mapping f in
          Ao.flush prefix |> Errs.raise_if_error
        in
        (* Step 5.2. Transfer again the parent commits but with a modified
           magic. Reopen the new prefix, this time _not_ in append-only
           as we have to modify data inside the file. *)
        stats :=
          Worker_stats.finish_current_step !stats
            "prefix: rewrite commit parents";
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
      stats := Worker_stats.finish_current_step !stats "suffix: start";
      let buffer = Bytes.create buffer_size in
      [%log.debug "GC: creating new suffix"];
      let suffix = create_new_suffix ~root ~generation in
      Errors.finalise_exn (fun _outcome ->
          Ao.fsync suffix
          >>= (fun _ -> Ao.close suffix)
          |> Errs.log_if_error "GC: Close suffix")
      @@ fun () ->
      let append_exn = Ao.append_exn suffix in
      let transfer_exn = transfer_append_exn ~dispatcher ~append_exn buffer in

      (* Step 7. Transfer to the next suffix. *)
      [%log.debug "GC: transfering to the new suffix"];
      stats := Worker_stats.finish_current_step !stats "suffix: transfer";
      let num_iterations = 7 in
      (* [transfer_loop] is needed because after garbage collection there may be new objects
         at the end of the suffix file that need to be copied over *)
      let rec transfer_loop i ~off =
        if i = 0 then off
        else
          let () = Fm.reload fm |> Errs.raise_if_error in
          let pl : Payload.t = Fm.Control.payload (Fm.control fm) in
          let end_offset =
            Dispatcher.offset_of_suffix_poff dispatcher pl.suffix_end_poff
          in
          let len = Int63.Syntax.(end_offset - off) in
          [%log.debug
            "GC: transfer_loop iteration %d, offset %a, length %a"
              (num_iterations - i + 1)
              Int63.pp off Int63.pp len];
          stats := Worker_stats.add_suffix_transfer !stats len;
          let () = transfer_exn ~off ~len in
          (* Check how many bytes are left, [4096*5] is selected because it is roughly the
             number of bytes that requires a read from the block device on ext4 *)
          if Int63.to_int len < 4096 * 5 then end_offset
          else
            let off = Int63.Syntax.(off + len) in
            transfer_loop ~off (i - 1)
      in
      let new_end_suffix_offset =
        transfer_loop ~off:commit_offset num_iterations
      in
      stats := Worker_stats.add_file_size !stats "suffix" new_end_suffix_offset;
      Ao.flush suffix |> Errs.raise_if_error;

      (* Step 8. Finalise stats and return. *)
      Worker_stats.finalise !stats

    type gc_output = (Stats.Latest_gc.worker, Args.Errs.t) result
    [@@deriving irmin]

    let write_gc_output ~root ~generation output =
      let open Result_syntax in
      let path = Irmin_pack.Layout.V3.gc_result ~root ~generation in
      let* io = Io.create ~path ~overwrite:true in
      let out = Irmin.Type.to_json_string gc_output_t output in
      let* () = Io.write_string io ~off:Int63.zero out in
      let* () = Io.fsync io in
      Io.close io

    (* No one catches errors when this function terminates. Write the result in a
       file and terminate. *)
    let run_and_output_result ~generation root commit_key =
      let result = Errs.catch (fun () -> run ~generation root commit_key) in
      let write_result = write_gc_output ~root ~generation result in
      write_result |> Errs.log_if_error "writing gc output"
    (* No need to raise or log if [result] is [Error _], we've written it in
       the file. *)
  end
end

(** Maker for a module that can manage GC processes. *)
module Make (Args : Args) = struct
  module Args = Args
  open Args
  module Io = Fm.Io
  module Ao = Append_only_file.Make (Io) (Errs)
  module Worker = Worker.Make (Args)

  type t = {
    root : string;
    generation : int;
    task : Async.t;
    unlink : bool;
    offset : int63;
    resolver : (Stats.Latest_gc.stats, Errs.t) result Lwt.u;
    promise : (Stats.Latest_gc.stats, Errs.t) result Lwt.t;
    dispatcher : Dispatcher.t;
    fm : Fm.t;
    contents : read Contents_store.t;
    node : read Node_store.t;
    commit : read Commit_store.t;
    mutable partial_stats : Main_stats.t;
    mutable resulting_stats : Stats.Latest_gc.stats option;
  }

  let v ~root ~generation ~unlink ~offset ~dispatcher ~fm ~contents ~node
      ~commit commit_key =
    let partial_stats =
      let commit_offset = offset in
      let before_suffix_start_offset =
        Dispatcher.suffix_start_offset dispatcher
      in
      let before_suffix_end_offset = Dispatcher.end_offset dispatcher in
      let after_suffix_start_offset =
        offset
        (* This will not just be [offset] anymore when the commit is moved the
           the prefix file. *)
      in
      Main_stats.create "worker startup" ~commit_offset
        ~before_suffix_start_offset ~before_suffix_end_offset
        ~after_suffix_start_offset
    in
    let unlink_result_file () =
      let result_file = Irmin_pack.Layout.V3.gc_result ~root ~generation in
      match Io.unlink result_file with
      | Ok () -> ()
      | Error (`Sys_error msg as err) ->
          if msg <> Fmt.str "%s: No such file or directory" result_file then
            [%log.warn
              "Unlinking temporary files from previous failed gc. Failed with \
               error %a"
              (Irmin.Type.pp Errs.t) err]
    in
    (* Unlink next gc's result file, in case it is on disk, for instance
       after a failed gc. *)
    unlink_result_file ();
    (* function to track durations *)
    (* internal promise for gc *)
    let promise, resolver = Lwt.wait () in
    (* start worker task *)
    let task =
      Async.async (fun () ->
          Worker.run_and_output_result root commit_key ~generation)
    in
    let partial_stats =
      Main_stats.finish_current_step partial_stats "before finalise"
    in
    {
      root;
      generation;
      unlink;
      offset;
      task;
      promise;
      resolver;
      dispatcher;
      fm;
      contents;
      node;
      commit;
      partial_stats;
      resulting_stats = None;
    }

  let open_new_suffix ~end_poff { root; generation; _ } =
    let open Result_syntax in
    let path = Irmin_pack.Layout.V3.suffix ~root ~generation in
    (* As the new suffix is necessarily in V3, the dead_header_size is
       0. *)
    let dead_header_size = 0 in
    let auto_flush_threshold = 1_000_000 in
    let* suffix =
      Ao.open_rw ~path ~end_poff ~dead_header_size
        ~auto_flush_procedure:`Internal ~auto_flush_threshold
    in
    Ok suffix

  let transfer_latest_newies ~new_suffix_start_offset ~new_suffix_end_offset t =
    [%log.debug "Gc in main: transfer latest newies"];
    let open Result_syntax in
    let open Int63.Syntax in
    let old_suffix_end_offset = Dispatcher.end_offset t.dispatcher in
    let remaining = old_suffix_end_offset - new_suffix_end_offset in
    (* When opening the suffix we need to provide a physical offset. We compute
       it from the global ones. *)
    let suffix_end_poff = new_suffix_end_offset - new_suffix_start_offset in
    let* new_suffix = open_new_suffix ~end_poff:suffix_end_poff t in
    Errors.finalise (fun _ ->
        Ao.close new_suffix
        |> Errs.log_if_error "GC: Close suffix after copy latest newies")
    @@ fun () ->
    let buffer = Bytes.create 8192 in
    let append_exn = Ao.append_exn new_suffix in
    let flush_and_raise () = Ao.flush new_suffix |> Errs.raise_if_error in
    let* () =
      Errs.catch (fun () ->
          Worker.transfer_append_exn ~dispatcher:t.dispatcher ~append_exn
            ~off:new_suffix_end_offset ~len:remaining buffer;
          flush_and_raise ())
    in
    Ok old_suffix_end_offset

  let swap_and_purge ~new_suffix_start_offset ~new_suffix_end_offset t =
    let open Result_syntax in
    let* () =
      Fm.swap t.fm ~generation:t.generation ~new_suffix_start_offset
        ~new_suffix_end_offset
    in

    (* No need to purge dict here, as it is global to the store. *)
    (* No need to purge index here. It is global too, but some hashes may
       not point to valid offsets anymore. Pack_store will just say that
       such keys are not member of the store. *)
    Contents_store.purge_lru t.contents;
    Node_store.purge_lru t.node;
    Commit_store.purge_lru t.commit;
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
          "Unlinking temporary files after gc, failed with error %a"
            (Irmin.Type.pp Errs.t) e]
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

  let read_gc_output ~root ~generation =
    let open Result_syntax in
    let read_file () =
      let path = Irmin_pack.Layout.V3.gc_result ~root ~generation in
      let* io = Io.open_ ~path ~readonly:true in
      let* len = Io.read_size io in
      let len = Int63.to_int len in
      let* string = Io.read_to_string io ~off:Int63.zero ~len in
      let* () = Io.close io in
      Ok string
    in
    let read_error err =
      `Corrupted_gc_result_file (Irmin.Type.to_string Errs.t err)
    in
    let gc_error err = `Gc_process_error (Irmin.Type.to_string Errs.t err) in
    let* s = read_file () |> Result.map_error read_error in
    match Irmin.Type.of_json_string Worker.gc_output_t s with
    | Error (`Msg error) -> Error (`Corrupted_gc_result_file error)
    | Ok ok -> ok |> Result.map_error gc_error

  let clean_after_abort t =
    Fm.cleanup ~root:t.root ~generation:(t.generation - 1)

  let finalise ~wait t =
    match t.resulting_stats with
    | Some partial_stats -> Lwt.return_ok (`Finalised partial_stats)
    | None -> (
        let partial_stats = t.partial_stats in
        let partial_stats =
          Main_stats.finish_current_step partial_stats "worker wait"
        in
        let go status =
          let partial_stats =
            Main_stats.finish_current_step partial_stats "read output"
          in

          let gc_output =
            read_gc_output ~root:t.root ~generation:t.generation
          in

          let result =
            let open Result_syntax in
            match (status, gc_output) with
            | `Success, Ok worker_stats ->
                let new_suffix_end_offset_before =
                  Stats.Latest_gc.new_suffix_end_offset_before_finalise
                    worker_stats
                in
                let partial_stats =
                  Main_stats.finish_current_step partial_stats
                    "copy latest newies"
                in
                let* new_suffix_end_offset =
                  transfer_latest_newies ~new_suffix_start_offset:t.offset
                    ~new_suffix_end_offset:new_suffix_end_offset_before t
                in
                let partial_stats =
                  Main_stats.finish_current_step partial_stats "swap and purge"
                in
                let* () =
                  swap_and_purge ~new_suffix_start_offset:t.offset
                    ~new_suffix_end_offset t
                in
                let partial_stats =
                  Main_stats.finish_current_step partial_stats "unlink"
                in
                if t.unlink then unlink_all t;

                let partial_stats =
                  let after_suffix_end_offset =
                    Dispatcher.end_offset t.dispatcher
                  in
                  Main_stats.finalise partial_stats worker_stats
                    ~after_suffix_end_offset
                in
                t.resulting_stats <- Some partial_stats;

                [%log.debug
                  "Gc ended successfully. %a"
                    (Irmin.Type.pp Stats.Latest_gc.stats_t)
                    partial_stats];
                let () = Lwt.wakeup_later t.resolver (Ok partial_stats) in
                Ok (`Finalised partial_stats)
            | _ ->
                clean_after_abort t;
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
          | #Async.outcome as status -> go status)

  let on_finalise t f =
    (* Ignore returned promise since the purpose of this
       function is to add asynchronous callbacks to the GC
       process -- this promise binding is an internal
       implementation detail. This is safe since the callback
       [f] is attached to [t.running_gc.promise], which is
       referenced for the lifetime of a GC process. *)
    let _ = Lwt.bind t.promise f in
    ()

  let cancel t =
    let cancelled = Async.cancel t.task in
    if cancelled then clean_after_abort t;
    cancelled
end
