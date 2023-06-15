(*
 * Copyright (c) 2022-2023 Tarides <contact@tarides.com>
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

(** Maker for a module that can manage GC processes. *)
module Make (Args : Gc_args.S) = struct
  module Args = Args
  open Args
  module Io = Fm.Io
  module Ao = Append_only_file.Make (Io) (Errs)
  module Worker = Gc_worker.Make (Args)

  type t = {
    root : string;
    generation : int;
    task : Async.t;
    unlink : bool;
    new_suffix_start_offset : int63;
    resolver : (Stats.Latest_gc.stats, Errs.t) result Lwt.u;
    promise : (Stats.Latest_gc.stats, Errs.t) result Lwt.t;
    dispatcher : Dispatcher.t;
    fm : Fm.t;
    contents : read Contents_store.t;
    node : read Node_store.t;
    commit : read Commit_store.t;
    mutable partial_stats : Gc_stats.Main.t;
    mutable resulting_stats : Stats.Latest_gc.stats option;
    latest_gc_target_offset : int63;
  }

  let v ~root ~lower_root ~output ~generation ~unlink ~dispatcher ~fm ~contents
      ~node ~commit commit_key =
    let open Result_syntax in
    let new_suffix_start_offset, latest_gc_target_offset =
      let state : _ Pack_key.state = Pack_key.inspect commit_key in
      match state with
      | Direct x ->
          let len = x.length |> Int63.of_int in
          (Int63.Syntax.(x.offset + len), x.offset)
      | Indexed _ ->
          (* The caller of this function lifted the key to a direct one. *)
          assert false
    in
    let status = Fm.control fm |> Fm.Control.payload |> fun p -> p.status in
    (* Ensure we are calling GC on a commit strictly newer than last GC commit
       Only checking when the output is the root (it is not a snapshot export) *)
    let* () =
      match (output, status) with
      | `Root, Gced previous
        when Int63.Syntax.(
               previous.latest_gc_target_offset >= latest_gc_target_offset) ->
          Error
            (`Gc_disallowed
              (Fmt.str "%a is less than or equal to previous GC offset of %a"
                 Int63.pp latest_gc_target_offset Int63.pp
                 previous.latest_gc_target_offset))
      | _ -> Ok ()
    in
    let new_files_path =
      match output with `Root -> root | `External path -> path
    in
    (* Since we can call GC on commits in the lower, ensure we do not provide a
       [new_suffix_start_offset] that is older than our current starting offset. *)
    let new_suffix_start_offset =
      match status with
      | Gced { suffix_start_offset; _ }
        when Int63.Syntax.(suffix_start_offset >= latest_gc_target_offset) ->
          suffix_start_offset
      | _ -> new_suffix_start_offset
    in
    let partial_stats =
      let commit_offset = latest_gc_target_offset in
      let before_suffix_start_offset =
        Dispatcher.suffix_start_offset dispatcher
      in
      let before_suffix_end_offset = Dispatcher.end_offset dispatcher in
      Gc_stats.Main.create "worker startup" ~commit_offset ~generation
        ~before_suffix_start_offset ~before_suffix_end_offset
        ~after_suffix_start_offset:new_suffix_start_offset
    in
    let unlink_result_file () =
      let result_file = Irmin_pack.Layout.V4.gc_result ~root ~generation in
      match Io.classify_path result_file with
      | `File ->
          Io.unlink_dont_wait
            ~on_exn:(fun exn ->
              [%log.warn
                "Unlinking temporary files from previous failed gc. Failed \
                 with error %s"
                (Printexc.to_string exn)])
            result_file
      | `Directory | `No_such_file_or_directory | `Other -> ()
    in
    (* Unlink next gc's result file, in case it is on disk, for instance
       after a failed gc. *)
    unlink_result_file ();
    (* internal promise for gc *)
    let promise, resolver = Lwt.wait () in
    (* start worker task *)
    let task =
      Async.async (fun () ->
          Worker.run_and_output_result root commit_key new_suffix_start_offset
            ~lower_root ~generation ~new_files_path)
    in
    let partial_stats =
      Gc_stats.Main.finish_current_step partial_stats "before finalise"
    in
    Ok
      {
        root;
        generation;
        unlink;
        new_suffix_start_offset;
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
        latest_gc_target_offset;
      }

  let swap_and_purge t (gc_results : Worker.gc_results) =
    let removable_chunk_num = List.length gc_results.removable_chunk_idxs in
    let { generation; latest_gc_target_offset; _ } = t in
    let Worker.
          {
            start_offset = suffix_start_offset;
            chunk_start_idx;
            dead_bytes = suffix_dead_bytes;
          } =
      gc_results.suffix_params
    in
    (* Calculate chunk num in main process since more chunks could have been
       added while GC was running. GC process only tells us how many chunks are
       to be removed. *)
    let suffix = Fm.suffix t.fm in
    let chunk_num = Fm.Suffix.chunk_num suffix - removable_chunk_num in
    (* Assert that we have at least one chunk (the appendable chunk), which
       is guaranteed by the GC process. *)
    assert (chunk_num >= 1);

    Fm.swap t.fm ~generation ~mapping_size:gc_results.mapping_size
      ~suffix_start_offset ~chunk_start_idx ~chunk_num ~suffix_dead_bytes
      ~latest_gc_target_offset ~volume:gc_results.modified_volume

  let unlink_all { root; generation; _ } removable_chunk_idxs =
    (* Unlink suffix chunks *)
    let () =
      removable_chunk_idxs
      |> List.iter (fun chunk_idx ->
             let path = Irmin_pack.Layout.V4.suffix_chunk ~root ~chunk_idx in
             Io.unlink_dont_wait
               ~on_exn:(fun exn ->
                 [%log.warn
                   "Unlinking chunk_idxs files after gc, failed with error %s"
                     (Printexc.to_string exn)])
               path)
    in
    if generation >= 2 then (
      (* Unlink previous prefix. *)
      let prefix =
        Irmin_pack.Layout.V4.prefix ~root ~generation:(generation - 1)
      in
      Io.unlink_dont_wait
        ~on_exn:(fun exn ->
          [%log.warn
            "Unlinking previous prefix after gc, failed with error %s"
              (Printexc.to_string exn)])
        prefix;

      (* Unlink previous mapping. *)
      let mapping =
        Irmin_pack.Layout.V4.mapping ~root ~generation:(generation - 1)
      in
      Io.unlink_dont_wait
        ~on_exn:(fun exn ->
          [%log.warn
            "Unlinking previous mapping after gc, failed with error %s"
              (Printexc.to_string exn)])
        mapping);

    (* Unlink current gc's result.*)
    let result = Irmin_pack.Layout.V4.gc_result ~root ~generation in
    Io.unlink_dont_wait
      ~on_exn:(fun exn ->
        [%log.warn
          "Unlinking current gc's result after gc, failed with error %s"
            (Printexc.to_string exn)])
      result

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
      let path = Irmin_pack.Layout.V4.gc_result ~root ~generation in
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
    Fm.cleanup t.fm |> Errs.log_if_error "clean_after_abort"

  let finalise ~wait t =
    match t.resulting_stats with
    | Some partial_stats -> Lwt.return_ok (`Finalised partial_stats)
    | None -> (
        let partial_stats = t.partial_stats in
        let partial_stats =
          Gc_stats.Main.finish_current_step partial_stats "worker wait"
        in
        let go status =
          let partial_stats =
            Gc_stats.Main.finish_current_step partial_stats "read output"
          in

          let gc_output =
            read_gc_output ~root:t.root ~generation:t.generation
          in

          let result =
            let open Result_syntax in
            match (status, gc_output) with
            | `Success, Ok gc_results ->
                let partial_stats =
                  Gc_stats.Main.finish_current_step partial_stats
                    "swap and purge"
                in
                let* () = swap_and_purge t gc_results in
                let partial_stats =
                  Gc_stats.Main.finish_current_step partial_stats "unlink"
                in
                if t.unlink then unlink_all t gc_results.removable_chunk_idxs;

                let stats =
                  let after_suffix_end_offset =
                    Dispatcher.end_offset t.dispatcher
                  in
                  Gc_stats.Main.finalise partial_stats gc_results.stats
                    ~after_suffix_end_offset
                in
                Stats.report_latest_gc stats;
                t.resulting_stats <- Some stats;

                [%log.debug
                  "Gc ended successfully. %a"
                    (Irmin.Type.pp Stats.Latest_gc.stats_t)
                    stats];
                let () = Lwt.wakeup_later t.resolver (Ok stats) in
                Ok (`Finalised stats)
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

  let finalise_without_swap t =
    let* status = Async.await t.task in
    let gc_output = read_gc_output ~root:t.root ~generation:t.generation in
    match (status, gc_output) with
    | `Success, Ok gc_results ->
        Lwt.return
          {
            Control_file_intf.Payload.Upper.Latest.generation =
              Fm.generation t.fm + 1;
            latest_gc_target_offset = t.latest_gc_target_offset;
            suffix_start_offset = t.new_suffix_start_offset;
            suffix_dead_bytes = Int63.zero;
            mapping_end_poff = Some gc_results.mapping_size;
          }
    | _ ->
        let r = gc_errors status gc_output |> Errs.raise_if_error in
        Lwt.return r

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
