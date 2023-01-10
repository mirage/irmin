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

open Import
module Payload = Control_file.Payload.Upper.Latest
include File_manager_intf

let legacy_io_header_size = 16

module Make
    (Io : Io.S)
    (Index : Pack_index.S with module Io = Io)
    (Errs : Io_errors.S with module Io = Io) =
struct
  module Io = Errs.Io
  module Index = Index
  module Errs = Io_errors.Make (Io)
  module Control = Control_file.Upper (Io)
  module Dict = Append_only_file.Make (Io) (Errs)
  module Suffix = Chunked_suffix.Make (Io) (Errs)
  module Sparse = Sparse_file.Make (Io)
  module Lower = Lower.Make (Io) (Errs)

  type after_reload_consumer = { after_reload : unit -> (unit, Errs.t) result }
  type after_flush_consumer = { after_flush : unit -> unit }

  type t = {
    dict : Dict.t;
    control : Control.t;
    mutable suffix : Suffix.t;
    mutable prefix : Sparse.t option;
    lower : Lower.t option;
    index : Index.t;
    mutable dict_consumers : after_reload_consumer list;
    mutable prefix_consumers : after_reload_consumer list;
    mutable suffix_consumers : after_flush_consumer list;
    indexing_strategy : Irmin_pack.Indexing_strategy.t;
    use_fsync : bool;
    root : string;
  }

  let control t = t.control
  let dict t = t.dict
  let suffix t = t.suffix
  let index t = t.index
  let prefix t = t.prefix
  let lower t = t.lower

  let close t =
    let open Result_syntax in
    let* () = Dict.close t.dict in
    let* () = Control.close t.control in
    let* () = Suffix.close t.suffix in
    let* () = Option.might Sparse.close t.prefix in
    let+ () = Index.close t.index in
    ()

  let register_dict_consumer t ~after_reload =
    t.dict_consumers <- { after_reload } :: t.dict_consumers

  let register_prefix_consumer t ~after_reload =
    t.prefix_consumers <- { after_reload } :: t.prefix_consumers

  let register_suffix_consumer t ~after_flush =
    t.suffix_consumers <- { after_flush } :: t.suffix_consumers

  let get_gced = function Payload.Gced x -> Some x | _ -> None

  let generation payload =
    match get_gced payload with Some x -> x.generation | None -> 0

  let mapping_size payload =
    match get_gced payload with Some x -> x.mapping_end_poff | None -> None

  let notify_reload_consumers consumers =
    List.fold_left
      (fun acc { after_reload } -> Result.bind acc after_reload)
      (Ok ()) consumers
    |> Result.map_error (fun err -> (err : Errs.t :> [> Errs.t ]))

  (** Flush stages *************************************************************

      The irmin-pack files are only mutated during calls to one of the 3
      following functions. Exceptions:

      - During [create] and [open_rw].
      - During a GC.
      - When the branch store is modified. *)

  (** Flush stage 1 *)
  let flush_dict t =
    let open Result_syntax in
    if Dict.empty_buffer t.dict then Ok ()
    else
      let* () =
        Stats.incr_fm_field Dict_flushes;
        Dict.flush t.dict
      in
      let* () = if t.use_fsync then Dict.fsync t.dict else Ok () in
      let* () =
        let pl : Payload.t = Control.payload t.control in
        let pl = { pl with dict_end_poff = Dict.end_poff t.dict } in
        Control.set_payload t.control pl
      in
      let+ () = if t.use_fsync then Control.fsync t.control else Ok () in
      ()

  (** Flush stage 2 *)
  let flush_suffix_and_its_deps ?hook t =
    let open Result_syntax in
    let* () = flush_dict t in
    (match hook with Some h -> h `After_dict | None -> ());
    if Suffix.empty_buffer t.suffix then Ok ()
    else
      let* () =
        Stats.incr_fm_field Suffix_flushes;
        Suffix.flush t.suffix
      in
      let* () = if t.use_fsync then Suffix.fsync t.suffix else Ok () in
      let* () =
        let pl : Payload.t = Control.payload t.control in
        let status =
          match pl.status with
          | From_v1_v2_post_upgrade _ -> pl.status
          | Gced _ -> pl.status
          | No_gc_yet ->
              if Irmin_pack.Indexing_strategy.is_minimal t.indexing_strategy
              then pl.status
              else (
                [%log.warn
                  "Updating the control file to \
                   [Used_non_minimal_indexing_strategy]. It won't be possible \
                   to GC this irmin-pack store anymore."];
                Payload.Used_non_minimal_indexing_strategy)
          | Used_non_minimal_indexing_strategy -> pl.status
          | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13
          | T14 | T15 ->
              assert false
        in
        let pl =
          {
            pl with
            appendable_chunk_poff = Suffix.appendable_chunk_poff t.suffix;
            status;
          }
        in
        Control.set_payload t.control pl
      in
      let+ () = if t.use_fsync then Control.fsync t.control else Ok () in
      List.iter (fun { after_flush } -> after_flush ()) t.suffix_consumers

  (** Flush stage 3 *)
  let flush_index_and_its_deps ?hook t =
    let open Result_syntax in
    let* () = flush_suffix_and_its_deps ?hook t in
    (match hook with Some h -> h `After_suffix | None -> ());
    let+ () =
      Stats.incr_fm_field Index_flushes;
      Index.flush ~with_fsync:t.use_fsync t.index
    in
    ()

  (* Auto flushes *********************************************************** *)

  (** Is expected to be called by the dict when its append buffer is full so
      that the file manager flushes. *)
  let dict_requires_a_flush_exn t =
    Stats.incr_fm_field Auto_dict;
    flush_dict t |> Errs.raise_if_error

  (** Is expected to be called by the suffix when its append buffer is full so
      that the file manager flushes. *)
  let suffix_requires_a_flush_exn t =
    Stats.incr_fm_field Auto_suffix;
    flush_suffix_and_its_deps t |> Errs.raise_if_error

  (** Is expected to be called by the index when its append buffer is full so
      that the dependendies of index are flushes. When the function returns,
      index will flush itself. *)
  let index_is_about_to_auto_flush_exn t =
    Stats.incr_fm_field Auto_index;
    flush_suffix_and_its_deps t |> Errs.raise_if_error

  (* Explicit flush ********************************************************* *)

  let flush ?hook t =
    Stats.incr_fm_field Flush;
    flush_index_and_its_deps ?hook t

  (* Explicit fsync ********************************************************* *)

  let fsync t =
    let open Result_syntax in
    let* () = Dict.fsync t.dict in
    let* () = Suffix.fsync t.suffix in
    let* () = Control.fsync t.control in
    Index.flush ~with_fsync:true t.index

  (* Constructors *********************************************************** *)

  module Layout = Irmin_pack.Layout.V5

  let open_prefix ~root ~generation ~mapping_size =
    let open Result_syntax in
    if generation = 0 then Ok None
    else
      let mapping = Layout.mapping ~generation ~root in
      let data = Layout.prefix ~root ~generation in
      let* mapping_size =
        match mapping_size with
        | Some size -> Ok size
        | None -> Io.size_of_path mapping
      in
      let mapping_size = Int63.to_int mapping_size in
      let+ prefix = Sparse.open_ro ~mapping_size ~mapping ~data in
      Some prefix

  let reopen_prefix t ~generation ~mapping_size =
    let open Result_syntax in
    let* some_prefix = open_prefix ~root:t.root ~generation ~mapping_size in
    match some_prefix with
    | None -> Ok ()
    | Some _ ->
        let prev_prefix = t.prefix in
        t.prefix <- some_prefix;
        let* () = notify_reload_consumers t.prefix_consumers in
        Option.might Sparse.close prev_prefix

  let reopen_suffix t ~chunk_start_idx ~chunk_num ~appendable_chunk_poff =
    let open Result_syntax in
    (* Invariant: reopen suffix is only called on V3 (and above) suffix files,
       for which dead_header_size is 0. *)
    let dead_header_size = 0 in
    [%log.debug
      "reopen_suffix chunk_start_idx:%d chunk_num:%d appendable_chunk_poff:%d"
        chunk_start_idx chunk_num
        (Int63.to_int appendable_chunk_poff)];
    let readonly = Suffix.readonly t.suffix in
    let* suffix1 =
      let root = t.root in
      let start_idx = chunk_start_idx in
      [%log.debug "reload: generation changed, opening suffix"];
      if readonly then
        Suffix.open_ro ~root ~appendable_chunk_poff ~dead_header_size ~start_idx
          ~chunk_num
      else
        let auto_flush_threshold =
          match Suffix.auto_flush_threshold t.suffix with
          | None -> assert false
          | Some x -> x
        in
        let cb _ = suffix_requires_a_flush_exn t in
        Suffix.open_rw ~root ~appendable_chunk_poff ~dead_header_size ~start_idx
          ~chunk_num ~auto_flush_threshold ~auto_flush_procedure:(`External cb)
    in
    let suffix0 = t.suffix in
    t.suffix <- suffix1;
    Suffix.close suffix0

  let reload_lower t ~volume_num =
    match t.lower with
    | Some lower -> Lower.reload ~volume_num lower
    | None -> Ok ()

  let cleanup ~root ~generation ~chunk_start_idx ~chunk_num ~lower =
    let () =
      Sys.readdir root
      |> Array.to_list
      |> List.filter (fun filename ->
             match Irmin_pack.Layout.Classification.Upper.v filename with
             | `Unknown | `Branch | `Control | `Dict | `V1_or_v2_pack -> false
             | `Prefix g | `Mapping g -> g <> generation
             | `Suffix idx ->
                 idx < chunk_start_idx || idx > chunk_start_idx + chunk_num
             | `Reachable _ | `Sorted _ | `Gc_result _ | `Control_tmp -> true)
      |> List.iter (fun residual ->
             let filename = Filename.concat root residual in
             [%log.debug "Remove residual file %s" filename];
             match Io.unlink filename with
             | Ok () -> ()
             | Error (`Sys_error error) ->
                 [%log.warn
                   "Could not remove residual file %s: %s" filename error])
    in
    Option.might (Lower.cleanup ~generation) lower

  let add_volume_and_update_control lower control =
    let open Result_syntax in
    (* Step 1. Add volume *)
    let* _ = Lower.add_volume lower in
    (* Step 2. Update control file *)
    let pl = Control.payload control in
    let pl = { pl with volume_num = Lower.volume_num lower } in
    [%log.debug "add_volume: update control_file volume_num:%d" pl.volume_num];
    Control.set_payload control pl

  let finish_constructing_rw config control ~make_dict ~make_suffix ~make_index
      ~make_lower =
    let open Result_syntax in
    let root = Irmin_pack.Conf.root config in
    let use_fsync = Irmin_pack.Conf.use_fsync config in
    let indexing_strategy = Conf.indexing_strategy config in
    let pl : Payload.t = Control.payload control in
    let generation, mapping_size =
      match pl.status with
      | From_v1_v2_post_upgrade _ | No_gc_yet
      | Used_non_minimal_indexing_strategy ->
          (0, None)
      | Gced x -> (x.generation, x.mapping_end_poff)
      | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
      | T15 ->
          assert false
    in
    let chunk_start_idx = pl.chunk_start_idx in
    let chunk_num = pl.chunk_num in
    (* 1. Create a ref for dependency injections for auto flushes *)
    let instance = ref None in
    let get_instance () =
      match !instance with
      | None ->
          [%log.warn
            "%s: instance was accessed whilst None; this is unexpected during \
             normal node operation"
            __FILE__];
          [%log.warn
            "%s: the stack trace is %s" __FILE__
              Printexc.(get_callstack 20 |> raw_backtrace_to_string)];
          (* get_instance is used by the callback functions below; if we reach this point, a
             callback was invoked whilst instance was None; it should be the case that we
             can ignore the callback *)
          assert false
      | Some x -> x
    in
    (* 2. Open the other files *)
    let* suffix =
      let auto_flush_threshold =
        Irmin_pack.Conf.suffix_auto_flush_threshold config
      in
      let cb _ = suffix_requires_a_flush_exn (get_instance ()) in
      make_suffix ~auto_flush_threshold ~auto_flush_procedure:(`External cb)
    in
    let* prefix = open_prefix ~root ~generation ~mapping_size in
    let* dict =
      let path = Layout.dict ~root in
      let auto_flush_threshold =
        Irmin_pack.Conf.dict_auto_flush_threshold config
      in
      let cb _ = dict_requires_a_flush_exn (get_instance ()) in
      make_dict ~path ~auto_flush_threshold ~auto_flush_procedure:(`External cb)
    in
    let* index =
      let log_size = Conf.index_log_size config in
      let throttle = Conf.merge_throttle config in
      let cb () =
        (* when creating the index, the index may call flush_callback, see
           https://github.com/mirage/irmin/issues/1963; so we can't assume that instance
           is set to Some _ in get_instance(); instead, we check instance, and just ignore
           the callback if the instance is None *)
        match !instance with
        | None -> ()
        | Some _ -> index_is_about_to_auto_flush_exn (get_instance ())
      in
      (* [cb] will not be called during calls to [index.flush] because we will
         use [~no_callback:()] *)
      make_index ~flush_callback:cb ~readonly:false ~throttle ~log_size root
    in
    (* 3. Open lower layer *)
    let* lower = make_lower () in
    (* 4. Perform any GC-related cleanups *)
    let* () = cleanup ~root ~generation ~chunk_start_idx ~chunk_num ~lower in
    let t =
      {
        dict;
        control;
        suffix;
        prefix;
        lower;
        use_fsync;
        index;
        dict_consumers = [];
        prefix_consumers = [];
        suffix_consumers = [];
        indexing_strategy;
        root;
      }
    in
    instance := Some t;
    Ok t

  let create_control_file ~overwrite config pl =
    let root = Irmin_pack.Conf.root config in
    let path = Layout.control ~root in
    let tmp_path = Layout.control_tmp ~root in
    Control.create_rw ~path ~tmp_path:(Some tmp_path) ~overwrite pl

  (* Reload ***************************************************************** *)

  let reload ?hook t =
    let open Result_syntax in
    (* Step 1. Reread index *)
    let* () = Index.reload t.index in
    (match hook with Some h -> h `After_index | None -> ());
    let pl0 = Control.payload t.control in
    (* Step 2. Reread control file *)
    let* () = Control.reload t.control in
    (match hook with Some h -> h `After_control | None -> ());
    let pl1 : Payload.t = Control.payload t.control in
    if pl0 = pl1 then Ok ()
    else
      (* Step 3. Reopen files if generation or chunk_num changed. *)
      let* () =
        let gen0 = generation pl0.status in
        let gen1 = generation pl1.status in
        let chunk_num0 = pl0.chunk_num in
        let chunk_num1 = pl1.chunk_num in
        let chunk_start_idx0 = pl0.chunk_start_idx in
        let chunk_start_idx1 = pl1.chunk_start_idx in
        (* Step 3.1. Potentially reload suffix *)
        let* () =
          if chunk_num0 <> chunk_num1 || chunk_start_idx0 <> chunk_start_idx1
          then
            let appendable_chunk_poff = pl1.appendable_chunk_poff in
            reopen_suffix t ~chunk_start_idx:chunk_start_idx1
              ~appendable_chunk_poff ~chunk_num:chunk_num1
          else Ok ()
        in
        (* Step 3.2. Potentially reload prefix *)
        let* () =
          if gen0 = gen1 then Ok ()
          else
            reopen_prefix t ~generation:gen1
              ~mapping_size:(mapping_size pl1.status)
        in
        (* Step 3.3. Potentially reload lower *)
        if gen0 = gen1 && pl0.volume_num = pl1.volume_num then Ok ()
        else reload_lower t ~volume_num:pl1.volume_num
      in
      (* Step 4. Update end offsets *)
      let* () =
        Suffix.refresh_appendable_chunk_poff t.suffix pl1.appendable_chunk_poff
      in
      (match hook with Some h -> h `After_suffix | None -> ());
      let* () = Dict.refresh_end_poff t.dict pl1.dict_end_poff in
      (* Step 5. Notify the dict consumers that they must reload *)
      let* () = notify_reload_consumers t.dict_consumers in
      Ok ()

  (* File creation ********************************************************** *)

  let create_lower_if_needed ~lower_root ~overwrite =
    match lower_root with
    | None -> Ok ()
    | Some path -> (
        match (Io.classify_path path, overwrite) with
        | `Directory, false -> Ok ()
        | `Directory, true ->
            (* TODO: implement recursive delete for lower root *)
            failwith
              (Fmt.str
                 "Lower root already exists but fresh = true in configuration. \
                  Please manually remove %s."
                 path)
        | `No_such_file_or_directory, _ -> Io.mkdir path
        | (`File | `Other), _ -> Errs.raise_error (`Not_a_directory path))

  let create_rw ~overwrite config =
    let open Result_syntax in
    let root = Irmin_pack.Conf.root config in
    let lower_root = Irmin_pack.Conf.lower_root config in
    let* () =
      match (overwrite, Io.classify_path root) with
      | _, (`File | `Other) -> Error (`Not_a_directory root)
      | false, `Directory -> Error (`File_exists root)
      | true, `Directory -> Ok ()
      | _, `No_such_file_or_directory -> Io.mkdir root
    in
    let* () = create_lower_if_needed ~lower_root ~overwrite in
    let* control =
      let open Payload in
      let status = No_gc_yet in
      let pl =
        let z = Int63.zero in
        {
          dict_end_poff = z;
          appendable_chunk_poff = z;
          checksum = z;
          status;
          upgraded_from = None;
          chunk_start_idx = 0;
          chunk_num = 1;
          volume_num = 0;
        }
      in
      create_control_file ~overwrite config pl
    in
    let make_dict = Dict.create_rw ~overwrite in
    let make_suffix = Suffix.create_rw ~root ~overwrite ~start_idx:0 in
    let make_index ~flush_callback ~readonly ~throttle ~log_size root =
      (* [overwrite] is ignored for index *)
      Index.v ~fresh:true ~flush_callback ~readonly ~throttle ~log_size root
    in
    let make_lower () =
      match lower_root with
      | None -> Ok None
      | Some path ->
          let* l = Lower.v ~readonly:false ~volume_num:0 path in
          let+ _ = add_volume_and_update_control l control in
          Some l
    in
    finish_constructing_rw config control ~make_dict ~make_suffix ~make_index
      ~make_lower

  (* Open rw **************************************************************** *)

  let dead_header_size_of_status = function
    | Payload.From_v1_v2_post_upgrade _ -> legacy_io_header_size
    | No_gc_yet | Gced _ | Used_non_minimal_indexing_strategy -> 0
    | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
    | T15 ->
        failwith "invalid status: T1..T15"

  let can_migrate_to_lower (payload : Payload.t) =
    match payload.status with
    | No_gc_yet | Used_non_minimal_indexing_strategy | From_v1_v2_post_upgrade _
      ->
        payload.chunk_num = 1 && payload.volume_num = 0
    | Gced _ -> false
    | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
    | T15 ->
        failwith "invalid status: T1..T15"

  let migrate_to_lower ~root ~lower_root ~control (payload : Payload.t) =
    let open Result_syntax in
    (* Step 1. Create a lower by moving the suffix file. *)
    let suffix_file =
      Layout.suffix_chunk ~root ~chunk_idx:payload.chunk_start_idx
    in
    let dead_header_size = dead_header_size_of_status payload.status in
    let end_offset = payload.appendable_chunk_poff in
    let* () =
      Lower.create_from ~src:suffix_file ~dead_header_size ~size:end_offset
        lower_root
    in
    (* Step 2. Create a new empty suffix for the upper. *)
    let chunk_start_idx = payload.chunk_start_idx + 1 in
    let* () =
      Suffix.create_rw ~root ~overwrite:false ~auto_flush_threshold:1_000_000
        ~auto_flush_procedure:`Internal ~start_idx:chunk_start_idx
      >>= Suffix.close
    in
    (* Step 3. Create a new empty prefix for the upper. *)
    let generation = 1 in
    let* () =
      let mapping = Layout.mapping ~generation ~root in
      let data = Layout.prefix ~root ~generation in
      Sparse.Ao.create ~mapping ~data >>= Sparse.Ao.close
    in
    (* Step 4. Remove dead header from dict (if needed) *)
    let* dict_end_poff, after_payload_write =
      if dead_header_size > 0 then (
        let dict_path = Layout.dict ~root in
        let tmp_dict_path = Filename.temp_file ~temp_dir:root "store" "dict" in
        let* dict_file = Io.open_ ~path:dict_path ~readonly:false in
        let* len = Io.read_size dict_file in
        let* tmp_dict_file = Io.open_ ~path:tmp_dict_path ~readonly:false in
        let contents_len = Int63.to_int len - dead_header_size in
        let* contents =
          Io.read_to_string dict_file
            ~off:(Int63.of_int dead_header_size)
            ~len:contents_len
        in
        Io.write_exn tmp_dict_file ~off:Int63.zero ~len:contents_len contents;
        let* _ = Io.close dict_file in
        let* _ = Io.close tmp_dict_file in
        (* Delay moving the temp file until after the payload is written so
           that we do not try to remove the dead header twice after a failure. *)
        Ok
          ( Int63.of_int contents_len,
            fun () -> Io.move_file ~src:tmp_dict_path ~dst:dict_path ))
      else Ok (payload.dict_end_poff, Fun.const (Ok ()))
    in
    (* Step 5. Update the upper control file. *)
    let payload =
      {
        payload with
        dict_end_poff;
        chunk_start_idx;
        appendable_chunk_poff = Int63.zero;
        volume_num = 1;
        status =
          Gced
            {
              suffix_start_offset = end_offset;
              generation;
              latest_gc_target_offset = Int63.zero;
              suffix_dead_bytes = Int63.zero;
              mapping_end_poff = Some Int63.zero;
            };
      }
    in
    let* () = Control.set_payload control payload in
    let* () = after_payload_write () in
    Ok payload

  let load_payload ~config ~root ~lower_root ~control =
    let payload = Control.payload control in
    match lower_root with
    | Some lower_root when payload.volume_num = 0 ->
        if Irmin_pack.Conf.no_migrate config then Error `Migration_needed
        else if not (can_migrate_to_lower payload) then
          Error `Migration_to_lower_not_allowed
        else migrate_to_lower ~root ~lower_root ~control payload
    | _ -> Ok payload

  let open_rw_with_control_file config =
    let open Result_syntax in
    let root = Irmin_pack.Conf.root config in
    let lower_root = Irmin_pack.Conf.lower_root config in
    let* () = create_lower_if_needed ~lower_root ~overwrite:false in
    let* control =
      let path = Layout.control ~root in
      let tmp_path = Layout.control_tmp ~root in
      Control.open_ ~readonly:false ~path ~tmp_path:(Some tmp_path)
    in
    let* Payload.
           {
             status;
             appendable_chunk_poff;
             chunk_start_idx = start_idx;
             chunk_num;
             dict_end_poff;
             volume_num;
             _;
           } =
      load_payload ~config ~root ~lower_root ~control
    in
    let* dead_header_size =
      match status with
      | From_v1_v2_post_upgrade _ -> Ok legacy_io_header_size
      | Gced _ ->
          let indexing_strategy = Conf.indexing_strategy config in
          if Irmin_pack.Indexing_strategy.is_minimal indexing_strategy then Ok 0
          else Error `Only_minimal_indexing_strategy_allowed
      | No_gc_yet | Used_non_minimal_indexing_strategy -> Ok 0
      | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
      | T15 ->
          Error `V3_store_from_the_future
    in
    let make_dict = Dict.open_rw ~end_poff:dict_end_poff ~dead_header_size in
    let make_suffix =
      Suffix.open_rw ~root ~appendable_chunk_poff ~start_idx ~chunk_num
        ~dead_header_size
    in
    let make_index ~flush_callback ~readonly ~throttle ~log_size root =
      Index.v ~fresh:false ~flush_callback ~readonly ~throttle ~log_size root
    in
    let make_lower () =
      match lower_root with
      | None -> Ok None
      | Some lower_root ->
          assert (volume_num > 0);
          let+ l = Lower.v ~readonly:false ~volume_num lower_root in
          Some l
    in
    finish_constructing_rw config control ~make_dict ~make_suffix ~make_index
      ~make_lower

  let read_offset_from_legacy_file path =
    let open Result_syntax in
    (* Bytes 0-7 contains the offset. Bytes 8-15 contain the version. *)
    let* io = Io.open_ ~path ~readonly:true in
    Errors.finalise (fun _ ->
        Io.close io |> Errs.log_if_error "FM: read_offset_from_legacy_file")
    @@ fun () ->
    let* s = Io.read_to_string io ~off:Int63.zero ~len:8 in
    let x = Int63.decode ~off:0 s in
    Ok x

  let read_version_from_legacy_file path =
    let open Result_syntax in
    (* Bytes 0-7 contains the offset. Bytes 8-15 contain the version. *)
    let* io = Io.open_ ~path ~readonly:true in
    Errors.finalise (fun _ ->
        Io.close io |> Errs.log_if_error "FM: read_version_from_legacy_file")
    @@ fun () ->
    let off = Int63.of_int 8 in
    let* s = Io.read_to_string io ~off ~len:8 in
    match Version.of_bin s with
    | Some x -> Ok x
    | None -> Error `Corrupted_legacy_file

  let open_rw_migrate_from_v1_v2 config =
    let open Result_syntax in
    let root = Irmin_pack.Conf.root config in
    let src = Irmin_pack.Layout.V1_and_v2.pack ~root in
    let chunk_start_idx = 0 in
    let dst = Layout.suffix_chunk ~root ~chunk_idx:chunk_start_idx in
    let* suffix_end_poff = read_offset_from_legacy_file src in
    let* dict_end_poff =
      let path = Layout.dict ~root in
      read_offset_from_legacy_file path
    in
    let* () = Io.move_file ~src ~dst in
    let* control =
      let open Payload in
      let status =
        From_v1_v2_post_upgrade
          { entry_offset_at_upgrade_to_v3 = suffix_end_poff }
      in
      let pl =
        {
          dict_end_poff;
          appendable_chunk_poff = suffix_end_poff;
          checksum = Int63.zero;
          status;
          upgraded_from = None;
          chunk_start_idx;
          chunk_num = 1;
          volume_num = 0;
        }
      in
      create_control_file ~overwrite:false config pl
    in
    let* () = Control.close control in
    open_rw_with_control_file config

  let open_rw_no_control_file config =
    let root = Irmin_pack.Conf.root config in
    let suffix_path = Irmin_pack.Layout.V1_and_v2.pack ~root in
    match Io.classify_path suffix_path with
    | `Directory | `No_such_file_or_directory | `Other -> Error `Invalid_layout
    | `File -> open_rw_migrate_from_v1_v2 config

  let open_rw config =
    let root = Irmin_pack.Conf.root config in
    let no_migrate = Irmin_pack.Conf.no_migrate config in
    match Io.classify_path root with
    | `File | `Other -> Error (`Not_a_directory root)
    | `No_such_file_or_directory -> Error (`No_such_file_or_directory root)
    | `Directory -> (
        let path = Layout.control ~root in
        match Io.classify_path path with
        | `File -> open_rw_with_control_file config
        | `No_such_file_or_directory ->
            if no_migrate then Error `Migration_needed
            else open_rw_no_control_file config
        | `Directory | `Other -> Error `Invalid_layout)

  (* Open ro **************************************************************** *)

  let open_ro config =
    let open Result_syntax in
    let indexing_strategy = Conf.indexing_strategy config in
    let root = Irmin_pack.Conf.root config in
    let lower_root = Irmin_pack.Conf.lower_root config in
    let use_fsync = Irmin_pack.Conf.use_fsync config in
    (* 1. Open the control file *)
    let* control =
      let path = Layout.control ~root in
      Control.open_ ~readonly:true ~path ~tmp_path:None
      (* If no control file, then check whether the store is in v1 or v2. *)
      |> Result.map_error (function
           | `No_such_file_or_directory _ -> (
               let pack = Irmin_pack.Layout.V1_and_v2.pack ~root in
               match Io.classify_path pack with
               | `File -> `Migration_needed
               | `No_such_file_or_directory -> `No_such_file_or_directory pack
               | `Directory | `Other -> `Invalid_layout)
           | error -> error)
    in
    let Payload.
          {
            status;
            appendable_chunk_poff;
            chunk_start_idx = start_idx;
            chunk_num;
            dict_end_poff;
            volume_num;
            _;
          } =
      Control.payload control
    in
    let dead_header_size = dead_header_size_of_status status in
    let generation = generation status in
    (* 2. Open the other files *)
    let* suffix =
      Suffix.open_ro ~root ~appendable_chunk_poff ~start_idx ~chunk_num
        ~dead_header_size
    in
    let* prefix =
      open_prefix ~root ~generation ~mapping_size:(mapping_size status)
    in
    let* dict =
      let path = Layout.dict ~root in
      Dict.open_ro ~path ~end_poff:dict_end_poff ~dead_header_size
    in
    let* index =
      let log_size = Conf.index_log_size config in
      let throttle = Conf.merge_throttle config in
      Index.v ~fresh:false ~readonly:true ~throttle ~log_size root
    in
    (* 3. Open lower layer *)
    let* lower =
      match lower_root with
      | None -> Ok None
      | Some path ->
          let+ l = Lower.v ~readonly:true ~volume_num path in
          Some l
    in
    (* 4. return with success *)
    Ok
      {
        dict;
        control;
        suffix;
        prefix;
        lower;
        use_fsync;
        indexing_strategy;
        index;
        dict_consumers = [];
        prefix_consumers = [];
        suffix_consumers = [];
        root;
      }

  (* MISC. ****************************************************************** *)

  let version ~root =
    let v2_or_v1 () =
      let path = Irmin_pack.Layout.V1_and_v2.pack ~root in
      match read_version_from_legacy_file path with
      | Ok v -> Ok v
      | Error `Double_close | Error `Invalid_argument | Error `Closed ->
          assert false
      | Error (`No_such_file_or_directory _) -> Error `Invalid_layout
      | Error `Not_a_file -> Error `Invalid_layout
      | Error `Corrupted_legacy_file | Error `Read_out_of_bounds ->
          Error `Corrupted_legacy_file
      | Error (`Io_misc _) as e -> e
    in
    match Io.classify_path root with
    | `No_such_file_or_directory -> Error (`No_such_file_or_directory root)
    | `File | `Other -> Error (`Not_a_directory root)
    | `Directory -> (
        let path = Layout.control ~root in
        match Control.open_ ~path ~tmp_path:None ~readonly:true with
        | Ok _ -> Ok `V3
        | Error (`No_such_file_or_directory _) -> v2_or_v1 ()
        | Error `Not_a_file -> Error `Invalid_layout
        | Error `Closed -> assert false
        | Error
            ( `Io_misc _ | `Corrupted_control_file _
            | `Unknown_major_pack_version _ ) as e ->
            e)

  let swap t ~generation ~mapping_size ~suffix_start_offset ~chunk_start_idx
      ~chunk_num ~suffix_dead_bytes ~latest_gc_target_offset ~volume =
    let open Result_syntax in
    [%log.debug
      "Gc in main: swap gen %d; suffix start %a; chunk start idx %d; chunk num \
       %d; suffix dead bytes %a"
      generation Int63.pp suffix_start_offset chunk_start_idx chunk_num Int63.pp
        suffix_dead_bytes];
    let c0 = Mtime_clock.counter () in
    let pl = Control.payload t.control in

    (* Step 1. Reopen files *)
    let mapping_size = Some mapping_size in
    let* () = reopen_prefix t ~generation ~mapping_size in
    let* () =
      reopen_suffix t ~chunk_start_idx ~chunk_num
        ~appendable_chunk_poff:pl.appendable_chunk_poff
    in
    let span1 = Mtime_clock.count c0 |> Mtime.span_to_us in

    (* Step 2. Update the control file *)
    let* () =
      let pl =
        let open Payload in
        let status =
          match pl.status with
          | From_v1_v2_post_upgrade _ -> assert false
          | Used_non_minimal_indexing_strategy -> assert false
          | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13
          | T14 | T15 ->
              assert false
          | Gced _ | No_gc_yet ->
              Gced
                {
                  suffix_start_offset;
                  generation;
                  latest_gc_target_offset;
                  suffix_dead_bytes;
                  mapping_end_poff = mapping_size;
                }
        in

        { pl with status; chunk_start_idx; chunk_num }
      in
      [%log.debug "GC: writing new control_file"];
      Control.set_payload t.control pl
    in

    (* Step 3. Swap volume and reload lower if needed *)
    let* () =
      match volume with
      | None -> Ok ()
      | Some volume -> (
          match t.lower with
          | None ->
              assert false
              (* Programmer error if lower does not exist but volume is given *)
          | Some lower ->
              Lower.swap ~volume ~generation ~volume_num:pl.volume_num lower)
    in

    let span2 = Mtime_clock.count c0 |> Mtime.span_to_us in
    [%log.debug
      "Gc reopen files, update control: %.0fus, %.0fus" span1 (span2 -. span1)];
    Ok ()

  let readonly t = Suffix.readonly t.suffix

  let generation t =
    let pl = Control.payload t.control in
    match pl.status with
    | From_v1_v2_post_upgrade _ | Used_non_minimal_indexing_strategy -> 0
    | No_gc_yet -> 0
    | Gced x -> x.generation
    | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
    | T15 ->
        (* Unreachable *)
        assert false

  let gc_behaviour t = match t.lower with Some _ -> `Archive | None -> `Delete

  let gc_destination t =
    match gc_behaviour t with
    | `Delete -> `Delete
    | `Archive -> `Archive (Option.get t.lower)

  let gc_allowed t =
    let pl = Control.payload t.control in
    let action = gc_behaviour t in
    match (action, pl.status) with
    | `Delete, (From_v1_v2_post_upgrade _ | Used_non_minimal_indexing_strategy)
      ->
        false
    | `Delete, (No_gc_yet | Gced _) -> true
    | `Archive, _ -> true
    | ( _,
        ( T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13
        | T14 | T15 ) ) ->
        (* Unreachable *)
        assert false

  let split t =
    let open Result_syntax in
    (* Step 1. Create a new chunk file *)
    let auto_flush_threshold =
      match Suffix.auto_flush_threshold t.suffix with
      | None -> assert false
      | Some x -> x
    in
    let cb _ = suffix_requires_a_flush_exn t in
    let* () =
      Suffix.add_chunk ~auto_flush_threshold
        ~auto_flush_procedure:(`External cb) t.suffix
    in

    (* Step 2. Update the control file *)
    let pl = Control.payload t.control in
    let pl =
      let open Payload in
      let chunk_num = succ pl.chunk_num in
      (* Update the control file with the poff of the last chunk. As last chunk
         is fresh, the poff is zero. *)
      let appendable_chunk_poff = Int63.zero in
      { pl with chunk_num; appendable_chunk_poff }
    in
    [%log.debug
      "split: update control_file chunk_start_idx:%d chunk_num:%d"
        pl.chunk_start_idx pl.chunk_num];
    Control.set_payload t.control pl

  let add_volume t =
    match t.lower with
    | None -> Error `Add_volume_requires_lower
    | Some lower -> add_volume_and_update_control lower t.control

  let cleanup t =
    let root = t.root in
    let pl : Payload.t = Control.payload t.control in
    let generation = generation t in
    let chunk_start_idx = pl.chunk_start_idx in
    let chunk_num = pl.chunk_num in
    let lower = t.lower in
    cleanup ~root ~generation ~chunk_start_idx ~chunk_num ~lower

  let create_one_commit_store t config gced commit_key =
    let open Result_syntax in
    let src_root = t.root in
    let dst_root = Irmin_pack.Conf.root config in
    (* Step 1. Copy the dict *)
    let src_dict = Layout.dict ~root:src_root in
    let dst_dict = Layout.dict ~root:dst_root in
    let* () = Io.copy_file ~src:src_dict ~dst:dst_dict in
    (* Step 2. Create an empty suffix and close it. *)
    let* suffix =
      Suffix.create_rw ~root:dst_root ~overwrite:false
        ~auto_flush_threshold:1_000_000 ~auto_flush_procedure:`Internal
        ~start_idx:1
    in
    let* () = Suffix.close suffix in
    (* Step 3. Create the control file and close it. *)
    let status = Payload.Gced gced in
    let dict_end_poff = Io.size_of_path dst_dict |> Errs.raise_if_error in
    let pl =
      {
        Payload.dict_end_poff;
        appendable_chunk_poff = Int63.zero;
        checksum = Int63.zero;
        status;
        upgraded_from = None;
        chunk_num = 1;
        chunk_start_idx = 1;
        volume_num = 0;
      }
    in
    let path = Layout.control ~root:dst_root in
    let* control = Control.create_rw ~path ~tmp_path:None ~overwrite:false pl in
    let* () = Control.close control in
    (* Step 4. Create the index. *)
    let* index =
      let log_size = Conf.index_log_size config in
      let throttle = Conf.merge_throttle config in
      Index.v ~fresh:true ~flush_callback:Fun.id ~readonly:false ~throttle
        ~log_size dst_root
    in
    (* Step 5. Add the commit to the index, close the index. *)
    let () =
      match Pack_key.inspect commit_key with
      | Pack_key.Direct { hash; offset; length; _ } ->
          Index.add index hash (offset, length, Pack_value.Kind.Commit_v2)
      | Indexed _ -> assert false
    in
    let* () = Index.close index in
    Ok ()
end
