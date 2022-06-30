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

include File_manager_intf
open Import
module Payload = Control_file.Latest_payload
include File_manager_intf

let legacy_io_header_size = 16

module Make
    (Control : Control_file.S with module Io = Io.Unix)
    (Dict : Append_only_file.S with module Io = Control.Io)
    (Suffix : Append_only_file.S with module Io = Control.Io)
    (Index : Pack_index.S)
    (Errs : Io_errors.S with module Io = Control.Io) =
struct
  module Io = Control.Io

  type dict_consumer_data = {
    after_reload : unit -> (unit, Io.read_error) result;
  }

  type suffix_consumer_data = { after_flush : unit -> unit }

  type t = {
    dict : Dict.t;
    control : Control.t;
    mutable suffix : Suffix.t;
    index : Index.t;
    mutable dict_consumers : dict_consumer_data list;
    mutable suffix_consumers : suffix_consumer_data list;
    indexing_strategy : Irmin_pack.Indexing_strategy.t;
    use_fsync : bool;
    root : string;
  }

  module Control = Control
  module Dict = Dict
  module Suffix = Suffix
  module Index = Index
  module Errs = Errs

  let control t = t.control
  let dict t = t.dict
  let suffix t = t.suffix
  let index t = t.index

  let close t =
    let open Result_syntax in
    let* () = Dict.close t.dict in
    let* () = Control.close t.control in
    let* () = Suffix.close t.suffix in
    let+ () = Index.close t.index in
    ()

  let register_dict_consumer t ~after_reload =
    t.dict_consumers <- { after_reload } :: t.dict_consumers

  let register_suffix_consumer t ~after_flush =
    t.suffix_consumers <- { after_flush } :: t.suffix_consumers

  (* Reload ***************************************************************** *)

  let generation = function
    | Payload.From_v1_v2_post_upgrade _
    | From_v3_used_non_minimal_indexing_strategy | From_v3_no_gc_yet ->
        0
    | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
    | T15 ->
        (* Unreachable *)
        assert false
    | From_v3_gced x -> x.generation

  let reload t =
    let open Result_syntax in
    let* () = Index.reload t.index in
    let pl0 = Control.payload t.control in
    let* () = Control.reload t.control in
    let pl1 : Payload.t = Control.payload t.control in
    if pl0 = pl1 then Ok ()
    else
      (* Check if generation changed. If it did, reopen suffix. *)
      let* () =
        let gen0 = generation pl0.status in
        let gen1 = generation pl1.status in
        if gen0 = gen1 then Ok ()
        else
          let* suffix1 =
            let path =
              Irmin_pack.Layout.V3.suffix ~root:t.root ~generation:gen1
            in
            let end_offset = pl1.entry_offset_suffix_end in
            [%log.debug "reload: generation changed, opening %s" path];
            Suffix.open_ro ~path ~end_offset ~dead_header_size:0
          in
          let suffix0 = t.suffix in
          t.suffix <- suffix1;
          Suffix.close suffix0
      in
      (* Update end offsets. This prevents the readonly instance to read data
         flushed to disk by the readwrite, between calls to reload. *)
      let* () =
        Suffix.refresh_end_offset t.suffix pl1.entry_offset_suffix_end
      in
      let* () = Dict.refresh_end_offset t.dict pl1.dict_offset_end in
      let+ () =
        let res =
          List.fold_left
            (fun acc { after_reload } -> Result.bind acc after_reload)
            (Ok ()) t.dict_consumers
        in
        (* The following dirty trick casts the result from
           [read_error] to [ [>read_error] ]. *)
        match res with Ok () -> Ok () | Error (#Io.read_error as e) -> Error e
      in
      ()

  (** Flush stages *************************************************************

      The irmin-pack files are only mutated during calls to one of the 3
      following functions. Exceptions:

      - During [create] and [open_rw].
      - During a GC.
      - When the branch store is modified. *)

  (* TODO: Stat on flushes:
     - How many calls to Dict.flush/Suffix.flush/Index.flush
     - How many calls to [flush] and the 3 auto ones. *)

  (** Flush stage 1 *)
  let flush_dict t =
    let open Result_syntax in
    if Dict.empty_buffer t.dict then Ok ()
    else
      (* NOTE we call the Stats increment function before the call to the Dict flush
         function; in general we increment the stat before the call, everywhere in this
         file. *)
      let* () =
        Stats.incr_fm_field Dict_flushes;
        Dict.flush t.dict
      in
      let* () = if t.use_fsync then Dict.fsync t.dict else Ok () in
      let* () =
        let pl : Payload.t = Control.payload t.control in
        let pl = { pl with dict_offset_end = Dict.end_offset t.dict } in
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
          | From_v3_gced _ -> pl.status
          | From_v3_no_gc_yet ->
              (* Using physical equality to test which indexing_strategy
                 we are using. Might not be great in the long term. *)
              if t.indexing_strategy == Irmin_pack.Indexing_strategy.minimal
              then pl.status
              else (
                [%log.warn
                  "Updating the control file from [From_v3] to \
                   [From_v3_used_non_minimal_indexing_strategy]. It won't be \
                   possible to GC this irmin-pack store anymore."];
                Payload.From_v3_used_non_minimal_indexing_strategy)
          | From_v3_used_non_minimal_indexing_strategy -> pl.status
          | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13
          | T14 | T15 ->
              assert false
        in
        let pl =
          {
            pl with
            entry_offset_suffix_end = Suffix.end_offset t.suffix;
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

  (* File creation ********************************************************** *)

  let finish_constructing_rw config control ~make_dict ~make_suffix ~make_index
      =
    let open Result_syntax in
    let root = Irmin_pack.Conf.root config in
    let use_fsync = Irmin_pack.Conf.use_fsync config in
    let indexing_strategy = Conf.indexing_strategy config in
    let pl : Payload.t = Control.payload control in
    let generation =
      match pl.status with
      | From_v1_v2_post_upgrade _ | From_v3_no_gc_yet
      | From_v3_used_non_minimal_indexing_strategy ->
          0
      | From_v3_gced x -> x.generation
      | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
      | T15 ->
          assert false
    in
    (* 1. Create a ref for dependency injections for auto flushes *)
    let instance = ref None in
    let get_instance () =
      match !instance with None -> assert false | Some x -> x
    in
    (* 2. Open the other files *)
    let* suffix =
      let path = Irmin_pack.Layout.V3.suffix ~root ~generation in
      let auto_flush_threshold =
        Irmin_pack.Conf.suffix_auto_flush_threshold config
      in
      let cb () = suffix_requires_a_flush_exn (get_instance ()) in
      make_suffix ~path ~auto_flush_threshold ~auto_flush_callback:cb
    in
    let* dict =
      let path = Irmin_pack.Layout.V3.dict ~root in
      let auto_flush_threshold =
        Irmin_pack.Conf.dict_auto_flush_threshold config
      in
      let cb () = dict_requires_a_flush_exn (get_instance ()) in
      make_dict ~path ~auto_flush_threshold ~auto_flush_callback:cb
    in
    let* index =
      let log_size = Conf.index_log_size config in
      let throttle = Conf.merge_throttle config in
      let cb () = index_is_about_to_auto_flush_exn (get_instance ()) in
      (* [cb] will not be called during calls to [index.flush] because we will
         use [~no_callback:()] *)
      make_index ~flush_callback:cb ~readonly:false ~throttle ~log_size root
    in
    let t =
      {
        dict;
        control;
        suffix;
        use_fsync;
        index;
        dict_consumers = [];
        suffix_consumers = [];
        indexing_strategy;
        root;
      }
    in
    instance := Some t;
    Ok t

  let create_control_file ~overwrite config pl =
    let root = Irmin_pack.Conf.root config in
    let path = Irmin_pack.Layout.V3.control ~root in
    Control.create_rw ~path ~overwrite pl

  let create_rw ~overwrite config =
    let open Result_syntax in
    let root = Irmin_pack.Conf.root config in
    let* () =
      match (overwrite, Io.classify_path root) with
      | _, (`File | `Other) -> Error (`Not_a_directory root)
      | false, `Directory -> Error (`File_exists root)
      | true, `Directory -> Ok ()
      | _, `No_such_file_or_directory -> Io.mkdir root
    in
    let* control =
      let open Payload in
      let status = From_v3_no_gc_yet in
      let pl =
        let z = Int63.zero in
        { dict_offset_end = z; entry_offset_suffix_end = z; status }
      in
      create_control_file ~overwrite config pl
    in
    let make_dict = Dict.create_rw ~overwrite in
    let make_suffix = Suffix.create_rw ~overwrite in
    let make_index ~flush_callback ~readonly ~throttle ~log_size root =
      (* [overwrite] is ignored for index *)
      Index.v ~fresh:true ~flush_callback ~readonly ~throttle ~log_size root
    in
    finish_constructing_rw config control ~make_dict ~make_suffix ~make_index

  (* Open rw **************************************************************** *)

  let open_rw_with_control_file config =
    let open Result_syntax in
    let root = Irmin_pack.Conf.root config in
    let* control =
      let path = Irmin_pack.Layout.V3.control ~root in
      Control.open_ ~readonly:false ~path
    in
    let pl : Payload.t = Control.payload control in
    let* dead_header_size =
      match pl.status with
      | From_v1_v2_post_upgrade _ -> Ok legacy_io_header_size
      | From_v3_gced _ ->
          let indexing_strategy = Conf.indexing_strategy config in
          (* Using physical equality to test which indexing_strategy
             we are using. Might not be great in the long term. *)
          if indexing_strategy == Irmin_pack.Indexing_strategy.minimal then Ok 0
          else Error `Only_minimal_indexing_strategy_allowed
      | From_v3_no_gc_yet | From_v3_used_non_minimal_indexing_strategy -> Ok 0
      | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
      | T15 ->
          Error `V3_store_from_the_future
    in
    let make_dict =
      let end_offset = pl.dict_offset_end in
      Dict.open_rw ~end_offset ~dead_header_size
    in
    let make_suffix =
      let end_offset = pl.entry_offset_suffix_end in
      Suffix.open_rw ~end_offset ~dead_header_size
    in
    let make_index ~flush_callback ~readonly ~throttle ~log_size root =
      Index.v ~fresh:false ~flush_callback ~readonly ~throttle ~log_size root
    in
    finish_constructing_rw config control ~make_dict ~make_suffix ~make_index

  let decode_int63 buf = Int63.decode ~off:0 buf

  let read_offset_from_legacy_file path =
    let open Result_syntax in
    (* Bytes 0-7 contains the offset. Bytes 8-15 contain the version. *)
    let* io = Io.open_ ~path ~readonly:true in
    Errors.finalise (fun _ ->
        Io.close io |> Errs.log_if_error "FM: read_offset_from_legacy_file")
    @@ fun () ->
    let* s = Io.read_to_string io ~off:Int63.zero ~len:8 in
    let x = decode_int63 s in
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
    let dst = Irmin_pack.Layout.V3.suffix ~root ~generation:0 in
    let* entry_offset_suffix_end = read_offset_from_legacy_file src in
    let* dict_offset_end =
      let path = Irmin_pack.Layout.V3.dict ~root in
      read_offset_from_legacy_file path
    in
    let* () = Io.move_file ~src ~dst in
    let* control =
      let open Payload in
      let status =
        From_v1_v2_post_upgrade
          { entry_offset_at_upgrade_to_v3 = entry_offset_suffix_end }
      in
      let pl = { dict_offset_end; entry_offset_suffix_end; status } in
      create_control_file ~overwrite:false config pl
    in
    let* () = Control.close control in
    open_rw_with_control_file config

  let open_rw_no_control_file config =
    let root = Irmin_pack.Conf.root config in
    let suffix_path = Irmin_pack.Layout.V1_and_v2.pack ~root in
    match Io.classify_path suffix_path with
    | `Directory -> Error `Invalid_layout
    | `No_such_file_or_directory | `Other -> Error `Invalid_layout
    | `File -> open_rw_migrate_from_v1_v2 config

  let open_rw config =
    let root = Irmin_pack.Conf.root config in
    let no_migrate = Irmin_pack.Conf.no_migrate config in
    match Io.classify_path root with
    | `File | `Other -> Error (`Not_a_directory root)
    | `No_such_file_or_directory -> Error `No_such_file_or_directory
    | `Directory -> (
        let path = Irmin_pack.Layout.V3.control ~root in
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
    let use_fsync = Irmin_pack.Conf.use_fsync config in
    (* 1. Open the control file *)
    let* control =
      let path = Irmin_pack.Layout.V3.control ~root in
      Control.open_ ~readonly:true ~path
      (* If no control file, then check whether the store is in v1 or v2. *)
      |> Result.map_error (function
           | `No_such_file_or_directory ->
               let pack = Irmin_pack.Layout.V1_and_v2.pack ~root in
               if Io.classify_path pack = `File then `Migration_needed
               else `No_such_file_or_directory
           | error -> error)
    in
    let pl : Payload.t = Control.payload control in
    let* dead_header_size =
      match pl.status with
      | From_v1_v2_post_upgrade _ -> Ok legacy_io_header_size
      | From_v3_no_gc_yet | From_v3_gced _
      | From_v3_used_non_minimal_indexing_strategy ->
          Ok 0
      | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
      | T15 ->
          Error `V3_store_from_the_future
    in
    let generation = generation pl.status in
    (* 2. Open the other files *)
    let* suffix =
      let path = Irmin_pack.Layout.V3.suffix ~root ~generation in
      let end_offset = pl.entry_offset_suffix_end in
      Suffix.open_ro ~path ~end_offset ~dead_header_size
    in
    let* dict =
      let path = Irmin_pack.Layout.V3.dict ~root in
      let end_offset = pl.dict_offset_end in
      Dict.open_ro ~path ~end_offset ~dead_header_size
    in
    let* index =
      let log_size = Conf.index_log_size config in
      let throttle = Conf.merge_throttle config in
      Index.v ~fresh:false ~readonly:true ~throttle ~log_size root
    in
    (* 3. return with success *)
    Ok
      {
        dict;
        control;
        suffix;
        use_fsync;
        indexing_strategy;
        index;
        dict_consumers = [];
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
      | Error `No_such_file_or_directory -> Error `Invalid_layout
      | Error `Not_a_file -> Error `Invalid_layout
      | Error `Corrupted_legacy_file | Error `Read_out_of_bounds ->
          Error `Corrupted_legacy_file
      | Error (`Io_misc _) as e -> e
    in
    match Io.classify_path root with
    | `No_such_file_or_directory -> Error `No_such_file_or_directory
    | `File | `Other -> Error (`Not_a_directory root)
    | `Directory -> (
        let path = Irmin_pack.Layout.V3.control ~root in
        match Control.open_ ~path ~readonly:true with
        | Ok _ -> Ok `V3
        | Error `No_such_file_or_directory -> v2_or_v1 ()
        | Error `Not_a_file -> Error `Invalid_layout
        | Error `Closed -> assert false
        | Error
            ( `Io_misc _ | `Corrupted_control_file
            | `Unknown_major_pack_version _ ) as e ->
            e)

  let swap t ~generation ~copy_end_offset =
    let open Result_syntax in
    (* Step 1. Open the suffix *)
    let* new_suffix =
      let end_offset = copy_end_offset in
      let path = Irmin_pack.Layout.V3.suffix ~root:t.root ~generation in
      let dead_header_size = 0 in
      let auto_flush_threshold =
        match Suffix.auto_flush_threshold t.suffix with
        | Some x -> x
        | None -> assert false
      in
      let cb () = suffix_requires_a_flush_exn t in
      [%log.debug "GC: opening new suffix in rw mode: %s" path];
      Suffix.open_rw ~end_offset ~dead_header_size ~path ~auto_flush_threshold
        ~auto_flush_callback:cb
    in

    (* Step 2. Update the control file *)
    let* () =
      let pl = Control.payload t.control in
      let pl =
        let open Payload in
        (* [swap] will logically only be called while in one of the 2 statuses. *)
        let status =
          match pl.status with
          | From_v1_v2_post_upgrade _ -> assert false
          | From_v3_used_non_minimal_indexing_strategy -> assert false
          | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13
          | T14 | T15 ->
              assert false
          | From_v3_gced _ | From_v3_no_gc_yet -> From_v3_gced { generation }
        in
        { pl with status }
      in
      [%log.debug "GC: writing new control_file"];
      Control.set_payload t.control pl
    in

    (* Step 3. Use the new suffix in the rw instance *)
    let old_suffix = t.suffix in
    t.suffix <- new_suffix;

    (* Step 4. Close and return *)
    let* () = Suffix.close old_suffix in
    Ok ()

  let write_gc_output ~root ~generation output =
    let open Result_syntax in
    let path = Irmin_pack.Layout.V3.gc_result ~root ~generation in
    let* io = Io.create ~path ~overwrite:true in
    let out = Errs.to_json_string output in
    let* () = Io.write_string io ~off:Int63.zero out in
    Io.close io

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
    match read_file () with
    | Error err -> Error (`Corrupted_gc_result_file (Fmt.str "%a" Errs.pp err))
    | Ok x ->
        Errs.of_json_string x
        |> Result.map_error (fun err ->
               `Gc_process_error (Fmt.str "%a" Errs.pp err))
end
