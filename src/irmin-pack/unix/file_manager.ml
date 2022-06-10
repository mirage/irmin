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
    (Index : Pack_index.S) =
struct
  module Io = Control.Io

  type t = {
    dict : Dict.t;
    control : Control.t;
    suffix : Suffix.t;
    index : Index.t;
    use_fsync : bool;
  }

  module Control = Control
  module Dict = Dict
  module Suffix = Suffix
  module Index = Index

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

  (* Reload ***************************************************************** *)

  let reload t =
    let open Result_syntax in
    let* () = Index.reload t.index in
    let pl0 = Control.payload t.control in
    let* () = Control.reload t.control in
    let pl1 : Payload.t = Control.payload t.control in
    if pl0 = pl1 then Ok ()
    else
      let* () =
        Suffix.refresh_end_offset t.suffix pl1.entry_offset_suffix_end
      in
      let+ () = Dict.refresh_end_offset t.dict pl1.dict_offset_end in
      (* TODO: call Dict's refill *)
      ()

  let reload_exn t =
    match reload t with
    | Ok () -> ()
    | Error _ ->
        (* TODO: Proper exception *)
        assert false

  (* Flush stages *********************************************************** *)

  (** Flush stage 1 *)
  let flush_dict t =
    let open Result_syntax in
    let* () = Dict.flush t.dict in
    let* () = if t.use_fsync then Dict.fsync t.dict else Ok () in
    let* () =
      let pl : Payload.t = Control.payload t.control in
      let pl = { pl with dict_offset_end = Dict.end_offset t.dict } in
      Control.set_payload t.control pl
    in
    let+ () = if t.use_fsync then Control.fsync t.control else Ok () in
    ()

  (** Flush stage 2 *)
  let flush_suffix_and_its_deps t =
    let open Result_syntax in
    let* () = flush_dict t in
    let* () = Suffix.flush t.suffix in
    let* () = if t.use_fsync then Suffix.fsync t.suffix else Ok () in
    let* () =
      let pl : Payload.t = Control.payload t.control in
      let pl =
        { pl with entry_offset_suffix_end = Suffix.end_offset t.suffix }
      in
      Control.set_payload t.control pl
    in
    let+ () = if t.use_fsync then Control.fsync t.control else Ok () in
    (* TODO: Flush staging table *)
    ()

  (** Flush stage 3 *)
  let flush_index_and_its_deps t =
    let open Result_syntax in
    let* () = flush_suffix_and_its_deps t in
    let+ () = Index.flush ~with_fsync:t.use_fsync t.index in
    ()

  (* Auto flushes *********************************************************** *)

  (** Is expected to be called by the dict when its append buffer is full so
      that the file manager flushes. *)
  let dict_requires_a_flush_exn t =
    match flush_dict t with
    | Ok () -> ()
    | Error _ ->
        (* TODO: Proper error *)
        assert false

  (** Is expected to be called by the suffix when its append buffer is full so
      that the file manager flushes. *)
  let suffix_requires_a_flush_exn t =
    match flush_suffix_and_its_deps t with
    | Ok () -> ()
    | Error _ ->
        (* TODO: Proper error *)
        assert false

  (** Is expected to be called by the index when its append buffer is full so
      that the dependendies of index are flushes. When the function returns,
      index will flush itself. *)
  let index_is_about_to_auto_flush_exn t =
    match flush_suffix_and_its_deps t with
    | Ok () -> ()
    | Error _ ->
        (* TODO: Proper error *)
        assert false

  (* Explicit flush ********************************************************* *)

  let flush t = flush_index_and_its_deps t

  let flush_exn t =
    match flush t with
    | Ok () -> ()
    | Error _ ->
        (* TODO: Proper exception *)
        assert false

  (* File creation ********************************************************** *)

  let finish_constructing_rw config control ~make_dict ~make_suffix ~make_index
      =
    let open Result_syntax in
    let root = Irmin_pack.Conf.root config in
    let use_fsync = Irmin_pack.Conf.use_fsync config in
    let pl : Payload.t = Control.payload control in
    let generation =
      match pl.status with
      | From_v1_v2_post_upgrade _ | From_v3_gc_disallowed -> 0
      | From_v3_gc_allowed x -> x.gc_generation
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
    Ok { dict; control; suffix; use_fsync; index }

  let create_control_file ~overwrite config pl =
    let root = Irmin_pack.Conf.root config in
    let path = Irmin_pack.Layout.V3.control ~root in
    Control.create_rw ~path ~overwrite pl

  (** Note on SWMR consistency: It is undefined for a reader to attempt an
      opening before [create_rw] is over.

      Note on crash consistency: Crashing during [create_rw] leaves the storage
      in an undefined state.

      Note on errors: If [create_rw] returns an error, the storage is left in an
      undefined state and some file descriptors might not be closed. *)
  let create_rw ~overwrite config =
    let open Result_syntax in
    let root = Irmin_pack.Conf.root config in
    let* () =
      match (overwrite, Io.classify_path root) with
      | _, (`File | `Other) -> Error `Not_a_directory
      | false, `Directory -> Error `File_exists
      | true, `Directory -> Ok ()
      | _, `No_such_file_or_directory -> Io.mkdir root
    in
    let* control =
      let open Payload in
      let status =
        From_v3_gc_allowed
          { entry_offset_suffix_start = Int63.zero; gc_generation = 0 }
      in
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
    let dead_header_size =
      match pl.status with
      | From_v1_v2_post_upgrade _ -> legacy_io_header_size
      | From_v3_gc_disallowed | From_v3_gc_allowed _ -> 0
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

  let read_offset_from_legacy_file path =
    let open Result_syntax in
    (* Bytes 0-7 contains the offset. Bytes 8-15 contain the version. *)
    let* io = Io.open_ ~path ~readonly:true in
    let* s = Io.read_to_string io ~off:Int63.zero ~len:8 in
    let* () = Io.close io in
    match Int63.of_string_opt s with
    | None -> Error `Corrupted_legacy_file
    | Some i -> Ok i

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

  (** Note on SWMR consistency: It is undefined for a reader to attempt and
      opening during an [open_rw].

      Note on crash consistency: If [open_rw] crashes during
      [open_rw_migrate_from_v1_v2], the storage is left in an undefined state.
      Otherwise the storage is unaffected.

      Note on errors: If [open_rw] returns an error during
      [open_rw_migrate_from_v1_v2], the storage is left in an undefined state.
      Otherwise the storage is unaffected. Anyhow, some file descriptors might
      not be closed. *)
  let open_rw config =
    let root = Irmin_pack.Conf.root config in
    match Io.classify_path root with
    | `File | `Other -> Error `Not_a_directory
    | `No_such_file_or_directory -> Error `No_such_file_or_directory
    | `Directory -> (
        let path = Irmin_pack.Layout.V3.control ~root in
        match Io.classify_path path with
        | `File -> open_rw_with_control_file config
        | `No_such_file_or_directory -> open_rw_no_control_file config
        | `Directory | `Other -> Error `Invalid_layout)

  (* Open ro **************************************************************** *)

  (** Note on SWMR consistency: TODO: doc

      Note on crash consistency: The storage is never mutated.

      Note on errors: The storage is never mutated. Some file descriptors might
      not be closed. *)
  let open_ro config =
    let open Result_syntax in
    let root = Irmin_pack.Conf.root config in
    let use_fsync = Irmin_pack.Conf.use_fsync config in
    (* 1. Open the control file *)
    let* control =
      let path = Irmin_pack.Layout.V3.control ~root in
      Control.open_ ~readonly:true ~path
    in
    let pl : Payload.t = Control.payload control in
    let dead_header_size =
      match pl.status with
      | From_v1_v2_post_upgrade _ -> legacy_io_header_size
      | From_v3_gc_disallowed | From_v3_gc_allowed _ -> 0
    in
    let generation =
      match pl.status with
      | From_v1_v2_post_upgrade _ | From_v3_gc_disallowed -> 0
      | From_v3_gc_allowed x -> x.gc_generation
    in
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
    Ok { dict; control; suffix; use_fsync; index }
end
