(*
 * Copyright (c) 2023 Tarides <contact@tarides.com>
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
include Lower_intf
module Layout = Irmin_pack.Layout.V5.Volume
module Payload = Control_file.Payload.Volume.Latest

module Make_volume (Io : Io.S) (Errs : Io_errors.S with module Io = Io) = struct
  module Io = Io
  module Errs = Errs
  module Control = Control_file.Volume (Io)
  module Sparse = Sparse_file.Make (Io)

  type t =
    | Empty of { path : string }
    | Nonempty of {
        path : string;
        control : Payload.t;
        mutable sparse : Sparse.t option;
      }

  type open_error =
    [ Io.open_error
    | `Closed
    | `Double_close
    | `Corrupted_control_file of string
    | `Unknown_major_pack_version of string ]

  let v volume_path =
    let open Result_syntax in
    let* control =
      let path = Layout.control ~root:volume_path in
      match Io.classify_path path with
      | `File ->
          let+ payload = Control.read_payload ~path in
          Some payload
      | `Directory | `Other | `No_such_file_or_directory -> Ok None
    in
    Ok
      (let path = volume_path in
       match control with
       | None -> Empty { path }
       | Some control -> Nonempty { path; control; sparse = None })

  let create_empty volume_path =
    let open Result_syntax in
    (* 0. Validate volume directory does not already exist *)
    let* () =
      match Io.classify_path volume_path with
      | `Directory | `File | `Other -> Error (`File_exists volume_path)
      | `No_such_file_or_directory -> Ok ()
    in
    (* 1. Make directory *)
    let* () = Io.mkdir volume_path in
    (* 2. Make empty mapping *)
    let* () =
      Io.create ~path:(Layout.mapping ~root:volume_path) ~overwrite:true
      >>= Io.close
    in
    (* 3. Make empty data *)
    let* () =
      Io.create ~path:(Layout.data ~root:volume_path) ~overwrite:true
      >>= Io.close
    in
    (* TODO: handle failure to create all artifacts, either here or in a cleanup
       when the store starts. *)
    v volume_path

  let create_from ~src ~dead_header_size ~size lower_root =
    let open Result_syntax in
    let root = Layout.directory ~root:lower_root ~idx:0 in
    let data = Layout.data ~root in
    let mapping = Layout.mapping ~root in
    let* () = Io.mkdir root in
    let* () = Io.move_file ~src ~dst:data in
    let* mapping_end_poff =
      Sparse.Wo.create_from_data ~mapping ~dead_header_size ~size ~data
    in
    let payload =
      {
        Payload.start_offset = Int63.zero;
        end_offset = size;
        mapping_end_poff;
        checksum = Int63.zero;
      }
    in
    let control = Layout.control ~root in
    Control.create_rw ~path:control ~tmp_path:None ~overwrite:false payload
    >>= Control.close

  let path = function Empty { path } -> path | Nonempty { path; _ } -> path

  let control = function
    | Empty _ -> None
    | Nonempty { control; _ } -> Some control

  let is_empty = function Empty _ -> true | Nonempty _ -> false

  let contains ~off = function
    | Empty _ -> false
    | Nonempty { control; _ } ->
        let open Int63.Syntax in
        control.start_offset <= off && off < control.end_offset

  let open_ = function
    | Empty _ -> Ok () (* Opening an empty volume is a no-op *)
    | Nonempty ({ path = root; sparse; control; _ } as t) -> (
        match sparse with
        | Some _ -> Ok () (* Sparse file is already open *)
        | None ->
            let open Result_syntax in
            let mapping = Layout.mapping ~root in
            let data = Layout.data ~root in
            let mapping_size = Int63.to_int control.Payload.mapping_end_poff in
            let+ sparse = Sparse.open_ro ~mapping_size ~mapping ~data in
            t.sparse <- Some sparse)

  let close = function
    | Empty _ -> Ok () (* Closing an empty volume is a no-op *)
    | Nonempty ({ sparse; _ } as t) -> (
        match sparse with
        | None -> Error `Double_close
        | Some s ->
            let open Result_syntax in
            let+ () = Sparse.close s in
            t.sparse <- None)

  let identifier t = path t
  let identifier_eq ~id t = String.equal (identifier t) id
  let eq a b = identifier_eq ~id:(identifier b) a

  let read_range_exn ~off ~min_len ~max_len b = function
    | Empty _ -> Errs.raise_error (`Invalid_volume_read (`Empty, off))
    | Nonempty { sparse; _ } -> (
        match sparse with
        | None -> Errs.raise_error (`Invalid_volume_read (`Closed, off))
        | Some s -> Sparse.read_range_exn s ~off ~min_len ~max_len b)

  let archive_seq ~upper_root ~generation ~is_first ~to_archive ~first_off t =
    let open Result_syntax in
    let root = path t in
    let* () =
      match t with
      | Empty _ -> Ok ()
      | Nonempty { control; _ } ->
          if control.end_offset <= first_off then Ok ()
          else
            Error
              (`Volume_history_newer_than_archived_data
                (control.end_offset, first_off))
    in
    let mapping = Irmin_pack.Layout.V5.Volume.mapping ~root in
    let data = Irmin_pack.Layout.V5.Volume.data ~root in
    let* mapping_size =
      match t with
      | Empty _ when is_first -> Ok Int63.zero
      | Empty _ ->
          (* If this is a new volume (and not the first),
             copy pre-GC prefix/mapping as new volume *)
          let old_generation = pred generation in
          let old_mapping =
            Irmin_pack.Layout.V5.mapping ~root:upper_root
              ~generation:old_generation
          in
          let old_prefix =
            Irmin_pack.Layout.V5.prefix ~root:upper_root
              ~generation:old_generation
          in
          let* () = Io.copy_file ~src:old_prefix ~dst:data in
          let* () = Io.copy_file ~src:old_mapping ~dst:mapping in
          Io.size_of_path mapping
      | Nonempty { control; _ } -> Ok control.mapping_end_poff
    in
    (* Append archived data *)
    let* ao = Sparse.Ao.open_ao ~mapping_size ~mapping ~data in
    List.iter
      (fun (off, seq) -> Sparse.Ao.append_seq_exn ao ~off seq)
      to_archive;
    let end_offset = Sparse.Ao.end_off ao in
    let mapping_end_poff = Sparse.Ao.mapping_size ao in
    let* () = Sparse.Ao.flush ao in
    let* () = Sparse.Ao.close ao in
    (* Prepare new control file *)
    let start_offset =
      match t with
      | Empty _ -> first_off
      | Nonempty { control; _ } -> control.start_offset
    in
    let new_control =
      Control_file.Payload.Volume.V5.
        { start_offset; end_offset; mapping_end_poff; checksum = Int63.zero }
    in
    (* Write into temporary file on disk *)
    let control_gc_tmp =
      Irmin_pack.Layout.V5.Volume.control_gc_tmp ~generation ~root
    in
    let* c =
      Control.create_rw ~path:control_gc_tmp ~tmp_path:None ~overwrite:true
        new_control
    in
    let* () = Control.close c in
    Ok (identifier t)

  let archive_seq ~upper_root ~generation ~is_first ~to_archive t =
    match to_archive with
    | [] ->
        [%log.warn
          "Lower.archive_seq: Nothing to archive! volume=%S generation=%i \
           is_first=%b"
          (identifier t) generation is_first];
        Ok (identifier t)
    | (first_off, _) :: _ ->
        archive_seq ~upper_root ~generation ~is_first ~to_archive ~first_off t

  let swap ~generation t =
    let root = path t in
    let control_tmp =
      Irmin_pack.Layout.V5.Volume.control_gc_tmp ~generation ~root
    in
    let control = Irmin_pack.Layout.V5.Volume.control ~root in
    match Io.classify_path control_tmp with
    | `File -> Io.move_file ~src:control_tmp ~dst:control
    | `No_such_file_or_directory ->
        [%log.info "No tmp volume control file to swap. %s" control];
        Ok ()
    | `Directory | `Other -> assert false

  let cleanup ~generation t =
    let clean filename =
      match Irmin_pack.Layout.Classification.Volume.v filename with
      | `Control_tmp g when g = generation -> swap ~generation t
      | `Control_tmp g when g <> generation ->
          Io.unlink filename
          |> Errs.log_if_error (Printf.sprintf "unlink %s" filename)
          |> Result.ok
      | _ -> Ok ()
    in
    path t |> Sys.readdir |> Array.to_list |> List.iter_result clean
end

module Make (Io : Io.S) (Errs : Io_errors.S with module Io = Io) = struct
  module Io = Io
  module Errs = Errs
  module Volume = Make_volume (Io) (Errs)

  type t = {
    root : string;
    mutable readonly : bool;
    mutable volumes : Volume.t array;
    mutable open_volume : Volume.t option;
  }

  type open_error = [ Volume.open_error | `Volume_missing of string ]
  type close_error = [ | Io.close_error ]
  type nonrec volume_identifier = volume_identifier [@@deriving irmin]

  type add_error =
    [ open_error
    | `Ro_not_allowed
    | `Multiple_empty_volumes
    | `File_exists of string
    | `Invalid_parent_directory ]

  let close_open_volume t =
    match t.open_volume with
    | None -> Ok ()
    | Some v ->
        let open Result_syntax in
        let+ _ = Volume.close v in
        t.open_volume <- None

  exception LoadVolumeError of open_error

  let load_volumes ~volume_num t =
    let open Result_syntax in
    let* () = close_open_volume t in
    let* volumes =
      let root = t.root in
      let volume i =
        let path = Layout.directory ~root ~idx:i in
        match Io.classify_path path with
        | `File | `Other | `No_such_file_or_directory ->
            raise (LoadVolumeError (`Volume_missing path))
        | `Directory -> (
            match Volume.v path with
            | Error e -> raise (LoadVolumeError e)
            | Ok v -> v)
      in
      try Ok (Array.init volume_num volume)
      with LoadVolumeError err -> Error (err : open_error :> [> open_error ])
    in
    t.volumes <- volumes;
    Ok t

  let v ~readonly ~volume_num root =
    load_volumes ~volume_num
      { root; readonly; volumes = [||]; open_volume = None }

  let reload ~volume_num t =
    let open Result_syntax in
    let* _ = load_volumes ~volume_num t in
    Ok ()

  let set_readonly t flag = t.readonly <- flag
  let close = close_open_volume
  let volume_num t = Array.length t.volumes

  let appendable_volume t =
    match volume_num t with 0 -> None | num -> Some t.volumes.(num - 1)

  let add_volume t =
    let open Result_syntax in
    let* () = if t.readonly then Error `Ro_not_allowed else Ok () in
    let* () =
      match appendable_volume t with
      | None -> Ok ()
      | Some v ->
          if Volume.is_empty v then Error `Multiple_empty_volumes else Ok ()
    in
    let volume_path =
      let next_idx = volume_num t in
      Layout.directory ~root:t.root ~idx:next_idx
    in
    let* vol = Volume.create_empty volume_path in
    t.volumes <- Array.append t.volumes [| vol |];
    Ok vol

  let find_volume ~off t = Array.find_opt (Volume.contains ~off) t.volumes

  let find_volume_by_offset_exn ~off t =
    match find_volume ~off t with
    | None ->
        let err = Fmt.str "Looking for offset %d" (Int63.to_int off) in
        Errs.raise_error (`Volume_not_found err)
    | Some v -> v

  let find_volume_by_identifier ~id t =
    match Array.find_opt (Volume.identifier_eq ~id) t.volumes with
    | None ->
        let err = Fmt.str "Looking for identifier %s" id in
        Error (`Volume_not_found err)
    | Some v -> Ok v

  let find_volume_by_identifier_exn ~id t =
    find_volume_by_identifier ~id t |> Errs.raise_if_error

  let read_range_exn ~off ~min_len ~max_len ?volume t b =
    [%log.debug
      "read_range_exn ~off:%a ~min_len:%i ~max_len:%i" Int63.pp off min_len
        max_len];
    let set_open_volume t v =
      (* Maintain one open volume at a time. *)
      let open Result_syntax in
      let* () =
        match t.open_volume with
        | None -> Ok ()
        | Some v0 -> if Volume.eq v0 v then Ok () else close_open_volume t
      in
      let+ _ = Volume.open_ v in
      t.open_volume <- Some v
    in
    let volume =
      match volume with
      | None -> find_volume_by_offset_exn t ~off
      | Some id -> find_volume_by_identifier_exn t ~id
    in
    set_open_volume t volume |> Errs.raise_if_error;
    let len = Volume.read_range_exn ~off ~min_len ~max_len b volume in
    (len, Volume.identifier volume)

  let archive_seq_exn ~upper_root ~generation ~to_archive t =
    Errs.raise_if_error
      (let open Result_syntax in
      let* () = if t.readonly then Error `Ro_not_allowed else Ok () in
      let* v =
        match appendable_volume t with
        | None -> Error `Lower_has_no_volume
        | Some v -> Ok v
      in
      let* () =
        match t.open_volume with
        | None -> Ok ()
        | Some v0 -> if Volume.eq v0 v then close_open_volume t else Ok ()
      in
      let is_first = volume_num t = 1 in
      Volume.archive_seq ~upper_root ~generation ~to_archive ~is_first v)

  let read_exn ~off ~len ?volume t b =
    let _, volume = read_range_exn ~off ~min_len:len ~max_len:len ?volume t b in
    volume

  let create_from = Volume.create_from

  let swap ~volume ~generation ~volume_num t =
    let open Result_syntax in
    let* vol = find_volume_by_identifier ~id:volume t in
    let* () = Volume.swap ~generation vol in
    reload ~volume_num t

  let cleanup ~generation t =
    match appendable_volume t with
    | None -> Ok [%log.warn "Attempted to cleanup but lower has no volumes"]
    | Some v -> Volume.cleanup ~generation v
end
