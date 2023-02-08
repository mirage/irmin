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

module Make_volume (Io : Io.S) (Errs : Io_errors.S with module Io = Io) = struct
  module Io = Io
  module Errs = Errs
  module Control = Control_file.Volume (Io)

  type t = {
    mutable readonly : bool;
    path : string;
    control : Control_file.Payload.Volume.Latest.t option;
  }

  type open_error =
    [ Io.open_error
    | `Closed
    | `Double_close
    | `Corrupted_control_file
    | `Unknown_major_pack_version of string ]

  let v ~readonly volume_path =
    let open Result_syntax in
    let* control =
      let path = Irmin_pack.Layout.V5.Volume.control ~root:volume_path in
      match Io.classify_path path with
      | `File ->
          let+ payload = Control.read_payload ~path in
          Some payload
      | `Directory | `Other | `No_such_file_or_directory -> Ok None
    in
    Ok { readonly; path = volume_path; control }

  let set_readonly ~readonly t =
    if t.readonly = readonly then Ok ()
    else
      (* TODO actually reopen based on readonly flag once
         sparse file is in place *)
      Ok (t.readonly <- readonly)

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
      Irmin_pack.Layout.V5.Volume.(
        Io.create ~path:(mapping ~root:volume_path) ~overwrite:true)
      >>= Io.close
    in
    (* 3. Make empty data *)
    let* () =
      Irmin_pack.Layout.V5.Volume.(
        Io.create ~path:(data ~root:volume_path) ~overwrite:true)
      >>= Io.close
    in
    (* TODO: handle failure to create all artifacts, either here or in a cleanup
       when the store starts. *)
    v ~readonly:false volume_path

  let path t = t.path
  let control t = t.control
  let empty_state t = if Option.is_none t.control then `Empty t else `Nonempty t

  let is_empty t =
    match empty_state t with `Empty _ -> true | `Nonempty _ -> false

  let nonempty_control (`Nonempty t) =
    match t.control with
    | None -> failwith "Nonempty volume has no control"
    | Some cf -> cf

  let start_offset t = (nonempty_control t).start_offset
  let end_offset t = (nonempty_control t).end_offset

  let contains ~offset t =
    match empty_state t with
    | `Empty _ -> false
    | `Nonempty _ as t ->
        let open Int63.Syntax in
        start_offset t <= offset && offset < end_offset t
end

module Make (Io : Io.S) (Errs : Io_errors.S with module Io = Io) = struct
  module Io = Io
  module Errs = Errs
  module Volume = Make_volume (Io) (Errs)

  type t = { root : string; readonly : bool; mutable volumes : Volume.t array }
  type open_error = [ Volume.open_error | `Volume_missing of string ]
  type close_error = [ | Io.close_error ]

  type add_error =
    [ open_error
    | `Ro_not_allowed
    | `Multiple_empty_volumes
    | `File_exists of string
    | `Invalid_parent_directory ]

  exception LoadVolumeError of open_error

  let load_volumes ~volume_num t =
    let open Result_syntax in
    let* volumes =
      let root = t.root in
      let volume i =
        let readonly = t.readonly || i < volume_num - 1 in
        let path = Irmin_pack.Layout.V5.Volume.directory ~root ~idx:i in
        match Io.classify_path path with
        | `File | `Other | `No_such_file_or_directory ->
            raise (LoadVolumeError (`Volume_missing path))
        | `Directory -> (
            match Volume.v ~readonly path with
            | Error e -> raise (LoadVolumeError e)
            | Ok v -> v)
      in
      try Ok (Array.init volume_num volume)
      with LoadVolumeError err -> Error (err : open_error :> [> open_error ])
    in
    t.volumes <- volumes;
    Ok t

  let v ~readonly ~volume_num root =
    load_volumes ~volume_num { root; readonly; volumes = [||] }

  let reload ~volume_num t =
    let open Result_syntax in
    let* _ = load_volumes ~volume_num t in
    Ok ()

  let close _t =
    (* TODO: update when actual fds are kept open *)
    Ok ()

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
          if Volume.is_empty v then Error `Multiple_empty_volumes
          else Volume.set_readonly ~readonly:true v
    in
    let volume_path =
      let next_idx = volume_num t in
      Irmin_pack.Layout.V5.Volume.directory ~root:t.root ~idx:next_idx
    in
    let* vol = Volume.create_empty volume_path in
    t.volumes <- Array.append t.volumes [| vol |];
    Ok vol

  let find_volume ~offset t = Array.find_opt (Volume.contains ~offset) t.volumes
end
