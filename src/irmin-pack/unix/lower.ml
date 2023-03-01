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
  module Sparse = Sparse_file.Make (Io)

  type t =
    | Empty of { path : string }
    | Nonempty of {
        path : string;
        mutable readonly : bool;
        control : Control_file.Payload.Volume.Latest.t;
        mutable sparse : Sparse.t option;
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
    Ok
      (let path = volume_path in
       match control with
       | None -> Empty { path }
       | Some control -> Nonempty { readonly; path; control; sparse = None })

  let set_readonly ~readonly = function
    | Empty { path } -> Fmt.failwith "set_readonly for empty volume %s" path
    | Nonempty t ->
        if t.readonly = readonly then Ok ()
        else
          (* TODO determine how this flag works once we have a read-write
             volume in GC *)
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
    | Nonempty ({ path = root; sparse; _ } as t) -> (
        match sparse with
        | Some _ -> Ok () (* Sparse file is already open *)
        | None ->
            let open Result_syntax in
            let mapping = Irmin_pack.Layout.V5.Volume.mapping ~root in
            let data = Irmin_pack.Layout.V5.Volume.data ~root in
            let+ sparse = Sparse.open_ro ~mapping ~data in
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
    | Empty _ -> ()
    | Nonempty { sparse; _ } -> (
        match sparse with
        | None -> Errs.raise_error `Closed
        | Some s -> Sparse.read_range_exn s ~off ~min_len ~max_len b)
end

module Make (Io : Io.S) (Errs : Io_errors.S with module Io = Io) = struct
  module Io = Io
  module Errs = Errs
  module Volume = Make_volume (Io) (Errs)

  type t = {
    root : string;
    readonly : bool;
    mutable volumes : Volume.t array;
    mutable open_volume : Volume.t option;
  }

  type open_error = [ Volume.open_error | `Volume_missing of string ]
  type close_error = [ | Io.close_error ]

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
    load_volumes ~volume_num
      { root; readonly; volumes = [||]; open_volume = None }

  let reload ~volume_num t =
    let open Result_syntax in
    let* _ = load_volumes ~volume_num t in
    Ok ()

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

  let find_volume ~off t = Array.find_opt (Volume.contains ~off) t.volumes

  let find_volume_by_offset_exn ~off t =
    match find_volume ~off t with
    | None ->
        let err = Fmt.str "Looking for offset %d" (Int63.to_int off) in
        Errs.raise_error (`Volume_not_found err)
    | Some v -> v

  let find_volume_by_identifier_exn ~id t =
    match Array.find_opt (Volume.identifier_eq ~id) t.volumes with
    | None ->
        let err = Fmt.str "Looking for identifier %s" id in
        Errs.raise_error (`Volume_not_found err)
    | Some v -> v

  let read_range_exn ~off ~min_len ~max_len ?volume t b =
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
    Volume.read_range_exn ~off ~min_len ~max_len b volume;
    Volume.identifier volume

  let read_exn ~off ~len ?volume t b =
    read_range_exn ~off ~min_len:len ~max_len:len ?volume t b
end
