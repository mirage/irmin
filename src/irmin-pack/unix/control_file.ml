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
include Control_file_intf

module Plv3 = struct
  include Payload_v3

  let of_bin_string = Irmin.Type.of_bin_string t |> Irmin.Type.unstage
  let to_bin_string = Irmin.Type.to_bin_string t |> Irmin.Type.unstage
end

module Version = Irmin_pack.Version

module Data (Io : Io.S) = struct
  (** Type of what's encoded in the control file. The variant tag is encoded as
      a [Version.t]. *)
  type t = V3 of Plv3.t

  let to_bin_string = function
    | V3 payload -> Version.to_bin `V3 ^ Plv3.to_bin_string payload

  let of_bin_string s =
    let open Result_syntax in
    let len = String.length s in
    let* left, right =
      try Ok (String.sub s 0 8, String.sub s 8 (len - 8))
      with Invalid_argument _ -> Error `Corrupted_control_file
    in
    let* version =
      match Version.of_bin left with
      | None -> Error (`Unknown_major_pack_version left)
      | Some `V3 when len > Io.page_size -> Error `Corrupted_control_file
      | Some `V3 -> Ok `V3
      | Some (`V1 | `V2) -> assert false
    in
    match version with
    | `V3 -> (
        match Plv3.of_bin_string right with
        | Ok x -> Ok (V3 x)
        | Error _ -> Error `Corrupted_control_file)
end

module Make (Io : Io.S) = struct
  module Io = Io
  module Data = Data (Io)

  type t = { io : Io.t; mutable payload : Latest_payload.t }

  let write io payload =
    let s = Data.(to_bin_string (V3 payload)) in

    (* The data must fit inside a single page for atomic updates of the file *)
    assert (String.length s <= Io.page_size);

    Io.write_string io ~off:Int63.zero s

  let read io =
    let open Result_syntax in
    let* string = Io.read_all_to_string io in
    (* Since the control file is expected to fit in a page,
       [read_all_to_string] is atomic.

       If [string] is larger than a page, it either means that the file is
       corrupted or that the major version is not supported. Either way it will
       be handled by [Data.of_bin_string]. *)
    Data.of_bin_string string

  let create_rw ~path ~overwrite payload =
    let open Result_syntax in
    let* io = Io.create ~path ~overwrite in
    let+ () = write io payload in
    { io; payload }

  let open_ ~path ~readonly =
    let open Result_syntax in
    let* io = Io.open_ ~path ~readonly in
    let+ data = read io in
    let payload = match data with Data.V3 payload -> payload in
    { io; payload }

  let close t = Io.close t.io
  let readonly t = Io.readonly t.io
  let payload t = t.payload

  let reload t =
    let open Result_syntax in
    if not @@ Io.readonly t.io then Error `Rw_not_allowed
    else
      let+ data = read t.io in
      let payload = match data with Data.V3 payload -> payload in
      t.payload <- payload

  let set_payload t payload =
    let open Result_syntax in
    let+ () = write t.io payload in
    t.payload <- payload

  let fsync t = Io.fsync t.io
end
