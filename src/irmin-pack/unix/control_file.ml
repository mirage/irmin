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

  let encode_bin = Repr.encode_bin t |> Repr.unstage
  let decode_bin = Repr.decode_bin t |> Repr.unstage

  let size_of_value =
    match Irmin.Type.Size.of_value t with
    | Irmin.Type.Size.Unknown -> assert false
    | Static x -> Fun.const x
    | Dynamic f -> f

  let size_of_encoding =
    match Irmin.Type.Size.of_encoding t with
    | Irmin.Type.Size.Unknown -> assert false
    | Static x -> fun _ _ -> x
    | Dynamic f -> f
end

module Version = struct
  type t = Irmin_pack.Version.t [@@deriving irmin ~encode_bin ~decode_bin]

  let size : int =
    match Irmin.Type.Size.of_value t with
    | Irmin.Type.Size.Static v -> v
    | _ -> assert false
end

module Data = struct
  (** Type of what's encoded in the control file. The boilerplate around [bin]
      makes the variant tag to be encoded as a [Version.t]. *)
  type t = V3 of Plv3.t [@@deriving irmin]

  let encode_bin t f =
    match t with
    | V3 payload ->
        Version.encode_bin `V3 f;
        Plv3.encode_bin payload f

  let decode_bin s offref =
    let version = Version.decode_bin s offref in
    match version with
    | `V1 | `V2 ->
        (* These versions have never been written to control file *)
        assert false
    | `V3 -> V3 (Plv3.decode_bin s offref)

  let of_value = function V3 pl -> Version.size + Plv3.size_of_value pl

  let of_encoding s off =
    let version = Version.decode_bin s (ref off) in
    match version with
    | `V1 | `V2 ->
        (* These versions have never been written to control file *)
        assert false
    | `V3 -> Version.size + Plv3.size_of_encoding s off

  let size_of = Irmin.Type.Size.custom_dynamic ~of_value ~of_encoding ()
  let bin = (encode_bin, decode_bin, size_of)
  let t = Irmin.Type.like t ~bin
  let of_bin_string = Irmin.Type.of_bin_string t |> Irmin.Type.unstage
  let to_bin_string = Irmin.Type.to_bin_string t |> Irmin.Type.unstage
end

module Make (Io : Io.S) = struct
  module Io = Io

  type t = { io : Io.t; mutable payload : Latest_payload.t }

  let write io payload =
    let s = Data.(to_bin_string (V3 payload)) in

    (* The data must fit inside a single page for atomic updates of the file *)
    assert (String.length s <= Io.page_size);

    Io.write_string io ~off:Int63.zero s

  let read io =
    let open Result_syntax in
    let* len = Io.read_size io in
    let len = Int63.to_int len in
    let* string = Io.read_to_string io ~off:Int63.zero ~len in
    match Data.of_bin_string string with
    | Ok _ as ok -> ok
    | Error (`Msg _msg) -> Error `Corrupted_control_file

  let read io =
    match read io with
    | Ok x -> Ok x
    | Error (`Read_out_of_bounds | `Corrupted_control_file) ->
        Error `Corrupted_control_file
    | Error `Invalid_argument -> assert false
    | Error (`Io_misc _ | `Read_on_closed) as e -> e

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
