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
module Version = Irmin_pack.Version

module Checksum = struct
  let calculate ~encode_bin ~set_checksum ~payload =
    let open Checkseum in
    let result = ref Adler32.default in
    encode_bin (set_checksum payload Int63.zero) (fun str ->
        result := Adler32.digest_string str 0 (String.length str) !result);
    Int63.of_int (Optint.to_int !result)

  let calculate_and_set ~encode_bin ~set_checksum ~payload =
    calculate ~encode_bin ~set_checksum ~payload |> set_checksum payload

  let is_valid ~encode_bin ~set_checksum ~get_checksum ~payload =
    Int63.equal
      (calculate ~encode_bin ~set_checksum ~payload)
      (get_checksum payload)
end

module Serde = struct
  module type S = sig
    type payload

    val of_bin_string :
      string ->
      ( payload,
        [> `Corrupted_control_file | `Unknown_major_pack_version of string ] )
      result

    val to_bin_string : payload -> string
  end

  let extract_version_and_payload s =
    let open Result_syntax in
    let len = String.length s in
    let* left, right =
      try Ok (String.sub s 0 8, String.sub s 8 (len - 8))
      with Invalid_argument _ -> Error `Corrupted_control_file
    in
    let+ version =
      match Version.of_bin left with
      | None -> Error (`Unknown_major_pack_version left)
      | Some (`V1 | `V2) -> assert false (* TODO: create specific error *)
      | Some ((`V3 | `V4) as x) ->
          if len > Io.Unix.page_size then
            (* TODO: make this a more specific error *)
            Error `Corrupted_control_file
          else Ok x
    in
    (version, right)

  module Upper : S with type payload = Payload.Upper.Latest.t = struct
    module Data = struct
      module Plv3 = struct
        include Payload.Upper.V3

        let of_bin_string = Irmin.Type.of_bin_string t |> Irmin.Type.unstage
      end

      module Plv4 = struct
        include Payload.Upper.V4

        let of_bin_string = Irmin.Type.of_bin_string t |> Irmin.Type.unstage
        let to_bin_string = Irmin.Type.to_bin_string t |> Irmin.Type.unstage
      end

      (** Type of what's encoded in the upper layer control file. The variant
          tag is encoded as a [Version.t]. *)
      type t = V3 of Plv3.t | V4 of Plv4.t

      let to_bin_string = function
        | V3 _ -> assert false
        | V4 payload -> Version.to_bin `V4 ^ Plv4.to_bin_string payload

      let of_bin_string s =
        let open Result_syntax in
        let* version, payload = extract_version_and_payload s in
        match version with
        | `V3 -> (
            match Plv3.of_bin_string payload with
            | Ok x -> Ok (V3 x)
            | Error _ -> Error `Corrupted_control_file)
        | `V4 -> (
            match Plv4.of_bin_string payload with
            | Ok x -> Ok (V4 x)
            | Error _ -> Error `Corrupted_control_file)
    end

    module Latest = Data.Plv4

    type payload = Latest.t

    let upgrade_from_v3 (pl : Payload.Upper.V3.t) : payload =
      let chunk_start_idx = ref 0 in
      let status =
        match pl.status with
        | From_v1_v2_post_upgrade x -> Latest.From_v1_v2_post_upgrade x
        | From_v3_no_gc_yet -> No_gc_yet
        | From_v3_used_non_minimal_indexing_strategy ->
            Used_non_minimal_indexing_strategy
        | From_v3_gced x ->
            chunk_start_idx := x.generation;
            Gced
              {
                suffix_start_offset = x.suffix_start_offset;
                generation = x.generation;
                latest_gc_target_offset = x.suffix_start_offset;
                suffix_dead_bytes = Int63.zero;
              }
        | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13
        | T14 | T15 ->
            (* Unreachable *)
            assert false
      in
      {
        dict_end_poff = pl.dict_end_poff;
        (* When upgrading from v3 to v4, there is only one (appendable) chunk,
           which is the existing suffix, so we set the new [appendable_chunk_poff]
           to [pl.suffix_end_poff]. *)
        appendable_chunk_poff = pl.suffix_end_poff;
        status;
        upgraded_from_v3_to_v4 = true;
        checksum = Int63.zero;
        chunk_start_idx = !chunk_start_idx;
        chunk_num = 1;
      }

    let checksum_encode_bin = Irmin.Type.(unstage (pre_hash Latest.t))
    let set_checksum payload checksum = Latest.{ payload with checksum }
    let get_checksum payload = payload.Latest.checksum

    let of_bin_string string =
      let open Result_syntax in
      let* payload = Data.of_bin_string string in
      match payload with
      | V3 payload -> Ok (upgrade_from_v3 payload)
      | V4 payload ->
          if
            Checksum.is_valid ~payload ~encode_bin:checksum_encode_bin
              ~set_checksum ~get_checksum
          then Ok payload
          else Error `Corrupted_control_file

    let to_bin_string payload =
      let payload =
        Checksum.calculate_and_set ~encode_bin:checksum_encode_bin ~set_checksum
          ~payload
      in
      let s = Data.(to_bin_string (V4 payload)) in
      s
  end
end

module Make (Serde : Serde.S) (Io : Io.S) = struct
  module Io = Io

  type payload = Serde.payload
  type t = { io : Io.t; mutable payload : payload }

  let write io payload =
    let s = Serde.to_bin_string payload in
    (* The data must fit inside a single page for atomic updates of the file.
       This is only true for some file systems. This system will have to be
       reworked for [V4]. *)
    assert (String.length s <= Io.page_size);

    Io.write_string io ~off:Int63.zero s

  let read io =
    let open Result_syntax in
    let* string = Io.read_all_to_string io in
    (* Since the control file is expected to fit in a page,
       [read_all_to_string] should be atomic for most filesystems. *)
    Serde.of_bin_string string

  let create_rw ~path ~overwrite (payload : payload) =
    let open Result_syntax in
    let* io = Io.create ~path ~overwrite in
    let+ () = write io payload in
    { io; payload }

  let open_ ~path ~readonly =
    let open Result_syntax in
    let* io = Io.open_ ~path ~readonly in
    let+ payload = read io in
    { io; payload }

  let close t = Io.close t.io
  let readonly t = Io.readonly t.io
  let payload t = t.payload

  let reload t =
    let open Result_syntax in
    if not @@ Io.readonly t.io then Error `Rw_not_allowed
    else
      let+ payload = read t.io in
      t.payload <- payload

  let set_payload t payload =
    let open Result_syntax in
    let+ () = write t.io payload in
    t.payload <- payload

  let fsync t = Io.fsync t.io
end

module Upper = Make (Serde.Upper)
