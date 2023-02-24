(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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
include Dispatcher_intf
module Payload = Control_file.Payload.Upper.Latest

(* The following [with module Io = Io.Unix] forces unix *)
module Make (Fm : File_manager.S with module Io = Io.Unix) :
  S with module Fm = Fm = struct
  module Fm = Fm
  module Io = Fm.Io
  module Suffix = Fm.Suffix
  module Sparse = Fm.Sparse
  module Errs = Fm.Errs
  module Control = Fm.Control

  type t = { fm : Fm.t }

  let v fm =
    let t = { fm } in
    Ok t

  let get_prefix t =
    match Fm.prefix t.fm with
    | Some prefix -> prefix
    | None -> raise (Errors.Pack_error (`Invalid_prefix_read "no prefix found"))

  let get_suffix t = Fm.suffix t.fm

  let suffix_start_offset t =
    let pl = Control.payload (Fm.control t.fm) in
    match pl.status with
    | Payload.From_v1_v2_post_upgrade _ | Used_non_minimal_indexing_strategy
    | No_gc_yet ->
        Int63.zero
    | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
    | T15 ->
        assert false
    | Gced { suffix_start_offset; _ } -> suffix_start_offset

  let suffix_dead_bytes t =
    let pl = Control.payload (Fm.control t.fm) in
    match pl.status with
    | Payload.From_v1_v2_post_upgrade _ | Used_non_minimal_indexing_strategy
    | No_gc_yet ->
        Int63.zero
    | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
    | T15 ->
        assert false
    | Gced { suffix_dead_bytes; _ } -> suffix_dead_bytes

  (* Adjust the read in suffix, as the global offset [off] is
     [off] = [suffix_start_offset] + [soff] - [suffix_dead_bytes]. *)
  let soff_of_offset t off =
    let open Int63.Syntax in
    let suffix_start_offset = suffix_start_offset t in
    let suffix_dead_bytes = suffix_dead_bytes t in
    off - suffix_start_offset + suffix_dead_bytes

  let offset_of_soff t soff =
    let open Int63.Syntax in
    let suffix_start_offset = suffix_start_offset t in
    let suffix_dead_bytes = suffix_dead_bytes t in
    suffix_start_offset + soff - suffix_dead_bytes

  let end_offset t =
    let end_soff = Suffix.end_soff (Fm.suffix t.fm) in
    offset_of_soff t end_soff

  let dispatch_suffix t ~off =
    let open Int63.Syntax in
    if off >= suffix_start_offset t then Some (soff_of_offset t off) else None

  let read_exn t ~off ~len buf =
    [%log.debug "read_exn ~off:%a ~len:%i" Int63.pp off len];
    match dispatch_suffix t ~off with
    | Some off -> Suffix.read_exn (get_suffix t) ~off ~len buf
    | None -> Sparse.read_exn (get_prefix t) ~off ~len buf

  let read_range_exn t ~off ~min_len ~max_len buf =
    match dispatch_suffix t ~off with
    | Some off ->
        Suffix.read_range_exn (Fm.suffix t.fm) ~off ~min_len ~max_len buf
    | None -> Sparse.read_range_exn (get_prefix t) ~off ~min_len ~max_len buf

  let read_seq_exn t ~off ~len =
    let bytes_in_prefix =
      let open Int63.Syntax in
      let prefix_bytes_after_off = suffix_start_offset t - off in
      if prefix_bytes_after_off <= Int63.zero then Int63.zero
      else min len prefix_bytes_after_off
    in
    let bytes_in_suffix =
      let open Int63.Syntax in
      if bytes_in_prefix < len then len - bytes_in_prefix else Int63.zero
    in
    assert (Int63.Syntax.(bytes_in_prefix + bytes_in_suffix = len));
    let len = Int63.to_int len in
    let max_read_size = min 8192 len in
    let buffer = Bytes.create max_read_size in
    let rec aux read_exn ~off ~len () =
      if len <= 0 then Seq.Nil
      else
        let read_len = min len max_read_size in
        read_exn ~off ~len:read_len buffer;
        Seq.Cons
          ( Bytes.sub_string buffer 0 read_len,
            aux read_exn
              ~off:Int63.Syntax.(off + Int63.of_int read_len)
              ~len:(len - read_len) )
    in
    let prefix =
      if bytes_in_prefix <= Int63.zero then Seq.empty
      else
        aux
          (Sparse.read_exn (get_prefix t))
          ~off
          ~len:(Int63.to_int bytes_in_prefix)
    in
    let suffix =
      if bytes_in_suffix <= Int63.zero then Seq.empty
      else
        let off = Int63.Syntax.(off + bytes_in_prefix) in
        let off = soff_of_offset t off in
        aux
          (Suffix.read_exn (get_suffix t))
          ~off
          ~len:(Int63.to_int bytes_in_suffix)
    in
    Seq.append prefix suffix

  let read_bytes_exn t ~f ~off ~len = Seq.iter f (read_seq_exn t ~off ~len)

  let next_valid_offset t ~off =
    let open Int63.Syntax in
    match dispatch_suffix t ~off with
    | Some soff when soff >= Suffix.end_soff (get_suffix t) -> None
    | Some _ -> Some off
    | None -> (
        match Sparse.next_valid_offset (get_prefix t) ~off with
        | None -> Some (suffix_start_offset t)
        | some_off -> some_off)
end
