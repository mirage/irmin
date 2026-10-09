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

open! Import
include Dict_intf

module Make (Io : Io_intf.S) = struct
  module Io = Io
  module Errs = Io_errors.Make (Io)
  module Ao = Append_only_file.Make (Io) (Errs)

  type t = {
    capacity : int;
    cache : (string, int) Hashtbl.t;
    index : (int, string) Hashtbl.t;
    ao : Ao.t;
    mutable last_refill_offset : int63;
  }

  let empty_buffer t = Ao.empty_buffer t.ao

  type nonrec int32 = int32 [@@deriving irmin ~to_bin_string ~decode_bin]

  let append_string t v =
    let len = Int32.of_int (String.length v) in
    let buf = int32_to_bin_string len ^ v in
    Ao.append_exn t.ao buf

  let refill t =
    let open Result_syntax in
    let from = t.last_refill_offset in
    let new_size = Ao.end_poff t.ao in
    let len = Int63.to_int Int63.Syntax.(new_size - from) in
    t.last_refill_offset <- new_size;
    let+ raw = Ao.read_to_string t.ao ~off:from ~len in
    let pos_ref = ref 0 in
    let rec aux n =
      if !pos_ref >= len then ()
      else
        let v = decode_bin_int32 raw pos_ref in
        let len = Int32.to_int v in
        let v = String.sub raw !pos_ref len in
        pos_ref := !pos_ref + len;
        Hashtbl.add t.cache v n;
        Hashtbl.add t.index n v;
        (aux [@tailcall]) (n + 1)
    in
    (aux [@tailcall]) (Hashtbl.length t.cache)

  let refresh_end_poff t new_end_poff =
    let open Result_syntax in
    let* () = Ao.refresh_end_poff t.ao new_end_poff in
    refill t

  let index t v =
    [%log.debug "[dict] index %S" v];
    try Some (Hashtbl.find t.cache v)
    with Not_found ->
      let id = Hashtbl.length t.cache in
      if id > t.capacity then None
      else (
        append_string t v;
        Hashtbl.add t.cache v id;
        Hashtbl.add t.index id v;
        Some id)

  let find t id =
    [%log.debug "[dict] find %d" id];
    let v = try Some (Hashtbl.find t.index id) with Not_found -> None in
    v

  let default_capacity = 100_000

  let v_empty ao =
    let cache = Hashtbl.create 997 in
    let index = Hashtbl.create 997 in
    let last_refill_offset = Int63.zero in
    { capacity = default_capacity; index; cache; ao; last_refill_offset }

  let create_rw ~sw ~overwrite ~path:filename =
    let open Result_syntax in
    let* ao = Ao.create_rw ~sw ~overwrite ~path:filename in
    Ok (v_empty ao)

  let v_filled ao =
    let open Result_syntax in
    let t = v_empty ao in
    let* () = refill t in
    Ok t

  let open_rw ~sw ~size ~dead_header_size filename =
    let open Result_syntax in
    let* ao = Ao.open_rw ~sw ~path:filename ~end_poff:size ~dead_header_size in
    v_filled ao

  let open_ro ~sw ~size ~dead_header_size filename =
    let open Result_syntax in
    let* ao = Ao.open_ro ~sw ~path:filename ~end_poff:size ~dead_header_size in
    v_filled ao

  let end_poff t = Ao.end_poff t.ao
  let flush t = Ao.flush t.ao
  let fsync t = Ao.fsync t.ao
  let close t = Ao.close t.ao
end
