(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

module Make (Fm : File_manager.S) = struct
  type file_manager = Fm.t

  type t = {
    capacity : int;
    cache : (string, int) Hashtbl.t;
    index : (int, string) Hashtbl.t;
    fm : Fm.t;
  }

  module File = struct
    let append_exn t = Fm.Dict.append_exn (Fm.dict t.fm)
    let offset t = Fm.Dict.end_offset (Fm.dict t.fm)
    let read_exn t = Fm.Dict.read_exn (Fm.dict t.fm)
  end

  type nonrec int32 = int32 [@@deriving irmin ~to_bin_string ~decode_bin]

  let append_string t v =
    let len = Int32.of_int (String.length v) in
    let buf = int32_to_bin_string len ^ v in
    File.append_exn t buf

  let refill ~from t =
    let len = Int63.to_int (File.offset t -- from) in
    let raw = Bytes.create len in
    File.read_exn t ~off:from ~len raw;
    let raw = Bytes.unsafe_to_string raw in
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

  (* let sync_offset t =
   *   let former_offset = File.offset t in
   *   let offset = Io_legacy.force_offset t.io in
   *   if offset > former_offset then refill ~from:former_offset t
   *
   * let sync t =
   *   if Io_legacy.readonly t.io then sync_offset t
   *   else invalid_arg "only a readonly instance should call this function"
   *
   * let flush t = Io_legacy.flush t.io *)

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

  let v ~capacity fm =
    let cache = Hashtbl.create 997 in
    let index = Hashtbl.create 997 in
    let t = { capacity; index; cache; fm } in
    (* TODO: refill shouldn't raise *)
    refill ~from:Int63.zero t;
    t

  let close _ = ()
  (* if not (File.readonly t) then flush t;
   * Io_legacy.close t.io;
   * Hashtbl.reset t.cache;
   * Hashtbl.reset t.index *)
end
