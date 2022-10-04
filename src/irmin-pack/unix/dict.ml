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

module Make (Fm : File_manager.S) = struct
  module Fm = Fm

  type t = {
    capacity : int;
    cache : (string, int) Hashtbl.t;
    index : (int, string) Hashtbl.t;
    fm : Fm.t;
    mutable last_refill_offset : int63;
  }

  module File = struct
    let append_exn t = Fm.Dict.append_exn (Fm.dict t.fm)
    let offset t = Fm.Dict.end_poff (Fm.dict t.fm)
    let read_to_string t = Fm.Dict.read_to_string (Fm.dict t.fm)
  end

  type nonrec int32 = int32 [@@deriving irmin ~to_bin_string ~decode_bin]

  let append_string t v =
    let len = Int32.of_int (String.length v) in
    let buf = int32_to_bin_string len ^ v in
    File.append_exn t buf

  (* Refill is only called once for a RW instance *)
  let refill t =
    let open Result_syntax in
    let from = t.last_refill_offset in
    let len = Int63.to_int Int63.Syntax.(File.offset t - from) in
    t.last_refill_offset <- File.offset t;
    let+ raw = File.read_to_string t ~off:from ~len in
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

  let v fm =
    let open Result_syntax in
    let cache = Hashtbl.create 997 in
    let index = Hashtbl.create 997 in
    let last_refill_offset = Int63.zero in
    let t =
      { capacity = default_capacity; index; cache; fm; last_refill_offset }
    in
    let* () = refill t in
    Fm.register_dict_consumer fm ~after_reload:(fun () -> refill t);
    Ok t

  let close _ = ()
end
