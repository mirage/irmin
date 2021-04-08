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

include Dict_intf
open! Import

module Make (V : Version.S) (IO : IO.S) : S = struct
  type t = {
    capacity : int;
    cache : (string, int) Hashtbl.t;
    index : (int, string) Hashtbl.t;
    mutable io : IO.t;
    mutable open_instances : int;
  }

  let int32_to_bin = Irmin.Type.(unstage (to_bin_string int32))
  let decode_int32 = Irmin.Type.(unstage (decode_bin int32))

  let append_string t v =
    let len = Int32.of_int (String.length v) in
    let buf = int32_to_bin len ^ v in
    IO.append t.io buf

  let refill ~from t =
    let len = Int63.to_int (IO.offset t.io -- from) in
    let raw = Bytes.create len in
    let n = IO.read t.io ~off:from raw in
    assert (n = len);
    let raw = Bytes.unsafe_to_string raw in
    let rec aux n offset =
      if offset >= len then ()
      else
        let _, v = decode_int32 raw offset in
        let len = Int32.to_int v in
        let v = String.sub raw (offset + 4) len in
        Hashtbl.add t.cache v n;
        Hashtbl.add t.index n v;
        (aux [@tailcall]) (n + 1) (offset + 4 + len)
    in
    (aux [@tailcall]) (Hashtbl.length t.cache) 0

  let sync_offset t =
    let former_offset = IO.offset t.io in
    let former_generation = IO.generation t.io in
    let h = IO.force_headers t.io in
    if former_generation <> h.generation then (
      IO.close t.io;
      let io =
        IO.v ~fresh:false ~readonly:true ~version:(Some V.version)
          (IO.name t.io)
      in
      t.io <- io;
      Hashtbl.clear t.cache;
      Hashtbl.clear t.index;
      refill ~from:Int63.zero t)
    else if h.offset > former_offset then refill ~from:former_offset t

  let sync t =
    if IO.readonly t.io then sync_offset t
    else invalid_arg "only a readonly instance should call this function"

  let flush t = IO.flush t.io

  let index t v =
    Log.debug (fun l -> l "[dict] index %S" v);
    try Some (Hashtbl.find t.cache v)
    with Not_found ->
      let id = Hashtbl.length t.cache in
      if id > t.capacity then None
      else (
        if IO.readonly t.io then raise S.RO_not_allowed;
        append_string t v;
        Hashtbl.add t.cache v id;
        Hashtbl.add t.index id v;
        Some id)

  let find t id =
    Log.debug (fun l -> l "[dict] find %d" id);
    let v = try Some (Hashtbl.find t.index id) with Not_found -> None in
    v

  let clear t =
    match V.version with
    | `V1 -> IO.truncate t.io
    | `V2 ->
        IO.clear t.io;
        Hashtbl.clear t.cache;
        Hashtbl.clear t.index

  let v ?(fresh = true) ?(readonly = false) ?(capacity = 100_000) file =
    let io = IO.v ~fresh ~version:(Some V.version) ~readonly file in
    let cache = Hashtbl.create 997 in
    let index = Hashtbl.create 997 in
    let t = { capacity; index; cache; io; open_instances = 1 } in
    refill ~from:Int63.zero t;
    t

  let close t =
    t.open_instances <- t.open_instances - 1;
    if t.open_instances = 0 then (
      if not (IO.readonly t.io) then flush t;
      IO.close t.io;
      Hashtbl.reset t.cache;
      Hashtbl.reset t.index)

  let valid t =
    if t.open_instances <> 0 then (
      t.open_instances <- t.open_instances + 1;
      true)
    else false
end
