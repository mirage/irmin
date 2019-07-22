(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let src =
  Logs.Src.create "irmin.pack.dict" ~doc:"irmin-pack backend dictionaries"

module Log = (val Logs.src_log src : Logs.LOG)

module IO = Irmin_pack_io.Unix

let current_version = "00000001"

let ( -- ) = Int64.sub

let with_cache = Irmin_pack_io.with_cache

exception RO_Not_Allowed = Irmin_pack_io.RO_Not_Allowed

type t = {
  cache : (string, int) Hashtbl.t;
  index : (int, string) Hashtbl.t;
  io : IO.t;
}

let io t = t.io

let append_string t v =
  let len = Int32.of_int (String.length v) in
  let buf = Irmin.Type.(to_bin_string int32 len) ^ v in
  IO.append t.io buf

let refill ~from t =
  let len = Int64.to_int (IO.offset t.io -- from) in
  let raw = Bytes.create len in
  let n = IO.read t.io ~off:from raw in
  assert (n = len);
  let raw = Bytes.unsafe_to_string raw in
  let rec aux n offset =
    if offset >= len then ()
    else
      let _, v = Irmin.Type.(decode_bin int32) raw offset in
      let len = Int32.to_int v in
      let v = String.sub raw (offset + 4) len in
      Hashtbl.add t.cache v n;
      Hashtbl.add t.index n v;
      (aux [@tailcall]) (n + 1) (offset + 4 + len)
  in
  (aux [@tailcall]) (Hashtbl.length t.cache) 0

let sync_offset t =
  let former_log_offset = IO.offset t.io in
  let log_offset = IO.force_offset t.io in
  if log_offset > former_log_offset then refill ~from:former_log_offset t

let sync t = IO.sync t.io

let index t v =
  Log.debug (fun l -> l "[dict] index %S" v);
  if IO.readonly t.io then sync_offset t;
  try Hashtbl.find t.cache v
  with Not_found ->
    if IO.readonly t.io then raise RO_Not_Allowed;
    let id = Hashtbl.length t.cache in
    append_string t v;
    Hashtbl.add t.cache v id;
    Hashtbl.add t.index id v;
    id

let find t id =
  if IO.readonly t.io then sync_offset t;
  Log.debug (fun l -> l "[dict] find %d" id);
  let v = try Some (Hashtbl.find t.index id) with Not_found -> None in
  v

let clear t =
  IO.clear t.io;
  Hashtbl.clear t.cache;
  Hashtbl.clear t.index

let v_no_cache ~fresh ~shared:_ ~readonly file =
  let io = IO.v ~fresh ~version:current_version ~readonly file in
  let cache = Hashtbl.create 997 in
  let index = Hashtbl.create 997 in
  let t = { index; cache; io } in
  refill ~from:0L t;
  t

let v = with_cache ~clear ~v:v_no_cache "store.dict"
