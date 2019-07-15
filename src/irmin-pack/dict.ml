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
  Logs.Src.create "irmin.pack.dict" ~doc:"irmin-pack backend dictionnaries"

module Log = (val Logs.src_log src : Logs.LOG)

let current_version = "00000001"

let ( // ) = Filename.concat

module IO = IO.Unix

type t = {
  cache : (string, int) Hashtbl.t;
  index : (int, string) Hashtbl.t;
  io : IO.t
}

let io t = t.io

let append_string t v =
  let len = Int32.of_int (String.length v) in
  let buf = Irmin.Type.(to_bin_string int32 len) ^ v in
  IO.append t.io buf

let index t v =
  Log.debug (fun l -> l "[dict] index %S" v);
  try Hashtbl.find t.cache v
  with Not_found ->
    let id = Hashtbl.length t.cache in
    append_string t v;
    Hashtbl.add t.cache v id;
    Hashtbl.add t.index id v;
    id

let find t id =
  Log.debug (fun l -> l "[dict] find %d" id);
  let v = try Some (Hashtbl.find t.index id) with Not_found -> None in
  v

let clear t =
  IO.clear t.io;
  Hashtbl.clear t.cache;
  Hashtbl.clear t.index

let files = Hashtbl.create 10

let v ?(fresh = false) ?(readonly = false) root =
  let root = root // "store.dict" in
  Log.debug (fun l -> l "[dict] v fresh=%b RO=%b root=%s" fresh readonly root);
  try
    let t = Hashtbl.find files root in
    if fresh then clear t;
    t
  with Not_found ->
    let io = IO.v ~version:current_version ~readonly root in
    if fresh then IO.clear io;
    let cache = Hashtbl.create 997 in
    let index = Hashtbl.create 997 in
    let len = Int64.to_int (IO.offset io) in
    let raw = Bytes.create len in
    let n = IO.read io ~off:0L raw in
    assert (n = len);
    let raw = Bytes.unsafe_to_string raw in
    let rec aux n offset k =
      if offset >= len then k ()
      else
        let _, v = Irmin.Type.(decode_bin int32) raw offset in
        let len = Int32.to_int v in
        let v = String.sub raw (offset + 4) len in
        Hashtbl.add cache v n;
        Hashtbl.add index n v;
        (aux [@tailcall]) (n + 1) (offset + 4 + len) k
    in
    (aux [@tailcall]) 0 0 @@ fun () ->
    let t = { index; cache; io } in
    Hashtbl.add files root t;
    t
