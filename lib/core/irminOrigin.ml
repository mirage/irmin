(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Core_kernel.Std

type t = {
  date: int64;
  id  : string;
  msg : string;
} with bin_io, compare, sexp

let to_json t =
  `O [ "date", Ezjsonm.string (Int64.to_string t.date) ;
       "id"  , IrminMisc.json_encode t.id;
       "msg" , IrminMisc.json_encode t.msg; ]

let of_json j =
  let date = Ezjsonm.find j ["date"] |> Ezjsonm.get_string |> Int64.of_string in
  let id   = Ezjsonm.find j ["id"]   |> Ezjsonm.get_string in
  let msg  = Ezjsonm.find j ["msg"]  |> IrminMisc.json_decode_exn in
  { date; id; msg }


let date_hook =
  let c = ref 0L in
  ref (fun () -> c := Int64.(!c + 1L); !c)

let set_date f =
  date_hook := f

let id_hook =
  let r = string_of_int (Random.int 1024) in
  ref (fun () -> r)

let set_id f =
  id_hook := f

let create ?date ? id fmt =
  let date = match date with
    | None   -> !date_hook ()
    | Some d -> d in
  let id = match id with
    | None   -> !id_hook ()
    | Some i -> i in
  ksprintf (fun msg ->
      { date; id; msg }
    ) fmt

let date t = t.date
let id t = t.id
let message t = t.msg
