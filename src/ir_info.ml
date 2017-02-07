(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type t = {
  date   : int64;
  owner  : string;
  message: string;
}

let t =
  let open Ir_type in
  record "info" (fun date owner message -> { date; owner; message })
  |+ field "date"    int64  (fun t -> t.date)
  |+ field "owner"   string (fun t -> t.owner)
  |+ field "message" string (fun t -> t.message)
  |> sealr

type 'a f = 'a -> t

let create ~date ~owner message = { date; message; owner }

let empty = { date=0L; owner=""; message = "" }

let v ~date ~owner message =
  if date = 0L && owner = "" && message = "" then empty
  else create ~date ~owner message

let date t = t.date
let owner t = t.owner
let message t = t.message
let none = fun () -> empty
