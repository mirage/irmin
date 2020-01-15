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

type t = { date : int64; author : string; message : string }

let t =
  let open Type in
  record "info" (fun date author message -> { date; author; message })
  |+ field "date" int64 (fun t -> t.date)
  |+ field "author" string (fun t -> t.author)
  |+ field "message" string (fun t -> t.message)
  |> sealr

type f = unit -> t

let create ~date ~author message = { date; message; author }

let empty = { date = 0L; author = ""; message = "" }

let v ~date ~author message =
  if date = 0L && author = "" && message = "" then empty
  else create ~date ~author message

let date t = t.date

let author t = t.author

let message t = t.message

let none () = empty
