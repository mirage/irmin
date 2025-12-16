(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

include Info_intf

module Default = struct
  type author = string [@@deriving irmin]
  type message = string [@@deriving irmin]

  type t = { date : int64; author : author; message : message }
  [@@deriving irmin ~equal]

  type f = unit -> t

  let empty = { date = 0L; author = ""; message = "" }
  let is_empty = equal empty

  let v ?(author = "") ?(message = "") date =
    let r = { date; message; author } in
    if is_empty r then empty else r

  let date t = t.date
  let author t = t.author
  let message t = t.message
  let none () = empty
end

type default = Default.t
