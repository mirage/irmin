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
  date : int64;
  uid  : int64;
  owner: string;
  mutable msgs: string list;
}

let t =
  let open Ir_type in
  record "task" (fun date uid owner msgs -> { date; uid; owner; msgs })
  |+ field "date"     int64         (fun t -> t.date)
  |+ field "uid"      int64         (fun t -> t.uid)
  |+ field "owner"    string        (fun t -> t.owner)
  |+ field "messages" (list string) (fun t -> t.msgs)
  |> sealr

type 'a f = 'a -> t

let uid_ref = ref 0L

let create ~date ~owner ?uid msg =
  let uid = match uid with
    | Some u -> u
    | None   ->
      let u = !uid_ref in
      uid_ref := Int64.add !uid_ref 1L;
      u
  in
  { date; uid; owner; msgs = [msg]}

let empty = { date=0L; uid=0L; owner=""; msgs=[]}

let v ~date ~owner ?uid msg =
  if date = 0L && owner = "" && msg = "" then empty
  else create ~date ~owner ?uid msg

let date t = t.date
let uid t = t.uid
let owner t = t.owner
let messages t = List.rev t.msgs

let add t msg =
  if t = empty then ()
  else t.msgs <- msg :: t.msgs

let none = fun () -> empty
