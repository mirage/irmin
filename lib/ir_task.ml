(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type 'a f = 'a -> t

let to_json t =
  `O [ ("date"    , Ezjsonm.string (Int64.to_string t.date));
       ("uid"     , Ezjsonm.string (Int64.to_string t.uid));
       ("owner"   , Ezjsonm.encode_string t.owner);
       ("messages", Ezjsonm.list Ezjsonm.encode_string t.msgs); ]

let of_json j =
  let int64 j = j |> Ezjsonm.get_string |> Int64.of_string in
  let date = Ezjsonm.find j ["date"] |> int64 in
  let uid = Ezjsonm.find j ["uid"] |> int64 in
  let owner = Ezjsonm.find j ["owner"] |> Ezjsonm.decode_string_exn in
  let msgs =
    Ezjsonm.find j ["messages"] |> Ezjsonm.get_list Ezjsonm.decode_string_exn
  in
  { date; uid; owner; msgs }

module X = Tc.Pair
    (Tc.Pair(Tc.Int64)(Tc.Int64))
    (Tc.Pair(Tc.String)(Tc.List(Tc.String)))

let explode t = (t.date, t.uid), (t.owner, t.msgs)
let implode ( (date, uid), (owner, msgs) ) = { date; uid; owner; msgs }

let hash t = X.hash (explode t)
let compare x y = X.compare (explode x) (explode y)
let size_of x = X.size_of (explode x)
let write x b = X.write (explode x) b
let read b = implode (X.read b)
let equal x y = X.equal (explode x) (explode y)

let uid_ref = ref 0L

let create_aux ~date ~owner ?uid msg =
  let uid = match uid with
    | Some u -> u
    | None   ->
      let u = !uid_ref in
      uid_ref := Int64.add !uid_ref 1L;
      u
  in
  { date; uid; owner; msgs = [msg]}

let empty = { date=0L; uid=0L; owner=""; msgs=[]}

let create ~date ~owner ?uid msg =
  if date = 0L && owner = "" && msg = "" then empty
  else create_aux ~date ~owner ?uid msg

let date t = t.date
let uid t = t.uid
let owner t = t.owner
let messages t = List.rev t.msgs

let add t msg =
  if t = empty then ()
  else t.msgs <- msg :: t.msgs

let none = fun () -> empty
