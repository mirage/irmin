(*
 * Copyright (c) 2022-2023 Tarides <contact@tarides.com>
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

module Stack = struct
  type t = Empty | Stack of { mutable len : int; arr : int63 array; prev : t }

  let capacity = 131_072 (* = 128*1024, a large but not too large chunk size *)
  let make prev = Stack { len = 0; arr = Array.make capacity Int63.zero; prev }
  let is_full = function Empty -> true | Stack s -> s.len >= capacity

  let rec push_pair ~off ~len t =
    match t with
    | Stack s when not (is_full t) ->
        let i = s.len in
        s.len <- i + 2;
        s.arr.(i) <- off;
        s.arr.(i + 1) <- Int63.of_int len;
        t
    | _ -> push_pair ~off ~len (make t)

  let rec iter_pair fn = function
    | Empty -> ()
    | Stack { len; arr; prev } ->
        assert (len mod 2 = 0);
        for i = (len / 2) - 1 downto 0 do
          let off = arr.(2 * i) in
          let len = arr.((2 * i) + 1) in
          fn ~off ~len
        done;
        iter_pair fn prev
end

type t = {
  mutable last : (int63 * int) option;
  mutable ranges : Stack.t;
  mutable count : int;
}

let make () = { last = None; ranges = Stack.Empty; count = 0 }
let count t = t.count

let add ~off ~len t =
  t.count <- t.count + 1;
  let off_end = Int63.(Syntax.(off + of_int len)) in
  match t.last with
  | None -> t.last <- Some (off, len)
  | Some (off', len') when off_end = off' -> t.last <- Some (off, len + len')
  | Some (off', len') ->
      t.last <- Some (off, len);
      t.ranges <- Stack.push_pair ~off:off' ~len:len' t.ranges

let iter fn t =
  match t.last with
  | None -> assert (t.count = 0)
  | Some (off, len) ->
      fn ~off ~len:(Int63.of_int len);
      Stack.iter_pair fn t.ranges
