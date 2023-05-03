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

type range = { off : int63; len : int63 }

module Stack = struct
  type t = Empty | Stack of { mutable len : int; arr : int63 array; prev : t }

  let capacity = 131_072 (* = 128*1024, a large but not too large chunk size *)
  let make prev = Stack { len = 0; arr = Array.make capacity Int63.zero; prev }
  let is_full = function Empty -> true | Stack s -> s.len >= capacity

  let rec push x t =
    match t with
    | Stack s when not (is_full t) ->
        let i = s.len in
        s.len <- i + 2;
        s.arr.(i) <- x.off;
        s.arr.(i + 1) <- x.len;
        t
    | _ -> push x (make t)

  let rec to_seq t () =
    match t with
    | Empty -> Seq.Nil
    | Stack { len; arr; prev } ->
        assert (len mod 2 = 0);
        let rec go i () =
          if i < 0 then to_seq prev ()
          else
            let range = { off = arr.(2 * i); len = arr.((2 * i) + 1) } in
            Seq.Cons (range, go (i - 1))
        in
        go ((len / 2) - 1) ()
end

type t = {
  mutable last : range option;
  mutable ranges : Stack.t;
  mutable count : int;
  mutable out_of_order : range list;
}

let make () =
  { last = None; ranges = Stack.Empty; count = 0; out_of_order = [] }

let count t = t.count

let add ~off ~len t =
  t.count <- t.count + 1;
  let open Int63.Syntax in
  let len = Int63.of_int len in
  match t.last with
  | None -> t.last <- Some { off; len }
  | Some last when off + len = last.off ->
      (* latest interval can be fused with the previous one *)
      t.last <- Some { off; len = len + last.len }
  | Some last when off + len < last.off ->
      (* disjoint and strictly smaller *)
      t.last <- Some { off; len };
      t.ranges <- Stack.push last t.ranges
  | Some _ ->
      (* latest range is not strictly smaller than previous,
       * this is only expected on legacy data with wrong object ordering
       * and is handled as a special case. *)
      t.out_of_order <- { off; len } :: t.out_of_order

let ranges_to_seq t () =
  match t.last with
  | None -> Seq.Nil
  | Some range -> Seq.Cons (range, Stack.to_seq t.ranges)

let out_of_order_to_seq t =
  List.to_seq
  @@ List.sort_uniq (fun a b -> Int63.compare a.off b.off) t.out_of_order

let rec seq_merge xs ys () =
  match (xs (), ys ()) with
  | Seq.Nil, rest | rest, Seq.Nil -> rest
  | Seq.Cons (x, xs'), Seq.Cons (y, ys') -> (
      match Int63.compare x.off y.off with
      | 0 ->
          assert (x.len = y.len);
          Seq.Cons (x, seq_merge xs' ys')
      | c when c < 0 -> Seq.Cons (x, seq_merge xs' ys)
      | _ -> Seq.Cons (y, seq_merge xs ys'))

type fused = Disjoint of range * range | Overlap of range

let fuse fst snd =
  let open Int63.Syntax in
  let fst_end = fst.off + fst.len in
  let snd_end = snd.off + snd.len in
  if fst_end < snd.off then Disjoint (fst, snd)
  else if snd_end < fst.off then Disjoint (snd, fst)
  else
    let start = min fst.off snd.off in
    let stop = max fst_end snd_end in
    Overlap { off = start; len = stop - start }

let rec seq_fuse ?prev s () =
  match (prev, s ()) with
  | None, Seq.Nil -> Seq.Nil
  | Some prev, Nil -> Seq.Cons (prev, Seq.empty)
  | None, Cons (x, xs) -> seq_fuse ~prev:x xs ()
  | Some prev, Cons (x, xs) -> (
      match fuse x prev with
      | Disjoint (fst, snd) -> Seq.Cons (fst, seq_fuse ~prev:snd xs)
      | Overlap prev -> seq_fuse ~prev xs ())

let iter fn t =
  let in_order = ranges_to_seq t in
  let ranges =
    match t.out_of_order with
    | [] -> in_order
    | _ -> seq_fuse (seq_merge in_order (out_of_order_to_seq t))
  in
  Seq.iter (fun { off; len } -> fn ~off ~len) ranges
