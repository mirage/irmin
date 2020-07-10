(* Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

(* Extracted from https://github.com/pqwy/lru
   Copyright (c) 2016 David Kaloper Meršinjak *)

module Make (H : Hashtbl.HashedType) = struct
  module HT = Hashtbl.Make (H)

  module Q = struct
    type 'a node = {
      value : 'a;
      mutable next : 'a node option;
      mutable prev : 'a node option;
    }

    type 'a t = {
      mutable first : 'a node option;
      mutable last : 'a node option;
    }

    let detach t n =
      let np = n.prev and nn = n.next in
      (match np with
      | None -> t.first <- nn
      | Some x ->
          x.next <- nn;
          n.prev <- None);
      match nn with
      | None -> t.last <- np
      | Some x ->
          x.prev <- np;
          n.next <- None

    let append t n =
      let on = Some n in
      match t.last with
      | Some x as l ->
          x.next <- on;
          t.last <- on;
          n.prev <- l
      | None ->
          t.first <- on;
          t.last <- on

    let node x = { value = x; prev = None; next = None }

    let create () = { first = None; last = None }
  end

  type key = HT.key

  type 'a t = {
    ht : (key * 'a) Q.node HT.t;
    q : (key * 'a) Q.t;
    mutable cap : int;
    mutable w : int;
  }

  let weight t = t.w

  let create cap = { cap; w = 0; ht = HT.create cap; q = Q.create () }

  let drop_lru t =
    match t.q.first with
    | None -> ()
    | Some ({ Q.value = k, _; _ } as n) ->
        t.w <- t.w - 1;
        HT.remove t.ht k;
        Q.detach t.q n

  let remove t k =
    try
      let n = HT.find t.ht k in
      t.w <- t.w - 1;
      HT.remove t.ht k;
      Q.detach t.q n
    with Not_found -> ()

  let add t k v =
    if t.w = 0 then ()
    else (
      remove t k;
      let n = Q.node (k, v) in
      t.w <- t.w + 1;
      if weight t > t.cap then drop_lru t;
      HT.add t.ht k n;
      Q.append t.q n)

  let promote t k =
    try
      let n = HT.find t.ht k in
      Q.(
        detach t.q n;
        append t.q n)
    with Not_found -> ()

  let find t k =
    let v = HT.find t.ht k in
    promote t k;
    snd v.value

  let mem t k =
    match HT.mem t.ht k with
    | false -> false
    | true ->
        promote t k;
        true

  let clear t = create t.cap
end
