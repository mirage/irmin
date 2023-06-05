(*
   Copyright (c) 2016 David Kaloper Meršinjak
   Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

(* Extracted from https://github.com/pqwy/lru *)

module MakeUnsafe (H : Hashtbl.HashedType) = struct
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

    let iter t f =
      let rec aux f = function
        | Some n ->
            let next = n.next in
            f n.value;
            aux f next
        | _ -> ()
      in
      aux f t.first

    let clear t =
      t.first <- None;
      t.last <- None
  end

  type key = HT.key

  type 'a t = {
    ht : (key * 'a) Q.node HT.t;
    q : (key * 'a) Q.t;
    mutable cap : int;
    mutable w : int;
    weight : 'a -> int;
  }

  let create ?(weight = function _ -> 1) cap =
    { cap; w = 0; ht = HT.create cap; q = Q.create (); weight }

  let drop_lru t =
    match t.q.first with
    | None -> ()
    | Some ({ Q.value = k, v; _ } as n) ->
        t.w <- t.w - t.weight v;
        HT.remove t.ht k;
        Q.detach t.q n

  let remove t k =
    try
      let n = HT.find t.ht k in
      t.w <- t.w - t.weight (snd n.value);
      HT.remove t.ht k;
      Q.detach t.q n
    with Not_found -> ()

  let add t k v =
    if t.cap = 0 then ()
    else (
      remove t k;
      let n = Q.node (k, v) in
      let w = t.weight v in
      if w > t.cap then
        (* if [v] is bigger than the LRU capacity, just skip it *) ()
      else (
        t.w <- t.w + w;
        while t.w > t.cap do
          drop_lru t
        done;
        HT.add t.ht k n;
        Q.append t.q n))

  let promote t k =
    try
      let n = HT.find t.ht k in
      Q.(
        detach t.q n;
        append t.q n)
    with Not_found -> ()

  let find_opt t k =
    match HT.find_opt t.ht k with
    | Some v ->
        promote t k;
        Some (snd v.value)
    | None -> None

  let mem t k =
    match HT.mem t.ht k with
    | false -> false
    | true ->
        promote t k;
        true

  let iter t f = Q.iter t.q (fun (k, v) -> f k v)

  let clear t =
    t.w <- 0;
    HT.clear t.ht;
    Q.clear t.q
end

(** Safe but might be incredibly slow. *)
module Make (H : Hashtbl.HashedType) = struct
  module Unsafe = MakeUnsafe (H)

  type 'a t = { lock : Eio.Mutex.t; data : 'a Unsafe.t }

  let create ?weight cap =
    let lock = Eio.Mutex.create () in
    let data = Unsafe.create ?weight cap in
    { lock; data }

  let add { lock; data } k v =
    Eio.Mutex.use_rw ~protect:true lock @@ fun () -> Unsafe.add data k v

  let find_opt { lock; data } k =
    Eio.Mutex.use_rw ~protect:true lock @@ fun () -> Unsafe.find_opt data k

  let find t k = match find_opt t k with Some v -> v | None -> raise Not_found

  let mem { lock; data } k =
    Eio.Mutex.use_rw ~protect:true lock @@ fun () -> Unsafe.mem data k

  let iter { lock; data } f =
    Eio.Mutex.use_rw ~protect:true lock @@ fun () -> Unsafe.iter data f

  let clear { lock; data } =
    Eio.Mutex.use_rw ~protect:true lock @@ fun () -> Unsafe.clear data
end
