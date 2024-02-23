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

open Kcas

module Make (H : Hashtbl.HashedType) = struct
  module HT = Kcas_data.Hashtbl

  module Q = struct
    type 'a node = {
      value : 'a;
      next : 'a node option Loc.t;
      prev : 'a node option Loc.t;
    }

    type 'a t = { tail : 'a node option Loc.t; head : 'a node option Loc.t }

    let detach ~xt t n =
      let np = Xt.get ~xt n.prev and nn = Xt.get ~xt n.next in
      (match np with
      | None -> Xt.set ~xt t.tail nn
      | Some x ->
          Xt.set ~xt x.next nn;
          Xt.set ~xt n.prev None);
      match nn with
      | None -> Xt.set ~xt t.head np
      | Some x ->
          Xt.set ~xt x.prev np;
          Xt.set ~xt n.next None

    let append ~xt t n =
      let on = Some n in
      let hd = Xt.get ~xt t.head in
      match hd with
      | Some x as l ->
          Xt.set ~xt x.next on;
          Xt.set ~xt t.head on;
          Xt.set ~xt n.prev l
      | None ->
          Xt.set ~xt t.tail on;
          Xt.set ~xt t.head on

    let node x = { value = x; prev = Loc.make None; next = Loc.make None }
    let create () = { tail = Loc.make None; head = Loc.make None }

    let clear ~xt t =
      Xt.set ~xt t.tail None;
      Xt.set ~xt t.head None
  end

  type key = H.t

  type 'a t = {
    ht : (key, (key * 'a) Q.node) HT.t;
    q : (key * 'a) Q.t;
    cap : cap;
    w : int Loc.t;
  }

  and cap = Uncapped | Capped of int

  let weight ~xt t = Xt.get ~xt t.w

  let create cap =
    let cap, ht_cap =
      if cap < 0 then (Uncapped, 65536) else (Capped cap, cap)
    in
    {
      cap;
      w = Loc.make 0;
      ht = HT.create ~hashed_type:(module H) ~min_buckets:ht_cap ();
      q = Q.create ();
    }

  let drop ~xt t =
    let tl = Xt.get ~xt t.q.tail in
    match tl with
    | None -> None
    | Some ({ Q.value = k, v; _ } as n) ->
        Xt.modify ~xt t.w (fun tw -> tw - 1);
        HT.Xt.remove ~xt t.ht k;
        Q.detach ~xt t.q n;
        Some v

  let remove ~xt t k =
    match HT.Xt.find_opt ~xt t.ht k with
    | None -> ()
    | Some n ->
        Xt.modify ~xt t.w (fun tw -> tw - 1);
        HT.Xt.remove ~xt t.ht k;
        Q.detach ~xt t.q n

  let add t k v =
    let tx ~xt =
      let add t k v =
        remove ~xt t k;
        let n = Q.node (k, v) in
        Xt.modify ~xt t.w (fun tw -> tw + 1);
        HT.Xt.replace ~xt t.ht k n;
        Q.append ~xt t.q n
      in
      match t.cap with
      | Capped c when c = 0 -> ()
      | Uncapped -> add t k v
      | Capped c ->
          add t k v;
          if weight ~xt t > c then
            let _ = drop ~xt t in
            ()
    in
    Xt.commit { tx }

  let drop t = Xt.commit { tx = drop t }

  let promote ~xt t n =
    Q.detach ~xt t.q n;
    Q.append ~xt t.q n

  let find t k =
    let tx ~xt =
      match HT.Xt.find_opt ~xt t.ht k with
      | Some v ->
          promote ~xt t v;
          snd v.value
      | None ->
          raise Not_found
    in
    Xt.commit { tx }

  let mem t k =
    let tx ~xt =
      match HT.Xt.find_opt ~xt t.ht k with
      | None -> false
      | Some v ->
          promote ~xt t v;
          true
    in
    Xt.commit { tx }

  let clear t =
    let tx ~xt =
      Xt.set ~xt t.w 0;
      HT.Xt.clear ~xt t.ht;
      Q.clear ~xt t.q
    in
    Xt.commit { tx }

  let iter t f =
    HT.iter (fun k q -> f k (snd q.Q.value)) t.ht
end
