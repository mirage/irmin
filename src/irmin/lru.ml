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
  type 'a value = { v : 'a; k : key; w : int }

  type 'a t = {
    ht : (key, 'a value Q.node) HT.t;
    q : 'a value Q.t;
    weight_limit : int option;
    weight : int Loc.t;
  }

  let weight ~xt t = Xt.get ~xt t.weight

  let create wl =
    let weight_limit, default_weight_limit =
      match wl with None -> (None, 65536) | Some wl -> (Some wl, wl)
    in
    {
      weight_limit;
      weight = Loc.make 0;
      ht =
        HT.create ~hashed_type:(module H) ~min_buckets:default_weight_limit ();
      q = Q.create ();
    }

  let drop ~xt t =
    let tl = Xt.get ~xt t.q.tail in
    match tl with
    | None -> None
    | Some ({ Q.value = v; _ } as n) ->
        Xt.fetch_and_add ~xt t.weight (-v.w) |> ignore;
        HT.Xt.remove ~xt t.ht v.k;
        Q.detach ~xt t.q n;
        Some v.v

  let lru_enabled t = match t.weight_limit with None -> true | Some x -> x > 0

  let add ~xt t key w v =
    if not (lru_enabled t) then ()
    else
      let add t k v =
        let n = Q.node { v; k; w } in
        (match HT.Xt.find_opt ~xt t.ht k with
        | Some old ->
            Q.detach ~xt t.q old;
            Xt.fetch_and_add ~xt t.weight (w - old.value.w) |> ignore
        | None -> Xt.fetch_and_add ~xt t.weight w |> ignore);
        Q.append ~xt t.q n;
        HT.Xt.replace ~xt t.ht key n
      in
      match t.weight_limit with
      | Some wl when wl = 0 -> ()
      | None -> add t key v
      | Some weight_limit ->
          add t key v;
          let rec drop_until_weight_limit () =
            if weight ~xt t > weight_limit then
              match drop ~xt t with
              | None -> ()
              | Some _ -> drop_until_weight_limit ()
          in
          drop_until_weight_limit ()

  let add t k ?(weight = 1) v = Xt.commit { tx = add t k weight v }
  let drop t = Xt.commit { tx = drop t }

  let promote ~xt t n =
    Q.detach ~xt t.q n;
    Q.append ~xt t.q n

  let find t k =
    let tx ~xt =
      match HT.Xt.find_opt ~xt t.ht k with
      | Some v ->
          promote ~xt t v;
          v.value.v
      | None -> raise Not_found
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
      Xt.set ~xt t.weight 0;
      HT.Xt.clear ~xt t.ht;
      Q.clear ~xt t.q
    in
    Xt.commit { tx }

  let iter t f = HT.iter (fun k q -> f k q.Q.value.v) t.ht
end
