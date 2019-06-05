(* Copyright (c) 2015-2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

module type Weighted = sig
  type t

  val weight : t -> int
end

let invalid_arg fmt = Format.ksprintf invalid_arg fmt

type 'a fmt = Format.formatter -> 'a -> unit

let pf = Format.fprintf

let pp_iter ?(sep = Format.pp_print_space) pp ppf i =
  let first = ref true in
  i @@ fun x ->
  (match !first with true -> first := false | _ -> sep ppf ());
  pp ppf x

let cap_makes_sense ~m ~f cap =
  if cap < 0 then invalid_arg "Lru.%s.%s: ~cap:%d" m f cap

module Q = struct
  type 'a node = {
    value : 'a;
    mutable next : 'a node option;
    mutable prev : 'a node option
  }

  type 'a t = { mutable first : 'a node option; mutable last : 'a node option }

  let detach t n =
    let np = n.prev and nn = n.next in
    ( match np with
    | None -> t.first <- nn
    | Some x ->
        x.next <- nn;
        n.prev <- None );
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

  let iter f t =
    let rec go f = function
      | Some n ->
          f n.value;
          go f n.next
      | _ -> ()
    in
    go f t.first

  let fold f t z =
    let rec go f z = function Some n -> go f (f n.value z) n.prev | _ -> z in
    go f z t.last

  let clear t =
    t.first <- None;
    t.last <- None
end

module type S = sig
  type t

  type k

  type v

  val create : ?random:bool -> int -> t

  val is_empty : t -> bool

  val size : t -> int

  val weight : t -> int

  val capacity : t -> int

  val resize : int -> t -> unit

  val trim : ?drop:(k -> v -> unit) -> t -> unit

  val mem : k -> t -> bool

  val find : k -> t -> v option

  val promote : k -> t -> unit

  val add : k -> v -> t -> unit

  val remove : k -> t -> unit

  val lru : t -> (k * v) option

  val drop_lru : t -> unit

  val fold : (k -> v -> 'a -> 'a) -> 'a -> t -> 'a

  val iter : (k -> v -> unit) -> t -> unit

  val of_list : (k * v) list -> t

  val to_list : t -> (k * v) list

  val pp : ?pp_size:(int * int) fmt -> ?sep:unit fmt -> (k * v) fmt -> t fmt

  val pp_dump : k fmt -> v fmt -> t fmt

  val clear : t -> unit
end

module Bake (HT : Hashtbl.SeededS) (V : Weighted) = struct
  type k = HT.key

  type v = V.t

  type t = {
    ht : (k * v) Q.node HT.t;
    q : (k * v) Q.t;
    mutable cap : int;
    mutable w : int
  }

  let clear t =
    HT.clear t.ht;
    Q.clear t.q;
    t.w <- 0

  let size t = HT.length t.ht

  let weight t = t.w

  let capacity t = t.cap

  let is_empty t = HT.length t.ht = 0

  let cap_makes_sense = cap_makes_sense ~m:"M"

  let create ?random cap =
    cap_makes_sense ~f:"create" cap;
    { cap; w = 0; ht = HT.create ?random cap; q = Q.create () }

  let lru t = match t.q.Q.first with Some n -> Some n.Q.value | _ -> None

  let drop_lru_opt t =
    match t.q.Q.first with
    | None -> None
    | Some ({ Q.value = k, v; _ } as n) ->
        t.w <- t.w - V.weight v;
        HT.remove t.ht k;
        Q.detach t.q n;
        Some (k, v)

  let drop_lru t = ignore (drop_lru_opt t)

  let rec trim ?(drop = fun _ _ -> ()) t =
    if weight t > t.cap then (
      (match drop_lru_opt t with None -> () | Some (k, v) -> drop k v);
      trim ~drop t )

  let resize cap t =
    cap_makes_sense ~f:"resize" cap;
    t.cap <- cap

  let remove k t =
    try
      let n = HT.find t.ht k in
      t.w <- t.w - (snd n.Q.value |> V.weight);
      HT.remove t.ht k;
      Q.detach t.q n
    with Not_found -> ()

  let add k v t =
    remove k t;
    let n = Q.node (k, v) in
    t.w <- t.w + V.weight v;
    HT.add t.ht k n;
    Q.append t.q n

  let promote k t =
    try
      let n = HT.find t.ht k in
      Q.(
        detach t.q n;
        append t.q n)
    with Not_found -> ()

  let find k t =
    try Some (snd (HT.find t.ht k).Q.value) with Not_found -> None

  let mem k t = HT.mem t.ht k

  let iter f t = Q.iter (fun (k, v) -> f k v) t.q

  let fold f z t = Q.fold (fun (k, v) a -> f k v a) t.q z

  let to_list t = Q.fold (fun x xs -> x :: xs) t.q []

  let of_list xs =
    let t = create 0 in
    List.iter (fun (k, v) -> add k v t) xs;
    resize (Q.fold (fun (_, v) w -> w + V.weight v) t.q 0) t;
    t

  let pp ?(pp_size = fun _ -> ignore) ?sep pp ppf t =
    pf ppf "@[%a@[%a@]@]" pp_size (t.w, t.cap) (pp_iter ?sep pp) (fun f ->
        Q.iter f t.q )

  let pp_dump ppk ppv ppf =
    let sep ppf () = pf ppf ";@ "
    and ppkv ppf (k, v) = pf ppf "(@[%a,@ %a@])" ppk k ppv v in
    pf ppf "of_list [%a]" (pp ~sep ppkv)
end

module SeededHash (H : Hashtbl.HashedType) = struct
  include H

  let hash _ x = hash x
end

module Make (K : Hashtbl.HashedType) (V : Weighted) =
  Bake (Hashtbl.MakeSeeded (SeededHash (K))) (V)
module MakeSeeded (K : Hashtbl.SeededHashedType) (V : Weighted) =
  Bake (Hashtbl.MakeSeeded (K)) (V)
