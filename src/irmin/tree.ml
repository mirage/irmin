(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2017 Grégoire Henry <gregoire.henry@ocamlpro.com>
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

open Lwt.Infix

let src = Logs.Src.create "irmin.tree" ~doc:"Persistent lazy trees for Irmin"

module Log = (val Logs.src_log src : Logs.LOG)

(* assume l1 and l2 are key-sorted *)
let alist_iter2 compare_k f l1 l2 =
  let rec aux l1 l2 =
    match (l1, l2) with
    | [], t -> List.iter (fun (key, v) -> f key (`Right v)) t
    | t, [] -> List.iter (fun (key, v) -> f key (`Left v)) t
    | (k1, v1) :: t1, (k2, v2) :: t2 -> (
      match compare_k k1 k2 with
      | 0 ->
          f k1 (`Both (v1, v2));
          (aux [@tailcall]) t1 t2
      | x ->
          if x < 0 then (
            f k1 (`Left v1);
            (aux [@tailcall]) t1 l2 )
          else (
            f k2 (`Right v2);
            (aux [@tailcall]) l1 t2 ) )
  in
  aux l1 l2

(* assume l1 and l2 are key-sorted *)
let alist_iter2_lwt compare_k f l1 l2 =
  let l3 = ref [] in
  alist_iter2 compare_k (fun left right -> l3 := f left right :: !l3) l1 l2;
  Lwt_list.iter_s (fun b -> b >>= fun () -> Lwt.return_unit) (List.rev !l3)

module Lru = struct
  (* Extracted from https://github.com/pqwy/lru
     Copyright (c) 2016 David Kaloper Meršinjak *)

  module Q = struct
    type 'a node = {
      value : 'a;
      mutable next : 'a node option;
      mutable prev : 'a node option
    }

    type 'a t = {
      mutable first : 'a node option;
      mutable last : 'a node option
    }

    let clear t =
      t.first <- None;
      t.last <- None

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
  end

  module Bake (HT : Hashtbl.SeededS) = struct
    type key = HT.key

    type 'a t = {
      ht : (key * 'a) Q.node HT.t;
      q : (key * 'a) Q.t;
      mutable cap : int;
      mutable w : int
    }

    let size t = HT.length t.ht

    let weight t = t.w

    let create ?random cap =
      { cap; w = 0; ht = HT.create ?random cap; q = Q.create () }

    let drop_lru t =
      match t.q.Q.first with
      | None -> ()
      | Some ({ Q.value = k, _; _ } as n) ->
          t.w <- t.w - 1;
          HT.remove t.ht k;
          Q.detach t.q n

    let rec trim t =
      if weight t > t.cap then (
        drop_lru t;
        trim t )

    let remove k t =
      try
        let n = HT.find t.ht k in
        t.w <- t.w - 1;
        HT.remove t.ht k;
        Q.detach t.q n
      with Not_found -> ()

    let clear t =
      HT.clear t.ht;
      Q.clear t.q

    let add k v t =
      remove k t;
      let n = Q.node (k, v) in
      t.w <- t.w + 1;
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

    let iter f t = Q.iter (fun (k, v) -> f k v) t.q
  end

  module SeededHash (H : Hashtbl.HashedType) = struct
    include H

    let hash _ x = hash x
  end

  module Make (K : Hashtbl.HashedType) =
    Bake (Hashtbl.MakeSeeded (SeededHash (K)))
end

module Cache (K : S.HASH) : sig
  type 'a t

  type key = K.t

  val create : int -> 'a t

  val find : 'a t -> key -> 'a

  val add : 'a t -> key -> 'a -> unit

  val remove : 'a t -> key -> unit

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val clear : 'a t -> unit

  val length : 'a t -> int
end = struct
  module M = Lru.Make (struct
    type t = K.t

    let equal (x : t) (y : t) = Type.equal K.t x y

    let hash (x : t) = Type.short_hash K.t x
  end)

  include M

  let create x = M.create x

  let length = M.size

  let add t k v =
    M.add k v t;
    M.trim t

  let find t k =
    match M.find k t with
    | None -> raise Not_found
    | Some v ->
        M.promote k t;
        v

  let remove t k = M.remove k t
end

module Make (P : S.PRIVATE) = struct
  module Path = P.Node.Path

  module StepMap = struct
    module X = struct
      type t = Path.step

      let t = Path.step_t

      let compare = Type.compare Path.step_t
    end

    include Map.Make (X)
    include Merge.Map (X)
  end

  module Metadata = P.Node.Metadata

  type key = Path.t

  type hash = P.Hash.t

  type step = Path.step

  type contents = P.Contents.value

  type repo = P.Repo.t

  let pp_hash = Type.pp P.Hash.t

  let pp_path = Type.pp Path.t

  module Hashes = Hashtbl.Make (struct
    type t = hash

    let hash = P.Hash.short_hash

    let equal = Type.equal P.Hash.t
  end)

  module Contents = struct
    type v = Hash of repo * hash | Value of contents

    type info = { mutable hash : hash option; mutable value : contents option }

    type t = { mutable v : v; mutable info : info }

    let v =
      let open Type in
      variant "Node.Contents.v" (fun hash value -> function
        | Hash (_, x) -> hash x | Value v -> value v )
      |~ case1 "hash" P.Hash.t (fun _ -> assert false)
      |~ case1 "value" P.Contents.Val.t (fun v -> Value v)
      |> sealv

    let clear_info i =
      i.value <- None;
      i.hash <- None

    let merge_info ~into:x y =
      let () =
        match (x.hash, y.hash) with
        | None, None | Some _, None -> ()
        | Some _, Some _ -> ()
        | None, _ -> x.hash <- y.hash
      in
      let () =
        match (x.value, y.value) with
        | None, None | Some _, None -> ()
        | Some _, Some _ -> ()
        | None, _ -> x.value <- y.value
      in
      ()

    let info_is_empty i = i.value = None

    module Cache = Cache (P.Hash)

    let cache = Cache.create 10_000

    let of_v v =
      let hash, value =
        match v with Hash (_, k) -> (Some k, None) | Value v -> (None, Some v)
      in
      (* hashcons the info *)
      let info =
        match hash with
        | None -> { hash; value }
        | Some k -> (
          match Cache.find cache k with
          | exception Not_found ->
              let i = { hash; value } in
              Log.debug (fun l -> l "Contents.of_v: cache %a" pp_hash k);
              Cache.add cache k i;
              i
          | i -> i )
      in
      (* hashcons for the contents (= leaf nodes) *)
      let v =
        match (v, info.value, info.hash) with
        | Value _, Some v, _ -> Value v
        | Hash (r, _), _, Some h -> Hash (r, h)
        | _ -> v
      in
      let t = { v; info } in
      t

    let export ?clear repo t k =
      Log.debug (fun l -> l "Tree.Contents.export clear=%b" (clear = Some true));
      if clear = Some true then t.info.value <- None;
      match (t.v, t.info.hash) with
      | Hash (_, k), _ -> t.v <- Hash (repo, k)
      | Value _, None -> t.v <- Hash (repo, k)
      | Value _, Some k -> t.v <- Hash (repo, k)

    let t = Type.map v of_v (fun t -> t.v)

    let of_value c = of_v (Value c)

    let of_hash repo k = of_v (Hash (repo, k))

    let hash t =
      match (t.v, t.info.hash) with
      | Hash (_, k), None ->
          let h = Some k in
          t.info.hash <- h;
          h
      | _, h -> h

    let value t =
      match (t.v, t.info.value) with
      | Value v, None ->
          let v = Some v in
          t.info.value <- v;
          v
      | _, v -> v

    let hashcons t =
      match (t.v, t.info.hash, t.info.value) with
      | Hash (r, h), Some h', _ -> if h != h' then t.v <- Hash (r, h')
      | Value v, _, Some v' -> if v != v' then t.v <- Value v'
      | _ -> ()

    let to_hash c =
      match hash c with
      | Some k -> k
      | None -> (
        match value c with
        | None -> assert false
        | Some v ->
            let k = P.Contents.Key.hash v in
            let () =
              match Cache.find cache k with
              | i ->
                  c.info <- i;
                  hashcons c
              | exception Not_found ->
                  c.info.hash <- Some k;
                  Log.debug (fun l -> l "Contents.to_hash: cache %a" pp_hash k);
                  Cache.add cache k c.info
            in
            let k =
              match c.info.hash with
              | Some k -> k
              | None ->
                  c.info.hash <- Some k;
                  k
            in
            k )

    let to_value t =
      match (t.v, t.info.value) with
      | _, Some v -> Lwt.return (Some v)
      | Value v, None ->
          t.info.value <- Some v;
          Lwt.return (Some v)
      | Hash (repo, k), None -> (
          Log.debug (fun l -> l "Node.Contents.to_value %a" pp_hash k);
          P.Contents.find (P.Repo.contents_t repo) k >|= function
          | None -> None
          | Some v ->
              t.info.value <- Some v;
              Some v )

    let equal (x : t) (y : t) =
      x == y || Type.equal P.Hash.t (to_hash x) (to_hash y)

    let merge : t Merge.t =
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              to_value old >|= fun c -> Ok (Some c) )
        in
        to_value x >>= fun x ->
        to_value y >>= fun y ->
        Merge.(f P.Contents.Val.merge) ~old x y >|= function
        | Ok (Some c) -> Ok (of_value c)
        | Ok None -> Error (`Conflict "empty contents")
        | Error _ as e -> e
      in
      Merge.v t f

    let fold ~force ~path f t acc =
      let aux = function None -> Lwt.return acc | Some c -> f path c acc in
      match force with
      | `True -> to_value t >>= aux
      | `False skip -> (
        match t.info.value with
        | None -> skip path acc
        | Some c -> aux (Some c) )
  end

  module Node = struct
    type value = P.Node.Val.t

    type elt = [ `Node of t | `Contents of Contents.t * Metadata.t ]

    and map = elt StepMap.t

    and info = {
      mutable value : value option;
      mutable map : map option;
      mutable hash : hash option
    }

    and v = Map of map | Hash of repo * hash | Value of repo * value

    and t = { mutable v : v; mutable info : info }

    let rec merge_map ~into:x y =
      List.iter2
        (fun (_, x) (_, y) ->
          match (x, y) with
          | `Contents (x, _), `Contents (y, _) ->
              Contents.merge_info ~into:x.Contents.info y.Contents.info
          | `Node x, `Node y -> (merge_info [@tailcall]) ~into:x.info y.info
          | _ -> assert false )
        (StepMap.bindings x) (StepMap.bindings y)

    and merge_info ~into:x y =
      let () =
        match (x.hash, y.hash) with
        | None, None | Some _, None -> ()
        | Some _, Some _ -> ()
        | None, _ -> x.hash <- y.hash
      in
      let () =
        match (x.value, y.value) with
        | None, None | Some _, None -> ()
        | Some _, Some _ -> ()
        | None, _ -> x.value <- y.value
      in
      match (x.map, y.map) with
      | None, None | Some _, None -> ()
      | None, Some _ -> x.map <- y.map
      | Some x, Some y -> (merge_map [@tailcall]) ~into:x y

    let elt t : elt Type.t =
      let open Type in
      variant "Node.value" (fun node contents -> function
        | `Node x -> node x | `Contents x -> contents x )
      |~ case1 "Node" t (fun x -> `Node x)
      |~ case1 "Contents" (pair Contents.t Metadata.t) (fun x -> `Contents x)
      |> sealv

    let map (elt : elt Type.t) : map Type.t =
      let open Type in
      let to_map x =
        List.fold_left (fun acc (k, v) -> StepMap.add k v acc) StepMap.empty x
      in
      let of_map m = StepMap.fold (fun k v acc -> (k, v) :: acc) m [] in
      map (list (pair Path.step_t elt)) to_map of_map

    let node m =
      let open Type in
      variant "Node.node" (fun map hash value -> function
        | Map m -> map m | Hash (_, y) -> hash y | Value (_, v) -> value v )
      |~ case1 "map" m (fun m -> Map m)
      |~ case1 "hash" P.Hash.t (fun _ -> assert false)
      |~ case1 "value" P.Node.Val.t (fun _ -> assert false)
      |> sealv

    let info_is_empty i = i.map = None && i.value = None

    let depth i =
      let rec map depth m k =
        StepMap.fold
          (fun _ v acc ->
            match v with
            | `Contents _ -> k (max acc (depth + 1))
            | `Node t ->
                (aux [@tailcall]) (depth + 1) t (fun d -> k (max acc d)) )
          m depth
      and aux depth t k =
        match (t.v, t.info.map) with
        | (Hash _ | Value _), None -> k 0
        | Map m, _ | _, Some m -> (map [@tailcall]) depth m k
      in
      match i.map with
      | None -> 0
      | Some m -> (map [@tailcall]) 0 m (fun x -> x)

    let width i = match i.map with None -> 0 | Some m -> StepMap.cardinal m

    let dump_info ppf i =
      let value = match i.value with None -> "<none>" | Some _ -> "<some>" in
      let map = match i.map with None -> "<none>" | Some _ -> "<some>" in
      let hash = match i.hash with None -> "<none>" | Some _ -> "<some>" in
      let empty = if info_is_empty i then "*" else "" in
      Fmt.pf ppf "[width=%d, depth=%d, value=%s, map=%s, hash=%s]%s %b"
        (width i) (depth i) value map hash empty
        (i.map = None && i.value = None)

    let clear_info ?depth:d i =
      i.hash <- None;
      i.value <- None;
      let max_depth = match d with None -> 0 | Some max_depth -> max_depth in
      let rec map depth m =
        StepMap.fold
          (fun _ v acc ->
            match v with
            | `Contents (c, _) ->
                if depth + 1 > max_depth then
                  Contents.clear_info c.Contents.info;
                acc
            | `Node t -> (aux [@tailcall]) (depth + 1) t )
          m depth
      and aux depth t =
        match (t.v, t.info.map) with
        | (Hash _ | Value _), None -> depth
        | Map m, _ | _, Some m ->
            if depth >= max_depth then t.info.map <- None;
            (map [@tailcall]) depth m
      in
      match i.map with None -> 0 | Some m -> (map [@tailcall]) 0 m

    module Cache = Cache (P.Hash)

    let cache = Cache.create 10_001

    let of_v v =
      let hash, map, value =
        match v with
        | Map m -> (None, Some m, None)
        | Hash (_, k) -> (Some k, None, None)
        | Value (_, v) -> (None, None, Some v)
      in
      (* hashcons info *)
      let info =
        match hash with
        | None -> { hash; map; value }
        | Some k -> (
          match Cache.find cache k with
          | exception Not_found ->
              let i = { hash; map; value } in
              Log.debug (fun l -> l "Node.of_v: cache %a" pp_hash k);
              Cache.add cache k i;
              i
          | i -> i )
      in
      (* hashcons v *)
      let v =
        match (v, info.map, info.hash, info.value) with
        | Map _, Some m, _, _ -> Map m
        | Hash (r, _), _, Some h, _ -> Hash (r, h)
        | Value (r, _), _, _, Some v -> Value (r, v)
        | _ -> v
      in
      { v; info }

    (* export t to the given repo and clear the cache *)
    let export ?clear repo t k =
      Log.debug (fun l ->
          l "Tree.Node.export_and_clear_cache clear=%b" (clear = Some true) );
      t.info.map <- None;
      match t.v with
      | Hash (_, k) -> t.v <- Hash (repo, k)
      | Value _ -> t.v <- Hash (repo, k)
      | Map m -> (
          if StepMap.is_empty m then ()
          else
            match t.info.hash with
            | None -> t.v <- Hash (repo, k)
            | Some k -> t.v <- Hash (repo, k) )

    let t node = Type.map node of_v (fun t -> t.v)

    let _, t =
      Type.mu2 (fun _ y ->
          let elt = elt y in
          let node = node (map elt) in
          let t = t node in
          (node, t) )

    let elt = elt t

    let dump = Type.pp_json ~minify:false t

    let of_map m = of_v (Map m)

    let of_hash repo k = of_v (Hash (repo, k))

    let of_value repo v = of_v (Value (repo, v))

    let map_of_value repo (n : value) : map =
      let entries = P.Node.Val.list n in
      let aux = function
        | `Node h -> `Node (of_hash repo h)
        | `Contents (c, m) -> `Contents (Contents.of_hash repo c, m)
      in
      List.fold_left
        (fun acc (k, v) -> StepMap.add k (aux v) acc)
        StepMap.empty entries

    let empty = of_map StepMap.empty

    let hash t =
      match (t.v, t.info.hash) with
      | Hash (_, h), None ->
          let h = Some h in
          t.info.hash <- h;
          h
      | _, h -> h

    let map t =
      match (t.v, t.info.map) with
      | Map m, None ->
          let m = Some m in
          t.info.map <- m;
          m
      | _, m -> m

    let value t =
      match (t.v, t.info.value) with
      | Value (_, v), None ->
          let v = Some v in
          t.info.value <- v;
          v
      | _, v -> v

    let hashcons t =
      match (t.v, t.info.hash, t.info.map, t.info.value) with
      | Hash (r, h), Some h', _, _ -> if h != h' then t.v <- Hash (r, h')
      | Map v, _, Some v', _ ->
          if v != v' then (
            merge_map ~into:v' v;
            t.v <- Map v' )
      | Value (r, v), _, _, Some v' -> if v != v' then t.v <- Value (r, v')
      | _ -> ()

    let hash_of_value t v =
      let k = P.Node.Key.hash v in
      let () =
        match Cache.find cache k with
        | i ->
            t.info <- i;
            hashcons t
        | exception Not_found ->
            t.info.hash <- Some k;
            Log.debug (fun l -> l "Node.hash_of_value: cache %a" pp_hash k);
            Cache.add cache k t.info
      in
      match t.info.hash with
      | Some k -> k
      | None ->
          t.info.hash <- Some k;
          k

    let rec to_hash : type a. t -> (hash -> a) -> a =
     fun t k ->
      match hash t with
      | Some h -> k h
      | None -> (
        match value t with
        | Some v -> k (hash_of_value t v)
        | None -> (
          match map t with
          | None -> assert false
          | Some m ->
              (value_of_map [@tailcall]) m @@ fun v ->
              t.info.value <- Some v;
              k (hash_of_value t v) ) )

    and value_of_map : type a. map -> (value -> a) -> a =
     fun map k ->
      let alist =
        StepMap.fold
          (fun step v acc ->
            match v with
            | `Contents (c, m) ->
                let v = `Contents (Contents.to_hash c, m) in
                (step, v) :: acc
            | `Node n ->
                (to_hash [@tailcall]) n @@ fun n ->
                let v = `Node n in
                (step, v) :: acc )
          map []
      in
      k (P.Node.Val.v alist)

    let to_hash t = to_hash t (fun t -> t)

    let value_of_map m = value_of_map m (fun t -> t)

    let to_value t =
      match value t with
      | Some v -> Lwt.return (Some v)
      | None -> (
          ( match t.v with
          | Value (_, v) -> Lwt.return (Some v)
          | Map m -> Lwt.return (Some (value_of_map m))
          | Hash (repo, k) ->
              Log.debug (fun l -> l "Tree.Node.to_value %a" pp_hash k);
              P.Node.find (P.Repo.node_t repo) k )
          >|= fun value ->
          match t.info.value with
          | None ->
              t.info.value <- value;
              value
          | Some _ as v -> v )

    let to_map t =
      match map t with
      | Some m -> Lwt.return (Some m)
      | None -> (
          let of_value repo v =
            let m = map_of_value repo v in
            t.info.map <- Some m;
            Some m
          in
          match t.v with
          | Map m -> Lwt.return (Some m)
          | Value (repo, v) -> Lwt.return (of_value repo v)
          | Hash (repo, k) -> (
              Log.debug (fun l -> l "Tree.Node.to_map %a" pp_hash k);
              P.Node.find (P.Repo.node_t repo) k >|= function
              | None -> None
              | Some v -> of_value repo v ) )

    let hash_equal x y = x == y || Type.equal P.Hash.t x y

    let contents_equal ((c1, m1) as x1) ((c2, m2) as x2) =
      x1 == x2 || (Contents.equal c1 c2 && Type.equal Metadata.t m1 m2)

    let equal (x : t) (y : t) = x == y || hash_equal (to_hash x) (to_hash y)

    let is_empty t =
      match map t with
      | Some m -> Lwt.return (StepMap.is_empty m)
      | None -> (
          to_value t >|= function
          | None -> false
          | Some n -> P.Node.Val.is_empty n )

    let list t =
      let trim l =
        List.map
          (fun (s, v) ->
            (s, match v with `Contents _ -> `Contents | `Node _ -> `Node) )
          l
      in
      match map t with
      | Some m -> Lwt.return (trim (StepMap.bindings m))
      | None -> (
          to_value t >|= function
          | None -> []
          | Some v -> trim (P.Node.Val.list v) )

    let listv t =
      to_map t >|= function None -> [] | Some m -> StepMap.bindings m

    let findv t step =
      to_map t >>= function
      | None -> Lwt.return None
      | Some m -> (
        match StepMap.find step m with
        | exception Not_found -> Lwt.return None
        | `Node n -> Lwt.return (Some (`Node n))
        | `Contents (c, m) -> (
            Contents.to_value c >|= function
            | None -> None
            | Some c -> Some (`Contents (c, m)) ) )

    let dummy_marks = Hashes.create 0

    type marks = unit Hashes.t

    let empty_marks () = Hashes.create 39

    let fold ~force ~uniq ~pre ~post ~path f t acc =
      let marks =
        match uniq with
        | `False -> dummy_marks
        | `True -> empty_marks ()
        | `Marks n -> n
      in
      let rec aux ~path acc t k =
        match force with
        | `True -> to_map t >>= fun m -> (map [@tailcall]) ~path acc m k
        | `False skip -> (
          match t.info.map with
          | Some n -> (map [@tailcall]) ~path acc (Some n) k
          | _ -> skip path acc )
      and aux_uniq ~path acc t k =
        if uniq = `False then (aux [@tailcall]) ~path acc t k
        else
          let h = to_hash t in
          if Hashes.mem marks h then k acc
          else (
            Hashes.add marks h ();
            (aux [@tailcall]) ~path acc t k )
      and step ~path acc (s, v) k =
        let path = Path.rcons path s in
        match v with
        | `Contents c -> Contents.fold ~force ~path f (fst c) acc >>= k
        | `Node n -> (aux_uniq [@tailcall]) ~path acc n k
      and steps ~path acc s k =
        match s with
        | [] -> k acc
        | h :: t ->
            (step [@tailcall]) ~path acc h @@ fun acc ->
            (steps [@tailcall]) ~path acc t k
      and map ~path acc m k =
        match m with
        | None -> k acc
        | Some m ->
            let bindings = StepMap.bindings m in
            let s = List.map fst bindings in
            pre path s acc >>= fun acc ->
            (steps [@tailcall]) ~path acc bindings @@ fun acc ->
            post path s acc >>= k
      in
      aux_uniq ~path acc t Lwt.return

    let remove t step =
      to_map t >|= function
      | None -> t
      | Some n ->
          if not (StepMap.mem step n) then t
          else of_map (StepMap.remove step n)

    let add t step x =
      match x with
      | (`Node _ | `Contents _) as v -> (
          let v =
            match v with
            | `Node _ as n -> fun _ -> n
            | `Contents (`Set (c, m)) ->
                fun _ -> `Contents (Contents.of_value c, m)
            | `Contents (`Keep c) -> (
                function
                | Some m -> `Contents (Contents.of_value c, m)
                | None -> `Contents (Contents.of_value c, Metadata.default) )
          in
          to_map t >>= function
          | None ->
              v (Some Metadata.default) |> StepMap.singleton step |> of_map
              |> Lwt.return
          | Some m ->
              let previous =
                try Some (StepMap.find step m) with Not_found -> None
              in
              let previous_m =
                match previous with
                | None | Some (`Node _) -> None
                | Some (`Contents (_, m)) -> Some m
              in
              let v = v previous_m in
              Lwt.return (of_map (StepMap.add step v m)) )

    let rec merge : type a. (t Merge.t -> a) -> a =
     fun k ->
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              to_map old >|= fun m -> Ok (Some m) )
        in
        to_map x >>= fun x ->
        to_map y >>= fun y ->
        let m =
          StepMap.merge elt (fun _step -> (merge_elt [@tailcall]) Merge.option)
        in
        Merge.(f @@ option m) ~old x y >|= function
        | Ok (Some map) -> Ok (of_map map)
        | Ok None -> Error (`Conflict "empty map")
        | Error _ as e -> e
      in
      k (Merge.v t f)

    and merge_elt : type a. (elt Merge.t -> a) -> a =
     fun k ->
      let open Merge.Infix in
      let f : elt Merge.f =
       fun ~old x y ->
        match (x, y) with
        | `Contents (x, cx), `Contents (y, cy) ->
            let mold =
              Merge.bind_promise old (fun old () ->
                  match old with
                  | `Contents (_, m) -> Lwt.return (Ok (Some m))
                  | `Node _ -> Lwt.return (Ok None) )
            in
            Merge.(f Metadata.merge) ~old:mold cx cy >>=* fun m ->
            let old =
              Merge.bind_promise old (fun old () ->
                  match old with
                  | `Contents (c, _) -> Lwt.return (Ok (Some c))
                  | `Node _ -> Lwt.return (Ok None) )
            in
            Merge.(f Contents.merge) ~old x y >>=* fun c ->
            Merge.ok (`Contents (c, m))
        | `Node x, `Node y ->
            (merge [@tailcall]) (fun m ->
                let old =
                  Merge.bind_promise old (fun old () ->
                      match old with
                      | `Contents _ -> Lwt.return (Ok None)
                      | `Node n -> Lwt.return (Ok (Some n)) )
                in
                Merge.(f m ~old x y) >>=* fun n -> Merge.ok (`Node n) )
        | _ -> Merge.conflict "add/add values"
      in
      k (Merge.seq [ Merge.default elt; Merge.v elt f ])

    let merge_elt = merge_elt (fun x -> x)
  end

  type node = Node.t

  type metadata = Metadata.t

  type tree = [ `Node of node | `Contents of contents * metadata ]

  let of_private_node = Node.of_value

  let to_private_node = Node.to_value

  let node_t = Node.t

  let tree_t =
    let open Type in
    variant "tree" (fun node contents -> function
      | `Node n -> node n | `Contents c -> contents c )
    |~ case1 "node" Node.t (fun n -> `Node n)
    |~ case1 "contents" (pair P.Contents.Val.t Metadata.t) (fun c ->
           `Contents c )
    |> sealv

  let dump ppf = function
    | `Node n -> Fmt.pf ppf "node: %a" Node.dump n
    | `Contents (c, _) ->
        Fmt.pf ppf "contents: %a" (Type.pp P.Contents.Val.t) c

  let contents_equal ((c1, m1) as x1) ((c2, m2) as x2) =
    x1 == x2
    || (Type.equal P.Contents.Val.t c1 c2 && Type.equal Metadata.t m1 m2)

  let equal (x : tree) (y : tree) =
    x == y
    ||
    match (x, y) with
    | `Node x, `Node y -> Node.equal x y
    | `Contents x, `Contents y -> contents_equal x y
    | `Node _, `Contents _ | `Contents _, `Node _ -> false

  let empty = `Node Node.empty

  let is_empty = function
    | `Node n -> Node.is_empty n
    | `Contents _ -> Lwt.return false

  let of_node n = `Node n

  let of_contents ?(metadata = Metadata.default) c = `Contents (c, metadata)

  let clear ?depth = function
    | `Node n ->
        let (_ : int) = Node.clear_info ?depth n.Node.info in
        ()
    | `Contents _ -> ()

  let sub t path =
    let rec aux node path =
      match Path.decons path with
      | None -> Lwt.return (Some node)
      | Some (h, p) -> (
          Node.findv node h >>= function
          | None | Some (`Contents _) -> Lwt.return_none
          | Some (`Node n) -> (aux [@tailcall]) n p )
    in
    match t with
    | `Node n -> (aux [@tailcall]) n path
    | `Contents _ -> Lwt.return_none

  let find_tree (t : tree) path =
    Log.debug (fun l -> l "Tree.find_tree %a" pp_path path);
    match (t, Path.rdecons path) with
    | v, None -> Lwt.return (Some v)
    | _, Some (path, file) -> (
        sub t path >>= function
        | None -> Lwt.return None
        | Some n -> Node.findv n file )

  type marks = Node.marks

  let empty_marks = Node.empty_marks

  type 'a force = [ `True | `False of key -> 'a -> 'a Lwt.t ]

  type uniq = [ `False | `True | `Marks of marks ]

  type 'a node_fn = key -> step list -> 'a -> 'a Lwt.t

  let id _ _ acc = Lwt.return acc

  let fold ?(force = `True) ?(uniq = `False) ?(pre = id) ?(post = id) f
      (t : tree) acc =
    match t with
    | `Contents v -> f Path.empty (fst v) acc
    | `Node n -> Node.fold ~force ~uniq ~pre ~post ~path:Path.empty f n acc

  type stats = {
    nodes : int;
    leafs : int;
    skips : int;
    depth : int;
    width : int
  }

  let empty_stats = { nodes = 0; leafs = 0; skips = 0; depth = 0; width = 0 }

  let pp_stats ppf { nodes; leafs; skips; depth; width } =
    Fmt.pf ppf "{@[nodes=%d; leafs=%d; skips=%d; depth=%d; width=%d]}" nodes
      leafs skips depth width

  let incr_nodes s = { s with nodes = s.nodes + 1 }

  let incr_leafs s = { s with leafs = s.leafs + 1 }

  let incr_skips s = { s with skips = s.skips + 1 }

  let set_depth p s =
    let n_depth = List.length (Path.map p (fun _ -> ())) in
    let depth = max n_depth s.depth in
    { s with depth }

  let set_width childs s =
    let width = max s.width (List.length childs) in
    { s with width }

  let stats ?(force = false) (t : tree) =
    let force =
      if force then `True
      else `False (fun k s -> set_depth k s |> incr_skips |> Lwt.return)
    in
    let f k _ s = set_depth k s |> incr_leafs |> Lwt.return in
    let pre k childs s =
      if childs = [] then Lwt.return s
      else set_depth k s |> set_width childs |> incr_nodes |> Lwt.return
    in
    let post _ _ acc = Lwt.return acc in
    fold ~force ~pre ~post f t empty_stats

  let err_not_found n k =
    Fmt.kstrf invalid_arg "Irmin.Tree.%s: %a not found" n pp_path k

  let get_tree (t : tree) path =
    find_tree t path >|= function
    | None -> err_not_found "get_tree" path
    | Some v -> v

  let find_all t k =
    find_tree t k >|= function
    | None | Some (`Node _) -> None
    | Some (`Contents c) -> Some c

  let find t k =
    find_all t k >|= function None -> None | Some (c, _) -> Some c

  let get_all t k =
    find_all t k >>= function
    | None -> err_not_found "get" k
    | Some v -> Lwt.return v

  let get t k = get_all t k >|= fun (c, _) -> c

  let mem t k = find t k >|= function None -> false | _ -> true

  let mem_tree t k = find_tree t k >|= function None -> false | _ -> true

  let kind t path =
    Log.debug (fun l -> l "Tree.kind %a" pp_path path);
    match (t, Path.rdecons path) with
    | `Contents _, None -> Lwt.return (Some `Contents)
    | _, None -> Lwt.return None
    | _, Some (dir, file) -> (
        sub t dir >>= function
        | None -> Lwt.return None
        | Some m -> (
            Node.findv m file >>= function
            | None -> Lwt.return None
            | Some (`Contents _) -> Lwt.return (Some `Contents)
            | Some (`Node _) -> Lwt.return (Some `Node) ) )

  let list t path =
    Log.debug (fun l -> l "Tree.list %a" pp_path path);
    sub t path >>= function None -> Lwt.return [] | Some n -> Node.list n

  let may_remove t k =
    Node.findv t k >>= function
    | None -> Lwt.return_none
    | Some _ -> Node.remove t k >>= fun t -> Lwt.return (Some t)

  let remove t k =
    Log.debug (fun l -> l "Tree.remove %a" pp_path k);
    match Path.rdecons k with
    | None ->
        is_empty t >>= fun is_empty ->
        if is_empty then Lwt.return t else Lwt.return empty
    | Some (path, file) -> (
        let rec aux view path k =
          let some n = k (Some n) in
          match Path.decons path with
          | None -> may_remove view file >>= k
          | Some (h, p) -> (
              Node.findv view h >>= function
              | None | Some (`Contents _) -> k None
              | Some (`Node child) ->
                  (aux [@tailcall]) child p (function
                    | None -> k None
                    | Some child' -> (
                        (* remove empty dirs *)
                        Node.is_empty child'
                        >>= function
                        | true -> may_remove view h >>= k
                        | false -> Node.add view h (`Node child') >>= some ) )
              )
        in
        let n = match t with `Node n -> n | _ -> Node.empty in
        (aux [@tailcall]) n path @@ function
        | None -> Lwt.return t
        | Some n -> Lwt.return (`Node n) )

  let with_setm = function
    | `Node _ as n -> n
    | `Contents c -> `Contents (`Set c)

  let add_tree t k v =
    Log.debug (fun l -> l "Tree.add_tree %a" pp_path k);
    match Path.rdecons k with
    | None -> Lwt.return v
    | Some (path, file) -> (
        let rec aux view path k =
          let some n = k (Some n) in
          match Path.decons path with
          | None -> (
              Node.findv view file >>= function
              | old -> (
                match old with
                | None -> Node.add view file (with_setm v) >>= some
                | Some old ->
                    if equal old v then k None
                    else Node.add view file (with_setm v) >>= some ) )
          | Some (h, p) -> (
              Node.findv view h >>= function
              | None | Some (`Contents _) ->
                  (aux [@tailcall]) Node.empty p (function
                    | None -> k None
                    | Some child' -> Node.add view h (`Node child') >>= some )
              | Some (`Node child) ->
                  (aux [@tailcall]) child p (function
                    | None -> k None
                    | Some child' -> Node.add view h (`Node child') >>= some )
              )
        in
        let n = match t with `Node n -> n | _ -> Node.empty in
        (aux [@tailcall]) n path @@ function
        | None -> Lwt.return t
        | Some node -> Lwt.return (`Node node) )

  let with_optm m c =
    match m with
    | None -> `Contents (`Keep c)
    | Some m -> `Contents (`Set (c, m))

  let add t k ?metadata c =
    Log.debug (fun l -> l "Tree.add %a" pp_path k);
    match Path.rdecons k with
    | None -> (
      match (metadata, t) with
      | None, `Contents (c', _) when Type.equal P.Contents.Val.t c' c ->
          Lwt.return t
      | None, `Contents (_, m) -> Lwt.return (`Contents (c, m))
      | None, _ -> Lwt.return (`Contents (c, Metadata.default))
      | Some m, `Contents c' when contents_equal c' (c, m) -> Lwt.return t
      | Some m, _ -> Lwt.return (`Contents (c, m)) )
    | Some (path, file) -> (
        let rec aux view path k =
          let some n = k (Some n) in
          match Path.decons path with
          | None -> (
              Node.findv view file >>= function
              | old -> (
                match old with
                | Some (`Node _) | None ->
                    Node.add view file (with_optm metadata c) >>= some
                | Some (`Contents (_, oldm) as old) ->
                    let m = match metadata with None -> oldm | Some m -> m in
                    if equal old (`Contents (c, m)) then k None
                    else Node.add view file (`Contents (`Set (c, m))) >>= some
                ) )
          | Some (h, p) -> (
              Node.findv view h >>= function
              | None | Some (`Contents _) ->
                  (aux [@tailcall]) Node.empty p (function
                    | None -> assert false
                    | Some child -> Node.add view h (`Node child) >>= some )
              | Some (`Node child) ->
                  (aux [@tailcall]) child p (function
                    | None -> k None
                    | Some child' -> Node.add view h (`Node child') >>= some )
              )
        in
        let n = match t with `Node n -> n | _ -> Node.empty in
        (aux [@tailcall]) n path @@ function
        | None -> Lwt.return t
        | Some n -> Lwt.return (`Node n) )

  let import repo k =
    P.Node.mem (P.Repo.node_t repo) k >|= function
    | true -> Some (Node.of_hash repo k)
    | false -> None

  let import_no_check repo k = Node.of_hash repo k

  let export ?clear repo contents_t node_t n =
    let seen = Hashtbl.create 127 in
    let add_node n v () =
      P.Node.add node_t v >|= fun k ->
      let k' = Node.to_hash n in
      assert (Type.equal P.Hash.t k k');
      Node.export ?clear repo n k
    in
    let add_node_map n x () = add_node n (Node.value_of_map x) () in
    let add_contents c x () =
      P.Contents.add contents_t x >|= fun k ->
      let k' = Contents.to_hash c in
      assert (Type.equal P.Hash.t k k');
      Contents.export ?clear repo c k
    in
    let todo = Stack.create () in
    let rec add_to_todo : type a. _ -> (unit -> a Lwt.t) -> a Lwt.t =
     fun n k ->
      let h = Node.to_hash n in
      match Hashtbl.find seen h with
      | _ -> k ()
      | exception Not_found -> (
          Hashtbl.add seen h ();
          match n.Node.v with
          | Node.Hash _ -> k ()
          | Node.Value (_, x) ->
              Stack.push (add_node n x) todo;
              k ()
          | Node.Map x -> (
              (* 1. we push the current node job on the stack. *)
              P.Node.mem node_t h
              >>= function
              | true -> k ()
              | false ->
                  Stack.push (add_node_map n x) todo;
                  let contents = ref [] in
                  let nodes = ref [] in
                  StepMap.iter
                    (fun _ -> function
                      | `Contents c -> contents := c :: !contents
                      | `Node n -> nodes := n :: !nodes )
                    x;
                  (* 2. we push the contents job on the stack. *)
                  List.iter
                    (fun (c, _) ->
                      let h = Contents.to_hash c in
                      match Hashtbl.find seen h with
                      | _ -> ()
                      | exception Not_found -> (
                          Hashtbl.add seen h ();
                          match c.Contents.v with
                          | Contents.Hash _ -> ()
                          | Contents.Value x ->
                              Stack.push (add_contents c x) todo ) )
                    !contents;
                  (* 3. we push the children jobs on the stack. *)
                  List.iter
                    (fun n ->
                      Stack.push
                        (fun () -> (add_to_todo [@tailcall]) n Lwt.return)
                        todo )
                    !nodes;
                  k () ) )
    in
    let rec loop () =
      let task = try Some (Stack.pop todo) with Stack.Empty -> None in
      match task with None -> Lwt.return_unit | Some t -> t () >>= loop
    in
    (add_to_todo [@tailcall]) n @@ fun () ->
    loop () >|= fun () ->
    let x = Node.to_hash n in
    Log.debug (fun l -> l "Tree.export -> %a" pp_hash x);
    x

  let merge : tree Merge.t =
    let f ~old (x : tree) y =
      let to_node x =
        match x with
        | `Node _ as x -> x
        | `Contents (c, m) -> `Contents (Contents.of_value c, m)
      in
      let x = to_node x in
      let y = to_node y in
      let old =
        Merge.bind_promise old (fun old -> Merge.promise (to_node old))
      in
      Merge.(f Node.merge_elt) ~old x y >>= function
      | Ok (`Contents (c, m)) -> (
          Contents.to_value c >>= function
          | None -> Merge.conflict "conflict: contents"
          | Some c -> Merge.ok (`Contents (c, m)) )
      | Ok (`Node _ as n) -> Merge.ok n
      | Error _ as e -> Lwt.return e
    in
    Merge.v tree_t f

  let entries path tree =
    let rec aux acc = function
      | [] -> Lwt.return acc
      | (path, h) :: todo ->
          Node.listv h >>= fun childs ->
          let acc, todo =
            List.fold_left
              (fun (acc, todo) (k, v) ->
                let path = Path.rcons path k in
                match v with
                | `Node v -> (acc, (path, v) :: todo)
                | `Contents c -> ((path, c) :: acc, todo) )
              (acc, todo) childs
          in
          (aux [@tailcall]) acc todo
    in
    (aux [@tailcall]) [] [ (path, tree) ]

  let diff_node (x : node) (y : node) =
    let bindings n =
      Node.to_map n >|= function None -> [] | Some m -> StepMap.bindings m
    in
    let removed acc (k, (c, m)) =
      Contents.to_value c >|= function
      | None -> acc
      | Some c -> (k, `Removed (c, m)) :: acc
    in
    let added acc (k, (c, m)) =
      Contents.to_value c >|= function
      | None -> acc
      | Some c -> (k, `Added (c, m)) :: acc
    in
    let rec aux acc = function
      | [] -> Lwt.return acc
      | (path, x, y) :: todo ->
          if Node.equal x y then (aux [@tailcall]) acc todo
          else
            bindings x >>= fun x ->
            bindings y >>= fun y ->
            let acc = ref acc in
            let todo = ref todo in
            alist_iter2_lwt
              Type.(compare @@ Path.step_t)
              (fun key v ->
                let path = Path.rcons path key in
                match v with
                (* Left *)
                | `Left (`Contents x) ->
                    removed !acc (path, x) >|= fun x -> acc := x
                | `Left (`Node x) ->
                    entries path x >>= fun xs ->
                    Lwt_list.fold_left_s removed !acc xs >|= fun xs ->
                    acc := xs
                (* Right *)
                | `Right (`Contents y) ->
                    added !acc (path, y) >|= fun y -> acc := y
                | `Right (`Node y) ->
                    entries path y >>= fun ys ->
                    Lwt_list.fold_left_s added !acc ys >|= fun ys -> acc := ys
                (* Both *)
                | `Both (`Node x, `Node y) ->
                    todo := (path, x, y) :: !todo;
                    Lwt.return_unit
                | `Both (`Contents x, `Node y) ->
                    entries path y >>= fun ys ->
                    removed !acc (path, x) >>= fun x ->
                    Lwt_list.fold_left_s added x ys >|= fun ys -> acc := ys
                | `Both (`Node x, `Contents y) ->
                    entries path x >>= fun xs ->
                    added !acc (path, y) >>= fun y ->
                    Lwt_list.fold_left_s removed y xs >|= fun ys -> acc := ys
                | `Both (`Contents x, `Contents y) -> (
                    if Node.contents_equal x y then Lwt.return_unit
                    else
                      Contents.to_value (fst x) >>= fun cx ->
                      Contents.to_value (fst y) >|= fun cy ->
                      match (cx, cy) with
                      | None, None -> ()
                      | Some cx, None ->
                          let x = (cx, snd x) in
                          acc := (path, `Removed x) :: !acc
                      | None, Some cy ->
                          let y = (cy, snd y) in
                          acc := (path, `Added y) :: !acc
                      | Some cx, Some cy ->
                          let x = (cx, snd x) in
                          let y = (cy, snd y) in
                          acc := (path, `Updated (x, y)) :: !acc ) )
              x y
            >>= fun () -> (aux [@tailcall]) !acc !todo
    in
    (aux [@tailcall]) [] [ (Path.empty, x, y) ]

  let diff (x : tree) (y : tree) =
    match (x, y) with
    | `Contents x, `Contents y ->
        if contents_equal x y then Lwt.return []
        else Lwt.return [ (Path.empty, `Updated (y, x)) ]
    | `Node x, `Node y -> diff_node x y
    | `Contents x, `Node y ->
        diff_node Node.empty y >|= fun diff -> (Path.empty, `Removed x) :: diff
    | `Node x, `Contents y ->
        diff_node x Node.empty >|= fun diff -> (Path.empty, `Added y) :: diff

  type concrete =
    [ `Tree of (step * concrete) list | `Contents of contents * metadata ]

  let of_concrete c =
    let rec concrete k = function
      | `Contents _ as v -> k v
      | `Tree childs -> tree StepMap.empty (fun n -> k (`Node n)) childs
    and contents k (c, m) = k (`Contents (Contents.of_value c, m))
    and tree map k = function
      | [] -> k (Node.of_map map)
      | (s, n) :: t ->
          (concrete [@tailcall])
            (function
              | `Contents c ->
                  (contents [@tailcall])
                    (fun v -> (tree [@tailcall]) (StepMap.add s v map) k t)
                    c
              | `Node _ as v -> (tree [@tailcall]) (StepMap.add s v map) k t )
            n
    in
    (concrete [@tailcall]) (fun x -> x) c

  let to_concrete t =
    let rec tree k = function
      | `Contents _ as v -> k v
      | `Node n -> (
          Node.to_map n >>= function
          | None -> k (`Tree [])
          | Some n ->
              (node [@tailcall]) [] (fun n -> k (`Tree n)) (StepMap.bindings n)
          )
    and contents k (c, m) =
      Contents.to_value c >>= function
      | None -> k None
      | Some c -> k @@ Some (`Contents (c, m))
    and node childs k = function
      | [] -> k childs
      | (s, n) :: t -> (
        match n with
        | `Node _ as n ->
            (tree [@tailcall]) (fun tree -> node ((s, tree) :: childs) k t) n
        | `Contents c ->
            (contents [@tailcall])
              (function
                | None -> (node [@tailcall]) childs k t
                | Some c -> (node [@tailcall]) ((s, c) :: childs) k t )
              c )
    in
    tree (fun x -> Lwt.return x) t

  let hash (t : tree) =
    match t with
    | `Node n -> `Node (Node.to_hash n)
    | `Contents (c, m) -> `Contents (P.Contents.Key.hash c, m)

  module Cache = struct
    let length () =
      ( `Contents Contents.(Cache.length cache),
        `Nodes Node.(Cache.length cache) )

    let clear ?depth () =
      Log.info (fun l -> l "Tree.Cache.clear");
      match depth with
      | None ->
          Contents.Cache.clear Contents.cache;
          Node.Cache.clear Node.cache
      | Some depth ->
          let to_remove = ref [] in
          Node.Cache.iter
            (fun h i ->
              let d = Node.clear_info ~depth i in
              if d >= depth then to_remove := h :: !to_remove )
            Node.cache;
          List.iter (Node.Cache.remove Node.cache) !to_remove

    let dump ppf () =
      let ppo t ppf = function
        | None -> Fmt.pf ppf "<none>"
        | Some y -> Type.pp t ppf y
      in
      Contents.Cache.iter
        (fun k v ->
          if not (Contents.info_is_empty v) then
            Fmt.pf ppf "C|%a: value:%a@." pp_hash k (ppo P.Contents.Val.t)
              v.Contents.value )
        Contents.cache;
      Node.Cache.iter
        (fun k v ->
          if not (Node.info_is_empty v) then
            Fmt.pf ppf "N|%a: %a@." pp_hash k Node.dump_info v )
        Node.cache
  end
end
