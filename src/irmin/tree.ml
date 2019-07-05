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

module Cache (K : Type.S) : sig
  type 'a t

  type key = K.t

  val create : is_empty:('a -> bool) -> int -> 'a t

  val find : 'a t -> key -> 'a

  val add : 'a t -> key -> 'a -> unit

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val length : 'a t -> int
end = struct
  module M = Ephemeron.K1.Make (struct
    type t = K.t

    let equal (x : t) (y : t) = Type.equal K.t x y

    let hash (x : t) = Type.short_hash K.t x
  end)

  type 'a t = { is_empty : 'a -> bool; m : 'a M.t }

  type key = K.t

  let create ~is_empty x = { is_empty; m = M.create x }

  let length t =
    M.fold (fun _ v acc -> if t.is_empty v then acc else acc + 1) t.m 0

  let add t k v = M.add t.m k v

  let find t k = M.find t.m k

  let iter f t = M.iter f t.m
end

module Make (P : S.PRIVATE) = struct
  type counters = {
    mutable contents_hash : int;
    mutable contents_find : int;
    mutable contents_add : int;
    mutable contents_cache_length : int;
    mutable contents_cache_find : int;
    mutable contents_cache_miss : int;
    mutable node_hash : int;
    mutable node_mem : int;
    mutable node_add : int;
    mutable node_find : int;
    mutable node_cache_length : int;
    mutable node_cache_find : int;
    mutable node_cache_miss : int;
    mutable node_val_v : int;
    mutable node_val_find : int;
    mutable node_val_list : int
  }

  let counters_t =
    let open Type in
    record "counters"
      (fun contents_hash
      contents_find
      contents_add
      contents_cache_length
      contents_cache_find
      contents_cache_miss
      node_hash
      node_mem
      node_add
      node_find
      node_cache_length
      node_cache_find
      node_cache_miss
      node_val_v
      node_val_find
      node_val_list
      ->
        { contents_hash;
          contents_find;
          contents_add;
          contents_cache_length;
          contents_cache_find;
          contents_cache_miss;
          node_hash;
          node_mem;
          node_add;
          node_find;
          node_cache_length;
          node_cache_find;
          node_cache_miss;
          node_val_v;
          node_val_find;
          node_val_list
        } )
    |+ field "contents_hash" int (fun x -> x.contents_hash)
    |+ field "contents_find" int (fun x -> x.contents_find)
    |+ field "contents_add" int (fun x -> x.contents_add)
    |+ field "contents_cache_length" int (fun x -> x.contents_cache_length)
    |+ field "contents_cache_find" int (fun x -> x.contents_cache_find)
    |+ field "contents_cache_miss" int (fun x -> x.contents_cache_miss)
    |+ field "node_hash" int (fun x -> x.node_hash)
    |+ field "node_mem" int (fun x -> x.node_mem)
    |+ field "node_add" int (fun x -> x.node_add)
    |+ field "node_find" int (fun x -> x.node_find)
    |+ field "node_cache_length" int (fun x -> x.node_cache_length)
    |+ field "node_cache_find" int (fun x -> x.node_cache_find)
    |+ field "node_cache_miss" int (fun x -> x.node_cache_miss)
    |+ field "node_val_v" int (fun x -> x.node_val_v)
    |+ field "node_val_find" int (fun x -> x.node_val_find)
    |+ field "node_val_list" int (fun x -> x.node_val_list)
    |> sealr

  let dump_counters ppf t = Type.pp_json ~minify:false counters_t ppf t

  let fresh_counters () =
    { contents_hash = 0;
      contents_add = 0;
      contents_find = 0;
      contents_cache_length = 0;
      contents_cache_find = 0;
      contents_cache_miss = 0;
      node_hash = 0;
      node_mem = 0;
      node_add = 0;
      node_find = 0;
      node_cache_length = 0;
      node_cache_find = 0;
      node_cache_miss = 0;
      node_val_v = 0;
      node_val_find = 0;
      node_val_list = 0
    }

  let reset_counters t =
    t.contents_hash <- 0;
    t.contents_add <- 0;
    t.contents_find <- 0;
    t.contents_cache_length <- 0;
    t.contents_cache_find <- 0;
    t.contents_cache_miss <- 0;
    t.node_hash <- 0;
    t.node_mem <- 0;
    t.node_add <- 0;
    t.node_find <- 0;
    t.node_cache_length <- 0;
    t.node_cache_find <- 0;
    t.node_cache_miss <- 0;
    t.node_val_v <- 0;
    t.node_val_find <- 0;
    t.node_val_list <- 0

  let cnt = fresh_counters ()

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

    type info = {
      mutable hash : hash option;
      mutable value : contents option;
      mutable color : [ `White | `Black ]
    }

    type t = { mutable v : v; mutable info : info }

    module Hashes = Cache (P.Hash)
    module Values = Cache (P.Contents.Val)

    let info_is_empty i = i.hash = None && i.value = None

    let values = Values.create ~is_empty:info_is_empty 1000

    let hashes = Hashes.create ~is_empty:info_is_empty 1000

    let iter_info f =
      Values.iter (fun _ i -> f i) values;
      Hashes.iter (fun _ i -> f i) hashes

    let mark color i = i.color <- color

    let v =
      let open Type in
      variant "Node.Contents.v" (fun hash value -> function
        | Hash (_, x) -> hash x | Value v -> value v )
      |~ case1 "hash" P.Hash.t (fun _ -> assert false)
      |~ case1 "value" P.Contents.Val.t (fun v -> Value v)
      |> sealv

    let clear_info i =
      if not (info_is_empty i) then (
        i.value <- None;
        i.hash <- None )

    let clear t = clear_info t.info

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

    let of_v v =
      let hash, value =
        match v with Hash (_, k) -> (Some k, None) | Value v -> (None, Some v)
      in
      (* hashcons the info *)
      let info =
        match (hash, value) with
        | Some _, Some _ -> assert false
        | None, None -> { hash; value; color = `White }
        | None, Some v -> (
            cnt.contents_cache_find <- cnt.contents_cache_find + 1;
            match Values.find values v with
            | exception Not_found ->
                cnt.contents_cache_miss <- cnt.contents_cache_miss + 1;
                let i = { hash; value; color = `White } in
                Values.add values v i;
                i
            | i -> i )
        | Some k, None -> (
            cnt.contents_cache_find <- cnt.contents_cache_find + 1;
            match Hashes.find hashes k with
            | exception Not_found ->
                cnt.contents_cache_miss <- cnt.contents_cache_miss + 1;
                let i = { hash; value; color = `White } in
                Hashes.add hashes k i;
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

    let export ?clear:c repo t k =
      let hash = t.info.hash in
      if c = Some true then clear t;
      match (t.v, hash) with
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

    let hash_of_value c v =
      cnt.contents_hash <- cnt.contents_hash + 1;
      let k = P.Contents.Key.hash v in
      c.info.hash <- Some k;
      let () =
        cnt.contents_cache_find <- cnt.contents_cache_find + 1;
        match Hashes.find hashes k with
        | i ->
            let old = c.info in
            c.info <- i;
            merge_info ~into:i old;
            hashcons c
        | exception Not_found ->
            cnt.contents_cache_miss <- cnt.contents_cache_miss + 1;
            Hashes.add hashes k c.info
      in
      match c.info.hash with Some k -> k | None -> k

    let to_hash c =
      match hash c with
      | Some k -> k
      | None -> (
        match value c with None -> assert false | Some v -> hash_of_value c v )

    let value_of_hash t repo k =
      cnt.contents_find <- cnt.contents_find + 1;
      P.Contents.find (P.Repo.contents_t repo) k >|= function
      | None -> None
      | Some v as vo ->
          t.info.value <- vo;
          let () =
            cnt.contents_cache_find <- cnt.contents_cache_find + 1;
            match Values.find values v with
            | i ->
                let old = t.info in
                t.info <- i;
                merge_info ~into:i old;
                hashcons t
            | exception Not_found ->
                cnt.contents_cache_miss <- cnt.contents_cache_miss + 1;
                Values.add values v t.info
          in
          t.info.value

    let to_value t =
      match value t with
      | Some v -> Lwt.return (Some v)
      | None -> (
        match t.v with
        | Value v -> Lwt.return (Some v)
        | Hash (repo, k) -> value_of_hash t repo k )

    let equal (x : t) (y : t) =
      x == y
      ||
      match (x.v, y.v) with
      | Hash (_, x), Hash (_, y) -> Type.equal P.Hash.t x y
      | Value x, Value y -> Type.equal P.Contents.Val.t x y
      | _ -> Type.equal P.Hash.t (to_hash x) (to_hash y)

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
      mutable hash : hash option;
      mutable color : [ `White | `Black ]
    }

    and v =
      | Map of map
      | Hash of repo * hash
      | Value of repo * value * map option

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

    let elt_t t : elt Type.t =
      let open Type in
      variant "Node.value" (fun node contents -> function
        | `Node x -> node x | `Contents x -> contents x )
      |~ case1 "Node" t (fun x -> `Node x)
      |~ case1 "Contents" (pair Contents.t Metadata.t) (fun x -> `Contents x)
      |> sealv

    let map_t (elt : elt Type.t) : map Type.t =
      let open Type in
      let to_map x =
        List.fold_left (fun acc (k, v) -> StepMap.add k v acc) StepMap.empty x
      in
      let of_map m = StepMap.fold (fun k v acc -> (k, v) :: acc) m [] in
      map (list (pair Path.step_t elt)) to_map of_map

    let v_t (m : map Type.t) : v Type.t =
      let open Type in
      variant "Node.node" (fun map hash value -> function
        | Map m -> map m
        | Hash (_, y) -> hash y
        | Value (_, v, m) -> value (v, m) )
      |~ case1 "map" m (fun m -> Map m)
      |~ case1 "hash" P.Hash.t (fun _ -> assert false)
      |~ case1 "value" (pair P.Node.Val.t (option m)) (fun _ -> assert false)
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
      Fmt.pf ppf "[width=%d, depth=%d, value=%s, map=%s, hash=%s]%s" (width i)
        (depth i) value map hash empty

    module Cache = Cache (P.Hash)

    let cache = Cache.create ~is_empty:info_is_empty 100

    let iter_info f = Cache.iter (fun _ i -> f i) cache

    let mark c t = t.color <- c

    let of_v v =
      let hash, map, value =
        match v with
        | Map m -> (None, Some m, None)
        | Hash (_, k) -> (Some k, None, None)
        | Value (_, v, None) -> (None, None, Some v)
        | Value _ -> (None, None, None)
      in
      (* hashcons info *)
      let info =
        match hash with
        | None -> { hash; map; value; color = `White }
        | Some k -> (
            cnt.node_cache_find <- cnt.node_cache_find + 1;
            match Cache.find cache k with
            | exception Not_found ->
                cnt.node_cache_miss <- cnt.node_cache_miss + 1;
                let i = { hash; map; value; color = `White } in
                Cache.add cache k i;
                i
            | i -> i )
      in
      (* hashcons v *)
      let v =
        match (v, info.map, info.hash, info.value) with
        | Map _, Some m, _, _ -> Map m
        | Hash (r, _), _, Some h, _ -> Hash (r, h)
        | Value (r, _, None), _, _, Some v -> Value (r, v, None)
        | _ -> v
      in
      { v; info }

    let rec clear_map ~max_depth depth m =
      List.iter
        (fun (_, v) ->
          match v with
          | `Contents (c, _) ->
              Contents.mark `Black c.Contents.info;
              if depth + 1 > max_depth then Contents.clear c
          | `Node t -> (clear [@tailcall]) ~max_depth (depth + 1) t )
        m

    and clear_info ~max_depth ?v depth i =
      if i.color = `White then (
        let added =
          match v with
          | Some (Value (_, _, Some m)) -> StepMap.bindings m
          | _ -> []
        in
        let map =
          match (v, i.map) with
          | Some (Map m), _ | _, Some m -> StepMap.bindings m
          | _ -> []
        in
        if depth >= max_depth && not (info_is_empty i) then (
          i.color <- `Black;
          i.value <- None;
          i.map <- None;
          i.hash <- None );
        (clear_map [@tailcall]) ~max_depth depth (map @ added) )

    and clear ~max_depth depth t = clear_info ~v:t.v ~max_depth depth t.info

    let clear_info ?depth:d i =
      let max_depth = match d with None -> 0 | Some max_depth -> max_depth in
      clear_info ~max_depth 0 i

    let clear ?depth:d n =
      let max_depth = match d with None -> 0 | Some max_depth -> max_depth in
      clear ~max_depth 0 n

    (* export t to the given repo and clear the cache *)
    let export ?clear:c repo t k =
      let hash = t.info.hash in
      if c = Some true then clear t;
      match t.v with
      | Hash (_, k) -> t.v <- Hash (repo, k)
      | Value (_, v, None) when P.Node.Val.is_empty v -> ()
      | Map m when StepMap.is_empty m -> ()
      | _ -> (
        match hash with
        | None -> t.v <- Hash (repo, k)
        | Some k -> t.v <- Hash (repo, k) )

    let t node = Type.map node of_v (fun t -> t.v)

    let _, t =
      Type.mu2 (fun _ y ->
          let elt = elt_t y in
          let v = v_t (map_t elt) in
          let t = t v in
          (v, t) )

    let elt_t = elt_t t

    let dump = Type.pp_json ~minify:false t

    let of_map m = of_v (Map m)

    let of_hash repo k = of_v (Hash (repo, k))

    let of_value ?added repo v = of_v (Value (repo, v, added))

    let empty = function
      | { v = Hash (repo, _) | Value (repo, _, _); _ } ->
          of_value repo P.Node.Val.empty
      | _ -> of_map StepMap.empty

    let map_of_value repo (n : value) : map =
      cnt.node_val_list <- cnt.node_val_list + 1;
      let entries = P.Node.Val.list n in
      let aux = function
        | `Node h -> `Node (of_hash repo h)
        | `Contents (c, m) -> `Contents (Contents.of_hash repo c, m)
      in
      List.fold_left
        (fun acc (k, v) -> StepMap.add k (aux v) acc)
        StepMap.empty entries

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
      | Value (_, v, None), None ->
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
      | Value (r, v, None), _, _, Some v' ->
          if v != v' then t.v <- Value (r, v', None)
      | _ -> ()

    let hash_of_value t v =
      cnt.node_hash <- cnt.node_hash + 1;
      let k = P.Node.Key.hash v in
      t.info.hash <- Some k;
      let () =
        cnt.node_cache_find <- cnt.node_cache_find + 1;
        match Cache.find cache k with
        | i ->
            let old = t.info in
            t.info <- i;
            merge_info ~into:i old;
            hashcons t
        | exception Not_found ->
            cnt.node_cache_miss <- cnt.node_cache_miss + 1;
            t.info.hash <- Some k;
            Cache.add cache k t.info
      in
      match t.info.hash with Some k -> k | None -> k

    let rec to_hash : type a. t -> (hash -> a) -> a =
     fun t k ->
      match hash t with
      | Some h -> k h
      | None -> (
        match t.v with
        | Hash (_, h) -> k h
        | Value (_, v, None) -> k (hash_of_value t v)
        | Value (_, v, Some a) ->
            value_of_adds t v a (fun v -> k (hash_of_value t v))
        | Map m ->
            (value_of_map [@tailcall]) m @@ fun v ->
            t.info.value <- Some v;
            k (hash_of_value t v) )

    and value_of_map : type a. map -> (value -> a) -> a =
     fun map k ->
      let alist = StepMap.bindings map in
      let rec aux acc = function
        | [] ->
            let alist = List.rev acc in
            cnt.node_val_v <- cnt.node_val_v + 1;
            k (P.Node.Val.v alist)
        | (step, v) :: rest -> (
          match v with
          | `Contents (c, m) ->
              let v = `Contents (Contents.to_hash c, m) in
              (aux [@tailcall]) ((step, v) :: acc) rest
          | `Node n ->
              (to_hash [@tailcall]) n @@ fun n ->
              let v = `Node n in
              (aux [@tailcall]) ((step, v) :: acc) rest )
      in
      aux [] alist

    and value_of_elt : type a. elt -> (P.Node.Val.value -> a) -> a =
     fun e k ->
      match e with
      | `Contents (c, m) -> k (`Contents (Contents.to_hash c, m))
      | `Node n -> (to_hash [@tailcall]) n (fun n -> k (`Node n))

    and value_of_adds : type a. t -> value -> map -> (value -> a) -> a =
     fun t v added k ->
      let added = StepMap.bindings added in
      let v =
        List.fold_left
          (fun v (k, e) -> (value_of_elt [@tailcall]) e (P.Node.Val.add v k))
          v added
      in
      t.info.value <- Some v;
      k v

    let to_hash t = to_hash t (fun t -> t)

    let value_of_map t m =
      value_of_map m (fun v ->
          t.info.value <- Some v;
          v )

    let value_of_hash t repo k =
      match t.info.value with
      | Some _ as v -> Lwt.return v
      | None -> (
          cnt.node_find <- cnt.node_find + 1;
          P.Node.find (P.Repo.node_t repo) k >|= function
          | None -> None
          | Some _ as v ->
              t.info.value <- v;
              v )

    let to_value t =
      match value t with
      | Some v -> Lwt.return (Some v)
      | None -> (
        match t.v with
        | Value (_, v, None) -> Lwt.return (Some v)
        | Value (_, v, Some m) ->
            value_of_adds t v m (fun v -> Lwt.return (Some v))
        | Map m -> Lwt.return (Some (value_of_map t m))
        | Hash (repo, h) -> value_of_hash t repo h )

    let to_map t =
      match map t with
      | Some m -> Lwt.return (Some m)
      | None -> (
          let of_value repo v added =
            let m = map_of_value repo v in
            let m =
              match added with
              | None -> m
              | Some added -> StepMap.union (fun _ _ a -> Some a) m added
            in
            t.info.map <- Some m;
            Some m
          in
          match t.v with
          | Map m -> Lwt.return (Some m)
          | Value (repo, v, m) -> Lwt.return (of_value repo v m)
          | Hash (repo, k) -> (
              value_of_hash t repo k >|= function
              | None -> None
              | Some v -> of_value repo v None ) )

    let hash_equal x y = x == y || Type.equal P.Hash.t x y

    let contents_equal ((c1, m1) as x1) ((c2, m2) as x2) =
      x1 == x2 || (Contents.equal c1 c2 && Type.equal Metadata.t m1 m2)

    let rec elt_equal (x : elt) (y : elt) =
      x == y
      ||
      match (x, y) with
      | `Contents x, `Contents y -> contents_equal x y
      | `Node x, `Node y -> equal x y
      | _ -> false

    and map_equal (x : map) (y : map) = StepMap.equal elt_equal x y

    and equal (x : t) (y : t) =
      x == y
      ||
      match (x.v, y.v) with
      | Hash (_, x), Hash (_, y) -> hash_equal x y
      | Value (_, x, None), Value (_, y, None) -> Type.equal P.Node.Val.t x y
      | Map x, Map y -> map_equal x y
      | Value (_, x, Some a), Value (_, y, Some b) ->
          Type.equal P.Node.Val.t x y && map_equal a b
      | _ -> hash_equal (to_hash x) (to_hash y)

    let is_empty t =
      match map t with
      | Some m -> Lwt.return (StepMap.is_empty m)
      | None -> (
          to_value t >|= function
          | None -> false
          | Some n -> P.Node.Val.is_empty n )

    let list t =
      let trim l =
        List.rev_map
          (fun (s, v) ->
            (s, match v with `Contents _ -> `Contents | `Node _ -> `Node) )
          l
        |> List.rev
      in
      match map t with
      | Some m -> Lwt.return (trim (StepMap.bindings m))
      | None -> (
          to_value t >|= function
          | None -> []
          | Some v ->
              cnt.node_val_list <- cnt.node_val_list + 1;
              trim (P.Node.Val.list v) )

    let listv t =
      to_map t >|= function None -> [] | Some m -> StepMap.bindings m

    let findv t step =
      let of_map m =
        match StepMap.find step m with
        | exception Not_found -> Lwt.return None
        | `Node n -> Lwt.return (Some (`Node n))
        | `Contents (c, m) -> (
            Contents.to_value c >|= function
            | None -> None
            | Some c -> Some (`Contents (c, m)) )
      in
      let of_value repo v =
        match P.Node.Val.find v step with
        | None -> Lwt.return None
        | Some (`Contents (c, m)) -> (
            let c = Contents.of_hash repo c in
            Contents.to_value c >|= function
            | None -> None
            | Some c -> Some (`Contents (c, m)) )
        | Some (`Node n) ->
            let n = of_hash repo n in
            Lwt.return (Some (`Node n))
      in
      match map t with
      | Some m -> of_map m
      | None -> (
        match t.v with
        | Map m -> of_map m
        | Value (repo, v, None) -> of_value repo v
        | Value (repo, v, Some m) -> (
            of_map m >>= function
            | Some _ as v -> Lwt.return v
            | None -> of_value repo v )
        | Hash (repo, v) -> (
            value_of_hash t repo v >>= function
            | None -> Lwt.return None
            | Some v -> of_value repo v ) )

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
            let s = List.rev_map fst bindings in
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

    let add t step v =
      let v =
        match v with
        | `Node _ as n -> n
        | `Contents (c, m) -> `Contents (Contents.of_value c, m)
      in
      let of_map m =
        let m' = StepMap.add step v m in
        if m == m' then t else of_map m'
      in
      let of_value repo n added =
        let added' = StepMap.add step v added in
        if added == added' then t else of_value repo n ~added:added'
      in
      match t.v with
      | Map m -> Lwt.return (of_map m)
      | Value (repo, n, None) -> Lwt.return (of_value repo n StepMap.empty)
      | Value (repo, n, Some m) -> Lwt.return (of_value repo n m)
      | Hash (repo, h) ->
          (value_of_hash t repo h >|= function
           | Some v -> v
           | None -> P.Node.Val.empty)
          >|= fun v -> of_value repo v StepMap.empty

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
          StepMap.merge elt_t (fun _step ->
              (merge_elt [@tailcall]) Merge.option )
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
      k (Merge.seq [ Merge.default elt_t; Merge.v elt_t f ])

    let merge_elt = merge_elt (fun x -> x)
  end

  type node = Node.t

  type metadata = Metadata.t

  type tree = [ `Node of node | `Contents of contents * metadata ]

  let of_private_node repo n = Node.of_value repo n

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
    || (c1 == c2 && m1 == m2)
    || (Type.equal P.Contents.Val.t c1 c2 && Type.equal Metadata.t m1 m2)

  let equal (x : tree) (y : tree) =
    x == y
    ||
    match (x, y) with
    | `Node x, `Node y -> Node.equal x y
    | `Contents x, `Contents y -> contents_equal x y
    | `Node _, `Contents _ | `Contents _, `Node _ -> false

  let empty = `Node (Node.of_map StepMap.empty)

  let is_empty = function
    | `Node n -> Node.is_empty n
    | `Contents _ -> Lwt.return false

  let of_node n = `Node n

  let of_contents ?(metadata = Metadata.default) c = `Contents (c, metadata)

  let clear ?depth = function
    | `Node n -> Node.clear ?depth n
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

  let empty_node = function
    | `Node n -> Node.empty n
    | `Contents _ -> Node.of_map StepMap.empty

  let remove t k =
    Log.debug (fun l -> l "Tree.remove %a" pp_path k);
    match Path.rdecons k with
    | None ->
        is_empty t >>= fun is_empty ->
        if is_empty then Lwt.return t else Lwt.return (`Node (empty_node t))
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
        let n = match t with `Node n -> n | _ -> empty_node t in
        (aux [@tailcall]) n path @@ function
        | None -> Lwt.return t
        | Some n -> Lwt.return (`Node n) )

  let add_tree t k v =
    Log.debug (fun l -> l "Tree.add_tree %a" pp_path k);
    let empty_node = empty_node t in
    match Path.rdecons k with
    | None -> Lwt.return v
    | Some (path, file) -> (
        let rec aux n path k =
          let some n = k (Some n) in
          match Path.decons path with
          | None -> (
              Node.findv n file >>= function
              | None -> Node.add n file v >>= some
              | Some old ->
                  if equal old v then k None else Node.add n file v >>= some )
          | Some (h, p) -> (
              Node.findv n h >>= function
              | None | Some (`Contents _) ->
                  (aux [@tailcall]) empty_node p (function
                    | None -> k None
                    | Some child' -> Node.add n h (`Node child') >>= some )
              | Some (`Node child) ->
                  (aux [@tailcall]) child p (function
                    | None -> k None
                    | Some child' -> Node.add n h (`Node child') >>= some ) )
        in
        let n = match t with `Node n -> n | _ -> empty_node in
        (aux [@tailcall]) n path @@ function
        | None -> Lwt.return t
        | Some node -> Lwt.return (`Node node) )

  let add t k ?(metadata = Metadata.default) c =
    Log.debug (fun l -> l "Tree.add %a" pp_path k);
    let empty_node = empty_node t in
    match Path.rdecons k with
    | None -> (
      match t with
      | `Contents c' when contents_equal c' (c, metadata) -> Lwt.return t
      | _ -> Lwt.return (`Contents (c, metadata)) )
    | Some (path, file) -> (
        let rec aux n path k =
          let some n = k (Some n) in
          match Path.decons path with
          | None -> (
              Node.findv n file >>= function
              | Some (`Node _) | None ->
                  Node.add n file (`Contents (c, metadata)) >>= some
              | Some (`Contents _ as old) ->
                  if equal old (`Contents (c, metadata)) then k None
                  else Node.add n file (`Contents (c, metadata)) >>= some )
          | Some (h, p) -> (
              Node.findv n h >>= function
              | None | Some (`Contents _) ->
                  (aux [@tailcall]) empty_node p (function
                    | None -> assert false
                    | Some child -> Node.add n h (`Node child) >>= some )
              | Some (`Node child) ->
                  (aux [@tailcall]) child p (function
                    | None -> k None
                    | Some child' -> Node.add n h (`Node child') >>= some ) )
        in
        let n = match t with `Node n -> n | _ -> empty_node in
        (aux [@tailcall]) n path @@ function
        | None -> Lwt.return t
        | Some n -> Lwt.return (`Node n) )

  let import repo k =
    cnt.node_mem <- cnt.node_mem + 1;
    P.Node.mem (P.Repo.node_t repo) k >|= function
    | true -> Some (Node.of_hash repo k)
    | false -> None

  let import_no_check repo k = Node.of_hash repo k

  let export ?clear repo contents_t node_t n =
    let seen = Hashtbl.create 127 in
    let add_node n v () =
      cnt.node_add <- cnt.node_add + 1;
      P.Node.add node_t v >|= fun k ->
      (*      let k' = Node.to_hash n in
        assert (Type.equal P.Hash.t k k'); *)
      Node.export ?clear repo n k
    in
    let add_contents c x () =
      cnt.contents_add <- cnt.contents_add + 1;
      P.Contents.add contents_t x >|= fun k ->
      (*      let k' = Contents.to_hash c in
        assert (Type.equal P.Hash.t k k'); *)
      Contents.export ?clear repo c k
    in
    let todo = Stack.create () in
    let rec add_to_todo : type a. _ -> (unit -> a Lwt.t) -> a Lwt.t =
     fun n k ->
      let h = Node.to_hash n in
      if Hashtbl.mem seen h then k ()
      else (
        Hashtbl.add seen h ();
        match n.Node.v with
        | Node.Hash _ -> k ()
        | Node.Value (_, x, None) ->
            Stack.push (add_node n x) todo;
            k ()
        | Node.Map _ | Node.Value (_, _, Some _) -> (
            cnt.node_mem <- cnt.node_mem + 1;
            P.Node.mem node_t h >>= function
            | true -> k ()
            | false ->
                (* 1. we push the current node job on the stack. *)
                (Node.to_value n >|= function
                 | None -> assert false
                 | Some v -> Stack.push (add_node n v) todo)
                >>= fun () ->
                let contents = ref [] in
                let nodes = ref [] in
                (Node.to_map n >|= function
                 | None -> assert false
                 | Some x ->
                     StepMap.iter
                       (fun _ -> function
                         | `Contents c -> contents := c :: !contents
                         | `Node n -> nodes := n :: !nodes )
                       x)
                >>= fun () ->
                (* 2. we push the contents job on the stack. *)
                List.iter
                  (fun (c, _) ->
                    let h = Contents.to_hash c in
                    if Hashtbl.mem seen h then ()
                    else (
                      Hashtbl.add seen h ();
                      match c.Contents.v with
                      | Contents.Hash _ -> ()
                      | Contents.Value x -> Stack.push (add_contents c x) todo )
                    )
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
        let empty_node = empty_node (`Node y) in
        diff_node empty_node y >|= fun diff -> (Path.empty, `Removed x) :: diff
    | `Node x, `Contents y ->
        let empty_node = empty_node (`Node x) in
        diff_node x empty_node >|= fun diff -> (Path.empty, `Added y) :: diff

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
    Log.debug (fun l -> l "Tree.hash");
    match t with
    | `Node n -> `Node (Node.to_hash n)
    | `Contents (c, m) ->
        cnt.contents_hash <- cnt.contents_hash + 1;
        `Contents (P.Contents.Key.hash c, m)

  module Cache = struct
    let length () =
      ( `Contents Contents.(Hashes.length hashes),
        `Nodes Node.(Cache.length cache) )

    let clear ?depth () =
      Log.info (fun l -> l "Tree.Cache.clear %a" Fmt.(option int) depth);
      match depth with
      | None ->
          Contents.iter_info Contents.clear_info;
          Node.iter_info (fun i -> Node.clear_info i)
      | Some depth ->
          Contents.iter_info (Contents.mark `White);
          Node.iter_info (Node.mark `White);
          Node.iter_info (fun i -> Node.clear_info ~depth i);
          Contents.iter_info (fun i ->
              if i.Contents.color = `White then Contents.clear_info i )

    let dump ppf () =
      let ppo t ppf = function
        | None -> Fmt.pf ppf "<none>"
        | Some y -> Type.pp t ppf y
      in
      Contents.Hashes.iter
        (fun k v ->
          Fmt.pf ppf "C|%a: %a@." pp_hash k (ppo P.Contents.Val.t)
            v.Contents.value )
        Contents.hashes;
      Node.Cache.iter
        (fun k v -> Fmt.pf ppf "N|%a: %a@." pp_hash k Node.dump_info v)
        Node.cache
  end

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

  let counters () = cnt

  let dump_counters ppf () =
    let `Contents c, `Nodes n = Cache.length () in
    cnt.contents_cache_length <- c;
    cnt.node_cache_length <- n;
    dump_counters ppf cnt

  let reset_counters () = reset_counters cnt

  let inspect = function
    | `Contents _ -> `Contents
    | `Node n ->
        `Node
          ( match n.Node.v with
          | Map _ -> `Map
          | Value _ -> `Value
          | Hash _ -> `Hash )
end
