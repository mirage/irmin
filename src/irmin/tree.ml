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

open! Import
include Tree_intf

let rec drop n (l : 'a Seq.t) () =
  match l () with
  | l' when n = 0 -> l'
  | Nil -> Nil
  | Cons (_, l') -> drop (n - 1) l' ()

let take : type a. int -> a Seq.t -> a list =
  let rec aux acc n (l : a Seq.t) =
    if n = 0 then acc
    else
      match l () with Nil -> acc | Cons (x, l') -> aux (x :: acc) (n - 1) l'
  in
  fun n s -> List.rev (aux [] n s)

let src = Logs.Src.create "irmin.tree" ~doc:"Persistent lazy trees for Irmin"

module Log = (val Logs.src_log src : Logs.LOG)

let option_of_result = function Ok x -> Some x | Error _ -> None

type fuzzy_bool = False | True | Maybe
type ('a, 'r) cont = ('a -> 'r) -> 'r
type ('a, 'r) cont_lwt = ('a, 'r Lwt.t) cont

let ok x = Lwt.return (Ok x)

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
              (aux [@tailcall]) t1 l2)
            else (
              f k2 (`Right v2);
              (aux [@tailcall]) l1 t2))
  in
  aux l1 l2

(* assume l1 and l2 are key-sorted *)
let alist_iter2_lwt compare_k f l1 l2 =
  let l3 = ref [] in
  alist_iter2 compare_k (fun left right -> l3 := f left right :: !l3) l1 l2;
  Lwt_list.iter_s (fun b -> b >>= fun () -> Lwt.return_unit) (List.rev !l3)

module Make (P : Private.S) = struct
  type counters = {
    mutable contents_hash : int;
    mutable contents_find : int;
    mutable contents_add : int;
    mutable node_hash : int;
    mutable node_mem : int;
    mutable node_add : int;
    mutable node_find : int;
    mutable node_val_v : int;
    mutable node_val_find : int;
    mutable node_val_list : int;
  }
  [@@deriving irmin]

  let dump_counters ppf t = Type.pp_json ~minify:false counters_t ppf t

  let fresh_counters () =
    {
      contents_hash = 0;
      contents_add = 0;
      contents_find = 0;
      node_hash = 0;
      node_mem = 0;
      node_add = 0;
      node_find = 0;
      node_val_v = 0;
      node_val_find = 0;
      node_val_list = 0;
    }

  let reset_counters t =
    t.contents_hash <- 0;
    t.contents_add <- 0;
    t.contents_find <- 0;
    t.node_hash <- 0;
    t.node_mem <- 0;
    t.node_add <- 0;
    t.node_find <- 0;
    t.node_val_v <- 0;
    t.node_val_find <- 0;
    t.node_val_list <- 0

  let cnt = fresh_counters ()

  module Path = P.Node.Path

  module StepMap = struct
    module X = struct
      type t = Path.step

      let t = Path.step_t
      let compare = Type.(unstage (compare Path.step_t))
    end

    include Map.Make (X)
    include Merge.Map (X)
  end

  module Metadata = P.Node.Metadata

  type key = Path.t
  type hash = P.Hash.t
  type 'a or_error = ('a, [ `Dangling_hash of hash ]) result

  let get_ok : type a. a or_error -> a = function
    | Ok x -> x
    | Error (`Dangling_hash hash) ->
        Fmt.failwith "Encountered dangling hash %a" (Type.pp P.Hash.t) hash

  type step = Path.step
  type contents = P.Contents.value
  type repo = P.Repo.t

  let pp_hash = Type.pp P.Hash.t
  let pp_path = Type.pp Path.t

  module Hashes = Hashtbl.Make (struct
    type t = hash

    let hash = P.Hash.short_hash
    let equal = Type.(unstage (equal P.Hash.t))
  end)

  let dummy_marks = Hashes.create 0

  type marks = unit Hashes.t

  let empty_marks () = Hashes.create 39

  type 'a force = [ `True | `False of key -> 'a -> 'a Lwt.t | `And_clear ]
  type uniq = [ `False | `True | `Marks of marks ]
  type 'a node_fn = key -> step list -> 'a -> 'a Lwt.t

  type depth = [ `Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int ]
  [@@deriving irmin]

  let equal_contents = Type.(unstage (equal P.Contents.Val.t))
  let equal_metadata = Type.(unstage (equal Metadata.t))
  let equal_hash = Type.(unstage (equal P.Hash.t))
  let equal_node = Type.(unstage (equal P.Node.Val.t))

  module Contents = struct
    type v = Hash of repo * hash | Value of contents
    type info = { mutable hash : hash option; mutable value : contents option }
    type t = { mutable v : v; mutable info : info }

    let info_is_empty i = i.hash = None && i.value = None

    let v =
      let open Type in
      variant "Node.Contents.v" (fun hash value -> function
        | Hash (_, x) -> hash x | Value v -> value v)
      |~ case1 "hash" P.Hash.t (fun _ -> assert false)
      |~ case1 "value" P.Contents.Val.t (fun v -> Value v)
      |> sealv

    let clear_info i =
      if not (info_is_empty i) then (
        i.value <- None;
        i.hash <- None)

    let clear t = clear_info t.info

    let of_v v =
      let hash, value =
        match v with Hash (_, k) -> (Some k, None) | Value v -> (None, Some v)
      in
      let info = { hash; value } in
      { v; info }

    let export ?clear:(c = true) repo t k =
      let hash = t.info.hash in
      if c then clear t;
      match (t.v, hash) with
      | Hash (_, k), _ -> t.v <- Hash (repo, k)
      | Value _, None -> t.v <- Hash (repo, k)
      | Value _, Some k -> t.v <- Hash (repo, k)

    let t = Type.map v of_v (fun t -> t.v)
    let of_value c = of_v (Value c)
    let of_hash repo k = of_v (Hash (repo, k))

    let cached_hash t =
      match (t.v, t.info.hash) with
      | Hash (_, k), None ->
          let h = Some k in
          t.info.hash <- h;
          h
      | _, h -> h

    let cached_value t =
      match (t.v, t.info.value) with
      | Value v, None ->
          let v = Some v in
          t.info.value <- v;
          v
      | _, v -> v

    let hash c =
      match cached_hash c with
      | Some k -> k
      | None -> (
          match cached_value c with
          | None -> assert false
          | Some v ->
              cnt.contents_hash <- cnt.contents_hash + 1;
              let k = P.Contents.Key.hash v in
              c.info.hash <- Some k;
              k)

    let value_of_hash t repo k =
      cnt.contents_find <- cnt.contents_find + 1;
      P.Contents.find (P.Repo.contents_t repo) k >|= function
      | None -> Error (`Dangling_hash k)
      | Some v as some_v ->
          t.info.value <- some_v;
          Ok v

    let to_value t =
      match cached_value t with
      | Some v -> Lwt.return (Ok v)
      | None -> (
          match t.v with
          | Value v -> Lwt.return (Ok v)
          | Hash (repo, k) -> value_of_hash t repo k)

    let force = to_value

    let equal (x : t) (y : t) =
      x == y
      ||
      match (cached_hash x, cached_hash y) with
      | Some x, Some y -> equal_hash x y
      | _ -> (
          match (cached_value x, cached_value y) with
          | Some x, Some y -> equal_contents x y
          | _ -> equal_hash (hash x) (hash y))

    let merge : t Merge.t =
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              let+ c = to_value old >|= option_of_result in
              Ok (Some c))
        in
        let* x = to_value x >|= option_of_result in
        let* y = to_value y >|= option_of_result in
        Merge.(f P.Contents.Val.merge) ~old x y >|= function
        | Ok (Some c) -> Ok (of_value c)
        | Ok None -> Error (`Conflict "empty contents")
        | Error _ as e -> e
      in
      Merge.v t f

    let fold ~force ~path f t acc =
      match force with
      | `True | `And_clear ->
          let* c = to_value t in
          if force = `And_clear then clear t;
          f path (get_ok c) acc
      | `False skip -> (
          match t.info.value with
          | None -> skip path acc
          | Some c -> f path c acc)
  end

  module Node = struct
    type value = P.Node.Val.t

    type elt = [ `Node of t | `Contents of Contents.t * Metadata.t ]

    and map = elt StepMap.t

    and info = {
      mutable value : value option;
      mutable map : map option;
      mutable hash : hash option;
      mutable findv_cache : map option;
    }

    and v =
      | Map of map
      | Hash of repo * hash
      | Value of repo * value * map option

    and t = { mutable v : v; mutable info : info }

    let elt_t t : elt Type.t =
      let open Type in
      variant "Node.value" (fun node contents contents_m -> function
        | `Node x -> node x
        | `Contents (c, m) ->
            if equal_metadata m Metadata.default then contents c
            else contents_m (c, m))
      |~ case1 "Node" t (fun x -> `Node x)
      |~ case1 "Contents" Contents.t (fun x -> `Contents (x, Metadata.default))
      |~ case1 "Contents-x" (pair Contents.t Metadata.t) (fun x -> `Contents x)
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
        | Value (_, v, m) -> value (v, m))
      |~ case1 "map" m (fun m -> Map m)
      |~ case1 "hash" P.Hash.t (fun _ -> assert false)
      |~ case1 "value" (pair P.Node.Val.t (option m)) (fun _ -> assert false)
      |> sealv

    let info_is_empty i =
      i.map = None && i.value = None && i.findv_cache = None && i.hash = None

    let of_v v =
      let hash, map, value =
        match v with
        | Map m -> (None, Some m, None)
        | Hash (_, k) -> (Some k, None, None)
        | Value (_, v, None) -> (None, None, Some v)
        | Value _ -> (None, None, None)
      in
      let findv_cache = None in
      let info = { hash; map; value; findv_cache } in
      { v; info }

    let t node = Type.map node of_v (fun t -> t.v)

    let _, t =
      Type.mu2 (fun _ y ->
          let elt = elt_t y in
          let v = v_t (map_t elt) in
          let t = t v in
          (v, t))

    let elt_t = elt_t t

    let rec clear_elt ~max_depth depth (_, v) =
      match v with
      | `Contents (c, _) -> if depth + 1 > max_depth then Contents.clear c
      | `Node t -> clear ~max_depth (depth + 1) t

    and clear_map ~max_depth depth = List.iter (clear_elt ~max_depth depth)
    and clear_maps ~max_depth depth = List.iter (clear_map ~max_depth depth)

    and clear_info ~max_depth ?v depth i =
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
      let findv =
        match i.findv_cache with Some m -> StepMap.bindings m | None -> []
      in
      if depth >= max_depth && not (info_is_empty i) then (
        i.value <- None;
        i.map <- None;
        i.hash <- None;
        i.findv_cache <- None);
      clear_maps ~max_depth depth [ map; added; findv ]

    and clear ~max_depth depth t = clear_info ~v:t.v ~max_depth depth t.info

    let clear ?depth:d n =
      let max_depth = match d with None -> 0 | Some max_depth -> max_depth in
      clear ~max_depth 0 n

    (* export t to the given repo and clear the cache *)
    let export ?clear:(c = true) repo t k =
      let hash = t.info.hash in
      if c then clear t;
      match t.v with
      | Hash (_, k) -> t.v <- Hash (repo, k)
      | Value (_, v, None) when P.Node.Val.is_empty v -> ()
      | Map m when StepMap.is_empty m ->
          t.v <- Value (repo, P.Node.Val.empty, None)
      | _ -> (
          match hash with
          | None -> t.v <- Hash (repo, k)
          | Some k -> t.v <- Hash (repo, k))

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

    let cached_hash t =
      match (t.v, t.info.hash) with
      | Hash (_, h), None ->
          let h = Some h in
          t.info.hash <- h;
          h
      | _, h -> h

    let cached_map t =
      match (t.v, t.info.map) with
      | Map m, None ->
          let m = Some m in
          t.info.map <- m;
          m
      | _, m -> m

    let cached_value t =
      match (t.v, t.info.value) with
      | Value (_, v, None), None ->
          let v = Some v in
          t.info.value <- v;
          v
      | _, v -> v

    let rec hash : type a. t -> (hash -> a) -> a =
     fun t k ->
      match cached_hash t with
      | Some h -> k h
      | None -> (
          let a_of_value v =
            cnt.node_hash <- cnt.node_hash + 1;
            let h = P.Node.Key.hash v in
            t.info.hash <- Some h;
            k h
          in
          match cached_value t with
          | Some v -> a_of_value v
          | None -> (
              match t.v with
              | Hash (_, h) -> k h
              | Value (_, v, None) -> a_of_value v
              | Value (_, v, Some a) -> value_of_adds t v a a_of_value
              | Map m -> value_of_map t m a_of_value))

    and value_of_map : type r. t -> map -> (value, r) cont =
     fun t map k ->
      if StepMap.is_empty map then (
        t.info.value <- Some P.Node.Val.empty;
        k P.Node.Val.empty)
      else
        let alist = StepMap.bindings map in
        let rec aux acc = function
          | [] ->
              cnt.node_val_v <- cnt.node_val_v + 1;
              let v = P.Node.Val.v (List.rev acc) in
              t.info.value <- Some v;
              k v
          | (step, v) :: rest -> (
              match v with
              | `Contents (c, m) ->
                  let v = `Contents (Contents.hash c, m) in
                  (aux [@tailcall]) ((step, v) :: acc) rest
              | `Node n -> hash n (fun h -> aux ((step, `Node h) :: acc) rest))
        in
        aux [] alist

    and value_of_elt : type r. elt -> (P.Node.Val.value, r) cont =
     fun e k ->
      match e with
      | `Contents (c, m) -> k (`Contents (Contents.hash c, m))
      | `Node n -> hash n (fun h -> k (`Node h))

    and value_of_adds : type r. t -> value -> _ -> (value, r) cont =
     fun t v added k ->
      let added = StepMap.bindings added in
      let rec aux acc = function
        | [] ->
            t.info.value <- Some acc;
            k acc
        | (k, e) :: rest ->
            value_of_elt e (fun e -> aux (P.Node.Val.add acc k e) rest)
      in
      aux v added

    let hash k = hash k (fun x -> x)

    let value_of_hash t repo k =
      match t.info.value with
      | Some v -> Lwt.return_ok v
      | None -> (
          cnt.node_find <- cnt.node_find + 1;
          P.Node.find (P.Repo.node_t repo) k >|= function
          | None -> Error (`Dangling_hash k)
          | Some v as some_v ->
              t.info.value <- some_v;
              Ok v)

    let to_value t =
      match cached_value t with
      | Some v -> ok v
      | None -> (
          match t.v with
          | Value (_, v, None) -> ok v
          | Value (_, v, Some m) -> value_of_adds t v m ok
          | Map m -> value_of_map t m ok
          | Hash (repo, h) -> value_of_hash t repo h)

    let to_map t =
      match cached_map t with
      | Some m -> Lwt.return (Ok m)
      | None -> (
          let of_value repo v added =
            let m = map_of_value repo v in
            let m =
              match added with
              | None -> m
              | Some added -> StepMap.union (fun _ _ a -> Some a) m added
            in
            t.info.map <- Some m;
            m
          in
          match t.v with
          | Map m -> Lwt.return (Ok m)
          | Value (repo, v, m) -> Lwt.return (Ok (of_value repo v m))
          | Hash (repo, k) -> (
              value_of_hash t repo k >|= function
              | Error _ as e -> e
              | Ok v -> Ok (of_value repo v None)))

    let hash_equal x y = x == y || equal_hash x y

    let contents_equal ((c1, m1) as x1) ((c2, m2) as x2) =
      x1 == x2 || (Contents.equal c1 c2 && equal_metadata m1 m2)

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
      match (cached_hash x, cached_hash y) with
      | Some x, Some y -> equal_hash x y
      | _ -> (
          match (cached_value x, cached_value y) with
          | Some x, Some y -> equal_node x y
          | _ -> (
              match (cached_map x, cached_map y) with
              | Some x, Some y -> map_equal x y
              | _ -> hash_equal (hash x) (hash y)))

    (* same as [equal] but do not compare in-memory maps
       recursively. *)
    let maybe_equal (x : t) (y : t) =
      if x == y then True
      else
        match (cached_hash x, cached_hash y) with
        | Some x, Some y -> if equal_hash x y then True else False
        | _ -> (
            match (cached_value x, cached_value y) with
            | Some x, Some y -> if equal_node x y then True else False
            | _ -> Maybe)

    let is_empty t =
      match cached_map t with
      | Some m -> Lwt.return (Ok (StepMap.is_empty m))
      | None -> (
          match t.v with
          | Value (_, _, Some _) -> Lwt.return (Ok false)
          | _ -> (
              to_value t >|= function
              | Error _ as e -> e
              | Ok n -> Ok (P.Node.Val.is_empty n)))

    let add_to_findv_cache t step v =
      match t.info.findv_cache with
      | None -> t.info.findv_cache <- Some (StepMap.singleton step v)
      | Some m -> t.info.findv_cache <- Some (StepMap.add step v m)

    let findv t step =
      let of_map m =
        match StepMap.find step m with
        | exception Not_found -> Lwt.return_none
        | `Node n -> Lwt.return_some (`Node n)
        | `Contents (c, m) -> (
            Contents.to_value c >|= function
            | Error _ -> None
            | Ok c -> Some (`Contents (c, m)))
      in
      let of_value repo v =
        match P.Node.Val.find v step with
        | None -> Lwt.return_none
        | Some (`Contents (c, m)) -> (
            let c = Contents.of_hash repo c in
            let (v : elt) = `Contents (c, m) in
            add_to_findv_cache t step v;
            Contents.to_value c >|= function
            | Error _ -> None
            | Ok c -> Some (`Contents (c, m)))
        | Some (`Node n) ->
            let n = of_hash repo n in
            let v = `Node n in
            add_to_findv_cache t step v;
            Lwt.return_some v
      in
      let of_t () =
        match t.v with
        | Map m -> of_map m
        | Value (repo, v, None) -> of_value repo v
        | Value (repo, v, Some m) -> (
            of_map m >>= function
            | Some _ as v -> Lwt.return v
            | None -> of_value repo v)
        | Hash (repo, h) -> (
            match cached_value t with
            | Some v -> of_value repo v
            | None -> (
                value_of_hash t repo h >>= function
                | Error (`Dangling_hash _) -> Lwt.return_none
                | Ok v -> of_value repo v))
      in
      match cached_map t with
      | Some m -> of_map m
      | None -> (
          match t.info.findv_cache with
          | None -> of_t ()
          | Some m -> (
              of_map m >>= function
              | None -> of_t ()
              | Some _ as r -> Lwt.return r))

    let list_of_map ?(offset = 0) ?length m : (step * elt) list =
      let take_length seq =
        match length with None -> List.of_seq seq | Some n -> take n seq
      in
      StepMap.to_seq m |> drop offset |> take_length

    let list_of_value repo ?offset ?length v : (step * elt) list =
      cnt.node_val_list <- cnt.node_val_list + 1;
      let t = P.Node.Val.list ?offset ?length v in
      List.fold_left
        (fun acc (k, v) ->
          match v with
          | `Node n ->
              let n = `Node (of_hash repo n) in
              (k, n) :: acc
          | `Contents (c, m) ->
              let c = Contents.of_hash repo c in
              (k, `Contents (c, m)) :: acc)
        [] (List.rev t)

    let list ?offset ?length t : (step * elt) list or_error Lwt.t =
      match cached_map t with
      | Some m -> ok (list_of_map ?offset ?length m)
      | None -> (
          match t.v with
          | Value (repo, n, None) -> ok (list_of_value ?offset ?length repo n)
          | Hash (repo, h) -> (
              value_of_hash t repo h >>= function
              | Error _ as e -> Lwt.return e
              | Ok v -> ok (list_of_value ?offset ?length repo v))
          | _ -> (
              to_map t >>= function
              | Error _ as e -> Lwt.return e
              | Ok m -> ok (list_of_map ?offset ?length m)))

    let bindings t =
      to_map t >|= function
      | Error _ as e -> e
      | Ok m -> Ok (StepMap.bindings m)

    type ('v, 'acc, 'r) folder =
      path:key -> 'acc -> int -> 'v -> ('acc, 'r) cont_lwt
    (** A ('val, 'acc, 'r) folder is a CPS, threaded fold function over values
        of type ['v] producing an accumulator of type ['acc]. *)

    let fold :
        type acc.
        force:acc force ->
        uniq:uniq ->
        pre:acc node_fn ->
        post:acc node_fn ->
        path:Path.t ->
        ?depth:depth ->
        node:(key -> _ -> acc -> acc Lwt.t) ->
        contents:(key -> contents -> acc -> acc Lwt.t) ->
        t ->
        acc ->
        acc Lwt.t =
     fun ~force ~uniq ~pre ~post ~path ?depth ~node ~contents t acc ->
      let marks =
        match uniq with
        | `False -> dummy_marks
        | `True -> empty_marks ()
        | `Marks n -> n
      in
      let rec aux : type r. (t, acc, r) folder =
       fun ~path acc d t k ->
        let apply acc = node path t acc in
        let next acc =
          match force with
          | `True | `And_clear -> (
              to_map t >>= function
              | Ok m ->
                  if force = `And_clear then clear ~depth:0 t;
                  (map [@tailcall]) ~path acc d (Some m) k
              | Error (`Dangling_hash _) -> (map [@tailcall]) ~path acc d None k
              )
          | `False skip -> (
              match t.info.map with
              | Some n -> (map [@tailcall]) ~path acc d (Some n) k
              | _ -> skip path acc >>= k)
        in
        match depth with
        | None -> apply acc >>= next
        | Some (`Eq depth) -> if d < depth then next acc else apply acc >>= k
        | Some (`Le depth) ->
            if d < depth then apply acc >>= next else apply acc >>= k
        | Some (`Lt depth) ->
            if d < depth - 1 then apply acc >>= next else apply acc >>= k
        | Some (`Ge depth) -> if d < depth then next acc else apply acc >>= next
        | Some (`Gt depth) ->
            if d <= depth then next acc else apply acc >>= next
      and aux_uniq : type r. (t, acc, r) folder =
       fun ~path acc d t k ->
        if uniq = `False then (aux [@tailcall]) ~path acc d t k
        else
          let h = hash t in
          if Hashes.mem marks h then k acc
          else (
            Hashes.add marks h ();
            (aux [@tailcall]) ~path acc d t k)
      and step : type r. (step * elt, acc, r) folder =
       fun ~path acc d (s, v) k ->
        let path = Path.rcons path s in
        match v with
        | `Node n -> (aux_uniq [@tailcall]) ~path acc (d + 1) n k
        | `Contents c -> (
            let apply () =
              Contents.fold ~force ~path contents (fst c) acc >>= k
            in
            match depth with
            | None -> apply ()
            | Some (`Eq depth) -> if d = depth - 1 then apply () else k acc
            | Some (`Le depth) -> if d < depth then apply () else k acc
            | Some (`Lt depth) -> if d < depth - 1 then apply () else k acc
            | Some (`Ge depth) -> if d >= depth - 1 then apply () else k acc
            | Some (`Gt depth) -> if d >= depth then apply () else k acc)
      and steps : type r. ((step * elt) list, acc, r) folder =
       fun ~path acc d s k ->
        match s with
        | [] -> k acc
        | h :: t ->
            (step [@tailcall]) ~path acc d h @@ fun acc ->
            (steps [@tailcall]) ~path acc d t k
      and map : type r. (map option, acc, r) folder =
       fun ~path acc d m k ->
        match m with
        | None -> k acc
        | Some m ->
            let bindings = StepMap.bindings m in
            let s = List.rev_map fst bindings in
            let* acc = pre path s acc in
            (steps [@tailcall]) ~path acc d bindings @@ fun acc ->
            post path s acc >>= k
      in
      aux_uniq ~path acc 0 t Lwt.return

    let remove t step =
      to_map t >|= function
      | Error (`Dangling_hash _) -> t
      | Ok n ->
          if not (StepMap.mem step n) then t else of_map (StepMap.remove step n)

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
      | Hash (repo, h) -> (
          match (cached_value t, cached_map t) with
          | Some v, _ -> Lwt.return (of_value repo v StepMap.empty)
          | _, Some m -> Lwt.return (of_map m)
          | None, None ->
              let+ v =
                value_of_hash t repo h >|= function
                | Ok v -> v
                | Error (`Dangling_hash _) -> P.Node.Val.empty
              in
              of_value repo v StepMap.empty)

    let rec merge : type a. (t Merge.t -> a) -> a =
     fun k ->
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              let+ m = to_map old >|= option_of_result in
              Ok (Some m))
        in
        let* x = to_map x >|= option_of_result in
        let* y = to_map y >|= option_of_result in
        let m =
          StepMap.merge elt_t (fun _step ->
              (merge_elt [@tailcall]) Merge.option)
        in
        Merge.(f @@ option m) ~old x y >|= function
        | Ok (Some map) -> Ok (of_map map)
        | Ok None -> Error (`Conflict "empty map")
        | Error _ as e -> e
      in
      k (Merge.v t f)

    and merge_elt : type r. (elt Merge.t, r) cont =
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
                  | `Node _ -> Lwt.return (Ok None))
            in
            Merge.(f Metadata.merge) ~old:mold cx cy >>=* fun m ->
            let old =
              Merge.bind_promise old (fun old () ->
                  match old with
                  | `Contents (c, _) -> Lwt.return (Ok (Some c))
                  | `Node _ -> Lwt.return (Ok None))
            in
            Merge.(f Contents.merge) ~old x y >>=* fun c ->
            Merge.ok (`Contents (c, m))
        | `Node x, `Node y ->
            (merge [@tailcall]) (fun m ->
                let old =
                  Merge.bind_promise old (fun old () ->
                      match old with
                      | `Contents _ -> Lwt.return (Ok None)
                      | `Node n -> Lwt.return (Ok (Some n)))
                in
                Merge.(f m ~old x y) >>=* fun n -> Merge.ok (`Node n))
        | _ -> Merge.conflict "add/add values"
      in
      k (Merge.seq [ Merge.default elt_t; Merge.v elt_t f ])

    let merge_elt = merge_elt (fun x -> x)
  end

  type node = Node.t [@@deriving irmin]
  type metadata = Metadata.t

  type t = [ `Node of node | `Contents of P.Contents.Val.t * Metadata.t ]
  [@@deriving irmin { name = "tree_t" }]

  let of_private_node repo n = Node.of_value repo n
  let to_private_node = Node.to_value

  let dump ppf = function
    | `Node n -> Fmt.pf ppf "node: %a" Node.dump n
    | `Contents (c, _) -> Fmt.pf ppf "contents: %a" (Type.pp P.Contents.Val.t) c

  let contents_equal ((c1, m1) as x1) ((c2, m2) as x2) =
    x1 == x2
    || (c1 == c2 && m1 == m2)
    || (equal_contents c1 c2 && equal_metadata m1 m2)

  let equal (x : t) (y : t) =
    x == y
    ||
    match (x, y) with
    | `Node x, `Node y -> Node.equal x y
    | `Contents x, `Contents y -> contents_equal x y
    | `Node _, `Contents _ | `Contents _, `Node _ -> false

  let is_empty = function
    | `Node n -> (
        Node.is_empty n >|= function
        | Ok b -> b
        | Error (`Dangling_hash hash) ->
            Fmt.failwith "is_empty: encountered dangling hash %a"
              (Type.pp P.Hash.t) hash)
    | `Contents _ -> Lwt.return_false

  type elt = [ `Node of node | `Contents of contents * metadata ]

  let of_node n = `Node n
  let of_contents ?(metadata = Metadata.default) c = `Contents (c, metadata)
  let v : elt -> t = function `Contents c -> `Contents c | `Node n -> `Node n

  let destruct : t -> elt = function
    | `Node n -> `Node n
    | `Contents c -> `Contents c

  let clear ?depth = function
    | `Node n -> Node.clear ?depth n
    | `Contents _ -> ()

  let sub t path =
    let rec aux node path =
      match Path.decons path with
      | None -> Lwt.return_some node
      | Some (h, p) -> (
          Node.findv node h >>= function
          | None | Some (`Contents _) -> Lwt.return_none
          | Some (`Node n) -> (aux [@tailcall]) n p)
    in
    match t with
    | `Node n -> (aux [@tailcall]) n path
    | `Contents _ -> Lwt.return_none

  let find_tree (t : t) path =
    Log.debug (fun l -> l "Tree.find_tree %a" pp_path path);
    match (t, Path.rdecons path) with
    | v, None -> Lwt.return_some v
    | _, Some (path, file) -> (
        sub t path >>= function
        | None -> Lwt.return_none
        | Some n -> Node.findv n file)

  let id _ _ acc = Lwt.return acc

  let fold ?(force = `And_clear) ?(uniq = `False) ?(pre = id) ?(post = id)
      ?depth ?(contents = id) ?(node = id) (t : t) acc =
    match t with
    | `Contents v -> contents Path.empty (fst v) acc
    | `Node n ->
        Node.fold ~force ~uniq ~pre ~post ~path:Path.empty ?depth ~contents
          ~node n acc

  type stats = {
    nodes : int;
    leafs : int;
    skips : int;
    depth : int;
    width : int;
  }
  [@@deriving irmin]

  let empty_stats = { nodes = 0; leafs = 0; skips = 0; depth = 0; width = 0 }
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

  let get_tree (t : t) path =
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
    | `Contents _, None -> Lwt.return_some `Contents
    | `Node _, None -> Lwt.return_some `Node
    | _, Some (dir, file) -> (
        sub t dir >>= function
        | None -> Lwt.return_none
        | Some m -> (
            Node.findv m file >>= function
            | None -> Lwt.return_none
            | Some (`Contents _) -> Lwt.return_some `Contents
            | Some (`Node _) -> Lwt.return_some `Node))

  let list t ?offset ?length path : (step * t) list Lwt.t =
    Log.debug (fun l -> l "Tree.list %a" pp_path path);
    sub t path >>= function
    | None -> Lwt.return []
    | Some n -> (
        Node.list ?offset ?length n >>= function
        | Error _ -> Lwt.return []
        | Ok l ->
            Lwt_list.fold_left_s
              (fun acc (k, v) ->
                match v with
                | `Contents (c, m) -> (
                    Contents.to_value c >|= function
                    | Error _ -> acc
                    | Ok c -> (k, `Contents (c, m)) :: acc)
                | `Node n -> Lwt.return ((k, `Node n) :: acc))
              [] (List.rev l))

  let empty = `Node (Node.of_map StepMap.empty)

  let empty_node = function
    | `Node n -> Node.empty n
    | `Contents _ -> Node.of_map StepMap.empty

  (** During recursive updates, we keep track of whether or not we've made a
      modification in order to avoid unnecessary allocations of identical tree
      objects. *)
  type 'a updated = Changed of 'a | Unchanged

  let maybe_equal (x : t) (y : t) =
    if x == y then True
    else
      match (x, y) with
      | `Node x, `Node y -> Node.maybe_equal x y
      | `Contents x, `Contents y -> if contents_equal x y then True else False
      | _ -> False

  let update_tree root_tree path f =
    let empty_node = empty_node root_tree in
    match Path.rdecons path with
    | None -> (
        let* empty_tree =
          is_empty root_tree >|= function
          | true -> root_tree
          | false -> `Node empty_node
        in
        match f (Some root_tree) with
        (* Here we consider "deleting" a root contents value or node to consist
           of changing it to an empty node. Note that this introduces
           sensitivity to ordering of subtree operations: updating in a subtree
           and adding the subtree are not necessarily commutative. *)
        | None -> Lwt.return empty_tree
        | Some new_root -> (
            match maybe_equal root_tree new_root with
            | True -> Lwt.return root_tree
            | Maybe | False -> Lwt.return new_root))
    | Some (path, file) -> (
        let rec aux : type r. key -> node -> (node updated, r) cont_lwt =
         fun path parent_node k ->
          let changed n = k (Changed n) in
          match Path.decons path with
          | None -> (
              let with_new_child t = Node.add parent_node file t >>= changed in
              let* old_binding = Node.findv parent_node file in
              let new_binding = f old_binding in
              match (old_binding, new_binding) with
              | None, None -> k Unchanged
              | None, Some t -> with_new_child t
              | Some _, None -> Node.remove parent_node file >>= changed
              | Some old_value, Some new_value -> (
                  match maybe_equal old_value new_value with
                  | True -> k Unchanged
                  | Maybe | False -> with_new_child new_value))
          | Some (step, key_suffix) -> (
              let* old_binding = Node.findv parent_node step in
              let to_recurse =
                match old_binding with
                | Some (`Node child) -> child
                | None | Some (`Contents _) -> empty_node
              in
              (aux [@tailcall]) key_suffix to_recurse @@ function
              | Unchanged ->
                  (* This includes [remove]s in an empty node, in which case we
                     want to avoid adding a binding anyway. *)
                  k Unchanged
              | Changed child -> (
                  Node.is_empty child >>= function
                  | Ok true ->
                      (* A [remove] has emptied previously non-empty child with
                         binding [h], so we remove the binding. *)
                      Node.remove parent_node step >>= changed
                  | Ok false | Error (`Dangling_hash _) ->
                      Node.add parent_node step (`Node child) >>= changed))
        in
        let top_node =
          match root_tree with `Node n -> n | `Contents _ -> empty_node
        in
        aux path top_node @@ function
        | Unchanged -> Lwt.return root_tree
        | Changed node -> Lwt.return (`Node node))

  let update t k ?(metadata = Metadata.default) f =
    Log.debug (fun l -> l "Tree.update %a" pp_path k);
    update_tree t k (fun t ->
        let old_contents =
          match t with
          | Some (`Node _) | None -> None
          | Some (`Contents (c, _)) -> Some c
        in
        match f old_contents with
        | None -> None
        | Some c -> Some (`Contents (c, metadata)))

  let add t k ?(metadata = Metadata.default) c =
    Log.debug (fun l -> l "Tree.add %a" pp_path k);
    update_tree t k (fun _ -> Some (`Contents (c, metadata)))

  let add_tree t k v =
    Log.debug (fun l -> l "Tree.add_tree %a" pp_path k);
    update_tree t k (fun _ -> Some v)

  let remove t k =
    Log.debug (fun l -> l "Tree.remove %a" pp_path k);
    update_tree t k (fun _ -> None)

  let update_tree t k f =
    Log.debug (fun l -> l "Tree.update_tree %a" pp_path k);
    update_tree t k f

  let import repo k =
    cnt.node_mem <- cnt.node_mem + 1;
    P.Node.mem (P.Repo.node_t repo) k >|= function
    | true -> Some (Node.of_hash repo k)
    | false -> None

  let import_no_check repo k = Node.of_hash repo k
  let value_of_map t map = Node.value_of_map t map (fun x -> x)

  let export ?clear repo contents_t node_t n =
    let seen = Hashes.create 127 in
    let add_node n v () =
      cnt.node_add <- cnt.node_add + 1;
      let+ k = P.Node.add node_t v in
      let k' = Node.hash n in
      assert (equal_hash k k');
      Node.export ?clear repo n k
    in
    let add_contents c x () =
      cnt.contents_add <- cnt.contents_add + 1;
      let+ k = P.Contents.add contents_t x in
      let k' = Contents.hash c in
      assert (equal_hash k k');
      Contents.export ?clear repo c k
    in
    let add_node_map n x () = add_node n (value_of_map n x) () in
    let todo = Stack.create () in
    let rec add_to_todo : type a. _ -> (unit -> a Lwt.t) -> a Lwt.t =
     fun n k ->
      let h = Node.hash n in
      if Hashes.mem seen h then k ()
      else (
        Hashes.add seen h ();
        match n.Node.v with
        | Node.Hash _ ->
            Node.export ?clear repo n h;
            k ()
        | Node.Value (_, x, None) ->
            Stack.push (add_node n x) todo;
            k ()
        | Map _ | Value (_, _, Some _) -> (
            cnt.node_mem <- cnt.node_mem + 1;
            P.Node.mem node_t h >>= function
            | true ->
                Node.export ?clear repo n h;
                k ()
            | false -> (
                match n.v with
                | Hash _ | Value (_, _, None) ->
                    (* might happen if the node has already been added
                       (while the thread was block on P.Node.mem *)
                    k ()
                | Map children | Value (_, _, Some children) ->
                    (* 1. convert partial values to total values *)
                    let* () =
                      match n.v with
                      | Value (_, _, Some _) -> (
                          Node.to_value n >|= function
                          | Error (`Dangling_hash _) -> ()
                          | Ok v -> n.v <- Value (repo, v, None))
                      | _ -> Lwt.return_unit
                    in
                    (* 2. push the current node job on the stack. *)
                    let () =
                      match (n.v, Node.cached_value n) with
                      | _, Some v -> Stack.push (add_node n v) todo
                      | Map x, None -> Stack.push (add_node_map n x) todo
                      | _ -> assert false
                    in
                    let contents = ref [] in
                    let nodes = ref [] in
                    StepMap.iter
                      (fun _ -> function
                        | `Contents c -> contents := c :: !contents
                        | `Node n -> nodes := n :: !nodes)
                      children;

                    (* 2. push the contents job on the stack. *)
                    List.iter
                      (fun (c, _) ->
                        let h = Contents.hash c in
                        if Hashes.mem seen h then ()
                        else (
                          Hashes.add seen h ();
                          match c.Contents.v with
                          | Contents.Hash _ -> ()
                          | Contents.Value x ->
                              Stack.push (add_contents c x) todo))
                      !contents;

                    (* 3. push the children jobs on the stack. *)
                    List.iter
                      (fun n ->
                        Stack.push
                          (fun () -> (add_to_todo [@tailcall]) n Lwt.return)
                          todo)
                      !nodes;
                    k ())))
    in
    let rec loop () =
      let task = try Some (Stack.pop todo) with Stack.Empty -> None in
      match task with None -> Lwt.return_unit | Some t -> t () >>= loop
    in
    (add_to_todo [@tailcall]) n @@ fun () ->
    loop () >|= fun () ->
    let x = Node.hash n in
    Log.debug (fun l -> l "Tree.export -> %a" pp_hash x);
    x

  let merge : t Merge.t =
    let f ~old (x : t) y =
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
          | Error _ -> Merge.conflict "conflict: contents"
          | Ok c -> Merge.ok (`Contents (c, m)))
      | Ok (`Node _ as n) -> Merge.ok n
      | Error e -> Lwt.return (Error e)
    in
    Merge.v tree_t f

  let entries path tree =
    let rec aux acc = function
      | [] -> Lwt.return acc
      | (path, h) :: todo ->
          let* childs = Node.bindings h >|= get_ok in
          let acc, todo =
            List.fold_left
              (fun (acc, todo) (k, v) ->
                let path = Path.rcons path k in
                match v with
                | `Node v -> (acc, (path, v) :: todo)
                | `Contents c -> ((path, c) :: acc, todo))
              (acc, todo) childs
          in
          (aux [@tailcall]) acc todo
    in
    (aux [@tailcall]) [] [ (path, tree) ]

  (** Given two forced lazy values, return an empty diff if they both use the
      same dangling hash. *)
  let diff_force_result (type a b) ~(empty : b) ~(diff_ok : a * a -> b)
      (x : a or_error) (y : a or_error) : b =
    match (x, y) with
    | Error (`Dangling_hash h1), Error (`Dangling_hash h2) -> (
        match equal_hash h1 h2 with true -> empty | false -> assert false)
    | Error _, Ok _ -> assert false
    | Ok _, Error _ -> assert false
    | Ok x, Ok y -> diff_ok (x, y)

  let diff_contents x y =
    if Node.contents_equal x y then Lwt.return_nil
    else
      let* cx = Contents.to_value (fst x) in
      let+ cy = Contents.to_value (fst y) in
      diff_force_result cx cy ~empty:[] ~diff_ok:(fun (cx, cy) ->
          [ `Updated ((cx, snd x), (cy, snd y)) ])

  let compare_step = Type.(unstage (compare Path.step_t))

  let diff_node (x : node) (y : node) =
    let bindings n =
      Node.to_map n >|= function
      | Ok m -> Ok (StepMap.bindings m)
      | Error _ as e -> e
    in
    let removed acc (k, (c, m)) =
      let+ c = Contents.to_value c >|= get_ok in
      (k, `Removed (c, m)) :: acc
    in
    let added acc (k, (c, m)) =
      let+ c = Contents.to_value c >|= get_ok in
      (k, `Added (c, m)) :: acc
    in
    let rec diff_bindings acc todo path x y =
      let acc = ref acc in
      let todo = ref todo in
      let* () =
        alist_iter2_lwt compare_step
          (fun key v ->
            let path = Path.rcons path key in
            match v with
            (* Left *)
            | `Left (`Contents x) ->
                let+ x = removed !acc (path, x) in
                acc := x
            | `Left (`Node x) ->
                let* xs = entries path x in
                let+ xs = Lwt_list.fold_left_s removed !acc xs in
                acc := xs
            (* Right *)
            | `Right (`Contents y) ->
                let+ y = added !acc (path, y) in
                acc := y
            | `Right (`Node y) ->
                let* ys = entries path y in
                let+ ys = Lwt_list.fold_left_s added !acc ys in
                acc := ys
            (* Both *)
            | `Both (`Node x, `Node y) ->
                todo := (path, x, y) :: !todo;
                Lwt.return_unit
            | `Both (`Contents x, `Node y) ->
                let* ys = entries path y in
                let* x = removed !acc (path, x) in
                let+ ys = Lwt_list.fold_left_s added x ys in
                acc := ys
            | `Both (`Node x, `Contents y) ->
                let* xs = entries path x in
                let* y = added !acc (path, y) in
                let+ ys = Lwt_list.fold_left_s removed y xs in
                acc := ys
            | `Both (`Contents x, `Contents y) ->
                let+ content_diffs =
                  diff_contents x y >|= List.map (fun d -> (path, d))
                in
                acc := content_diffs @ !acc)
          x y
      in
      (diff_node [@tailcall]) !acc !todo
    and diff_node acc = function
      | [] -> Lwt.return acc
      | (path, x, y) :: todo ->
          if Node.equal x y then (diff_node [@tailcall]) acc todo
          else
            let* x = bindings x in
            let* y = bindings y in
            diff_force_result ~empty:Lwt.return_nil
              ~diff_ok:(fun (x, y) -> diff_bindings acc todo path x y)
              x y
    in
    (diff_node [@tailcall]) [] [ (Path.empty, x, y) ]

  let diff (x : t) (y : t) =
    match (x, y) with
    | `Contents x, `Contents y ->
        if contents_equal x y then Lwt.return_nil
        else Lwt.return [ (Path.empty, `Updated (y, x)) ]
    | `Node x, `Node y -> diff_node x y
    | `Contents x, `Node y ->
        let empty = Node.empty y in
        let+ diff = diff_node empty y in
        (Path.empty, `Removed x) :: diff
    | `Node x, `Contents y ->
        let empty = Node.empty x in
        let+ diff = diff_node x empty in
        (Path.empty, `Added y) :: diff

  type concrete =
    [ `Tree of (Path.step * concrete) list
    | `Contents of P.Contents.Val.t * Metadata.t ]
  [@@deriving irmin]

  let of_concrete c =
    let rec concrete : type r. concrete -> (t, r) cont =
     fun t k ->
      match t with
      | `Contents _ as v -> k v
      | `Tree childs -> tree StepMap.empty childs (fun n -> k (`Node n))
    and contents : type r. contents * metadata -> (Node.elt, r) cont =
     fun (c, m) k -> k (`Contents (Contents.of_value c, m))
    and tree :
        type r. Node.elt StepMap.t -> (step * concrete) list -> (node, r) cont =
     fun map t k ->
      match t with
      | [] -> k (Node.of_map map)
      | (s, n) :: t ->
          (concrete [@tailcall]) n (function
            | `Contents c ->
                (contents [@tailcall]) c (fun v ->
                    (tree [@tailcall]) (StepMap.add s v map) t k)
            | `Node _ as v -> (tree [@tailcall]) (StepMap.add s v map) t k)
    in
    (concrete [@tailcall]) c (fun x -> x)

  let to_concrete t =
    let rec tree : type r. t -> (concrete, r) cont_lwt =
     fun t k ->
      match t with
      | `Contents _ as v -> k v
      | `Node n ->
          let* m = Node.to_map n in
          let bindings = m |> get_ok |> StepMap.bindings in
          (node [@tailcall]) [] bindings (fun n ->
              let n = List.sort (fun (s, _) (s', _) -> compare_step s s') n in
              k (`Tree n))
    and contents : type r. Contents.t * metadata -> (concrete, r) cont_lwt =
     fun (c, m) k ->
      let* c = Contents.to_value c >|= get_ok in
      k (`Contents (c, m))
    and node :
        type r.
        (step * concrete) list ->
        (step * Node.elt) list ->
        ((step * concrete) list, r) cont_lwt =
     fun childs x k ->
      match x with
      | [] -> k childs
      | (s, n) :: t -> (
          match n with
          | `Node _ as n ->
              (tree [@tailcall]) n (fun tree -> node ((s, tree) :: childs) t k)
          | `Contents c ->
              (contents [@tailcall]) c (fun c ->
                  (node [@tailcall]) ((s, c) :: childs) t k))
    in
    tree t (fun x -> Lwt.return x)

  let hash (t : t) =
    Log.debug (fun l -> l "Tree.hash");
    match t with
    | `Node n -> `Node (Node.hash n)
    | `Contents (c, m) ->
        cnt.contents_hash <- cnt.contents_hash + 1;
        `Contents (P.Contents.Key.hash c, m)

  let stats ?(force = false) (t : t) =
    let force =
      if force then `True
      else `False (fun k s -> set_depth k s |> incr_skips |> Lwt.return)
    in
    let contents k _ s = set_depth k s |> incr_leafs |> Lwt.return in
    let pre k childs s =
      if childs = [] then Lwt.return s
      else set_depth k s |> set_width childs |> incr_nodes |> Lwt.return
    in
    let post _ _ acc = Lwt.return acc in
    fold ~force ~pre ~post ~contents t empty_stats

  let counters () = cnt
  let dump_counters ppf () = dump_counters ppf cnt
  let reset_counters () = reset_counters cnt

  let inspect = function
    | `Contents _ -> `Contents
    | `Node n ->
        `Node
          (match n.Node.v with
          | Map _ -> `Map
          | Value _ -> `Value
          | Hash _ -> `Hash)
end
