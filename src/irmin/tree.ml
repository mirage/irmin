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

let src = Logs.Src.create "irmin.tree" ~doc:"Persistent lazy trees for Irmin"

module Log = (val Logs.src_log src : Logs.LOG)

type fuzzy_bool = False | True | Maybe
type ('a, 'r) cont = ('a -> 'r) -> 'r

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

module Make (P : Private.S) = struct
  module IO_list = IO.List (P.IO)
  module IO = P.IO
  module Merge = P.Merge
  open IO.Syntax

  type ('a, 'r) cont_io = ('a, 'r IO.t) cont

  let ok x = IO.return (Ok x)
  let some x = IO.return (Some x)
  let none = IO.return None

  (* assume l1 and l2 are key-sorted *)
  let alist_iter2_lwt compare_k f l1 l2 =
    let l3 = ref [] in
    alist_iter2 compare_k (fun left right -> l3 := f left right :: !l3) l1 l2;
    IO_list.iter_s Fun.id (List.rev !l3)

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

    let stdlib_merge = merge

    include Merge.Map (X)
  end

  module Metadata = P.Node.Metadata

  type key = Path.t
  type hash = P.Hash.t
  type 'a or_error = ('a, [ `Dangling_hash of hash ]) result

  let get_ok : type a. string -> a or_error -> a =
   fun context -> function
    | Ok x -> x
    | Error (`Dangling_hash hash) ->
        Fmt.failwith "%s: Encountered dangling hash %a" context
          (Type.pp P.Hash.t) hash

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

  type 'a force = [ `True | `False of key -> 'a -> 'a IO.t | `And_clear ]
  type uniq = [ `False | `True | `Marks of marks ]
  type 'a node_fn = key -> step list -> 'a -> 'a IO.t

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
      let+ v_opt = P.Contents.find (P.Repo.contents_t repo) k in
      match v_opt with
      | None -> Error (`Dangling_hash k)
      | Some v ->
          t.info.value <- v_opt;
          Ok v

    let to_value t =
      match cached_value t with
      | Some v -> ok v
      | None -> (
          match t.v with
          | Value v -> ok v
          | Hash (repo, k) -> value_of_hash t repo k)

    let force = to_value

    let force_exn t =
      let+ v = force t in
      get_ok "force" v

    let equal (x : t) (y : t) =
      x == y
      ||
      match (cached_hash x, cached_hash y) with
      | Some x, Some y -> equal_hash x y
      | _ -> (
          match (cached_value x, cached_value y) with
          | Some x, Some y -> equal_contents x y
          | _ -> equal_hash (hash x) (hash y))

    let t = Type.map ~equal:(Type.stage equal) v of_v (fun t -> t.v)
    let merge_val = P.Contents.Val.merge ()

    let to_value_opt x =
      let+ v = to_value x in
      Option.of_result v

    let merge : unit -> t Merge.t =
     fun () ->
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              let+ c = to_value_opt old in
              Ok (Some c))
        in
        let* x = to_value_opt x in
        let* y = to_value_opt y in
        let+ c = Merge.(f merge_val) ~old x y in
        match c with
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
          f path (get_ok "fold" c) acc
      | `False skip -> (
          match t.info.value with
          | None -> skip path acc
          | Some c -> f path c acc)
  end

  module Node = struct
    type value = P.Node.Val.t

    type elt = [ `Node of t | `Contents of Contents.t * Metadata.t ]

    and update = Add of elt | Remove

    and updatemap = update StepMap.t

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
      | Value of repo * value * updatemap option

    and t = { mutable v : v; mutable info : info }
    (** [t.v] has 3 possible states:

        - A [Map], only after a [Tree.of_concrete] operation.
        - A [Value], only after an add, a remove, temporarily during an export
          or at the end of a merge.
        - It is otherwise a [Hash].

        [t.info.map] is only populated during a call to [Node.to_map]. *)

    let elt_t (t : t Type.t) : elt Type.t =
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

    let stepmap_t : 'a. 'a Type.t -> 'a StepMap.t Type.t =
     fun elt ->
      let open Type in
      let to_map x =
        List.fold_left (fun acc (k, v) -> StepMap.add k v acc) StepMap.empty x
      in
      let of_map m = StepMap.fold (fun k v acc -> (k, v) :: acc) m [] in
      map (list (pair Path.step_t elt)) to_map of_map

    let update_t (elt : elt Type.t) : update Type.t =
      let open Type in
      variant "Node.update" (fun add remove -> function
        | Add elt -> add elt | Remove -> remove)
      |~ case1 "add" elt (fun elt -> Add elt)
      |~ case0 "remove" Remove
      |> sealv

    let v_t (elt : elt Type.t) : v Type.t =
      let m = stepmap_t elt in
      let um = stepmap_t (update_t elt) in
      let open Type in
      variant "Node.node" (fun map hash value -> function
        | Map m -> map m
        | Hash (_, y) -> hash y
        | Value (_, v, m) -> value (v, m))
      |~ case1 "map" m (fun m -> Map m)
      |~ case1 "hash" P.Hash.t (fun _ -> assert false)
      |~ case1 "value" (pair P.Node.Val.t (option um)) (fun _ -> assert false)
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
          let v = v_t elt in
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
        | Some (Value (_, _, Some um)) ->
            StepMap.bindings um
            |> List.filter_map (function
                 | _, Remove -> None
                 | k, Add v -> Some (k, v))
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
    let of_value ?updates repo v = of_v (Value (repo, v, updates))

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
              | Value (_, v, Some um) -> value_of_updates t v um a_of_value
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

    and value_of_updates : type r. t -> value -> _ -> (value, r) cont =
     fun t v updates k ->
      let updates = StepMap.bindings updates in
      let rec aux acc = function
        | [] ->
            t.info.value <- Some acc;
            k acc
        | (k, Add e) :: rest ->
            value_of_elt e (fun e -> aux (P.Node.Val.add acc k e) rest)
        | (k, Remove) :: rest -> aux (P.Node.Val.remove acc k) rest
      in
      aux v updates

    let hash k = hash k (fun x -> x)

    let value_of_hash t repo k =
      match t.info.value with
      | Some v -> ok v
      | None -> (
          cnt.node_find <- cnt.node_find + 1;
          let+ v_opt = P.Node.find (P.Repo.node_t repo) k in
          match v_opt with
          | None -> Error (`Dangling_hash k)
          | Some v ->
              t.info.value <- v_opt;
              Ok v)

    let to_value t =
      match cached_value t with
      | Some v -> ok v
      | None -> (
          match t.v with
          | Value (_, v, None) -> ok v
          | Value (_, v, Some um) -> value_of_updates t v um ok
          | Map m -> value_of_map t m ok
          | Hash (repo, h) -> value_of_hash t repo h)

    let to_map t =
      match cached_map t with
      | Some m -> ok m
      | None -> (
          let of_value repo v updates =
            let m = map_of_value repo v in
            let m =
              match updates with
              | None -> m
              | Some updates ->
                  StepMap.stdlib_merge
                    (fun _ left right ->
                      match (left, right) with
                      | None, None -> assert false
                      | (Some _ as v), None -> v
                      | _, Some (Add v) -> Some v
                      | _, Some Remove -> None)
                    m updates
            in
            t.info.map <- Some m;
            m
          in
          match t.v with
          | Map m -> ok m
          | Value (repo, v, m) -> ok (of_value repo v m)
          | Hash (repo, k) -> (
              let+ v = value_of_hash t repo k in
              match v with
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

    (** Does [um] empties [v]?

        Gotcha: Some [Remove] entries in [um] might not be in [v]. *)
    let is_empty_after_updates v um =
      let any_add =
        StepMap.to_seq um
        |> Seq.exists (function _, Remove -> false | _, Add _ -> true)
      in
      if any_add then false
      else
        let val_is_empty = P.Node.Val.is_empty v in
        if val_is_empty then true
        else
          let remove_count = StepMap.cardinal um in
          if (not val_is_empty) && remove_count = 0 then false
          else if P.Node.Val.length v > remove_count then false
          else (
            (* Starting from this point the function is expensive, but there is
               no alternative. *)
            cnt.node_val_list <- cnt.node_val_list + 1;
            let entries = P.Node.Val.list v in
            List.for_all (fun (step, _) -> StepMap.mem step um) entries)

    let length t =
      match cached_map t with
      | Some m -> IO.return (StepMap.cardinal m)
      | None ->
          let+ v = to_value t in
          get_ok "length" v |> P.Node.Val.length

    let is_empty t =
      match cached_map t with
      | Some m -> ok (StepMap.is_empty m)
      | None -> (
          match t.v with
          | Value (_, v, Some um) -> ok (is_empty_after_updates v um)
          | _ -> (
              let+ n = to_value t in
              match n with
              | Error _ as e -> e
              | Ok n -> Ok (P.Node.Val.is_empty n)))

    let add_to_findv_cache t step v =
      match t.info.findv_cache with
      | None -> t.info.findv_cache <- Some (StepMap.singleton step v)
      | Some m -> t.info.findv_cache <- Some (StepMap.add step v m)

    let findv t step =
      let of_map m = try Some (StepMap.find step m) with Not_found -> None in
      let of_value repo v =
        match P.Node.Val.find v step with
        | None -> None
        | Some (`Contents (c, m)) ->
            let c = Contents.of_hash repo c in
            let (v : elt) = `Contents (c, m) in
            add_to_findv_cache t step v;
            Some v
        | Some (`Node n) ->
            let n = of_hash repo n in
            let v = `Node n in
            add_to_findv_cache t step v;
            Some v
      in
      let of_t () =
        match t.v with
        | Map m -> IO.return (of_map m)
        | Value (repo, v, None) -> IO.return (of_value repo v)
        | Value (repo, v, Some um) -> (
            match StepMap.find_opt step um with
            | Some (Add v) -> some v
            | Some Remove -> none
            | None -> IO.return (of_value repo v))
        | Hash (repo, h) -> (
            match cached_value t with
            | Some v -> IO.return (of_value repo v)
            | None -> (
                let+ v = value_of_hash t repo h in
                match v with
                | Error (`Dangling_hash _) -> None
                | Ok v -> of_value repo v))
      in
      match cached_map t with
      | Some m -> IO.return (of_map m)
      | None -> (
          match t.info.findv_cache with
          | None -> of_t ()
          | Some m -> (
              match of_map m with None -> of_t () | Some _ as r -> IO.return r))

    let list_of_map ?(offset = 0) ?length m : (step * elt) list =
      let take_length seq =
        match length with None -> List.of_seq seq | Some n -> Seq.take n seq
      in
      StepMap.to_seq m |> Seq.drop offset |> take_length

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

    let list ?offset ?length t : (step * elt) list or_error IO.t =
      match cached_map t with
      | Some m -> ok (list_of_map ?offset ?length m)
      | None -> (
          match t.v with
          | Value (repo, n, None) -> ok (list_of_value ?offset ?length repo n)
          | Hash (repo, h) -> (
              let* v = value_of_hash t repo h in
              match v with
              | Error _ as e -> IO.return e
              | Ok v -> ok (list_of_value ?offset ?length repo v))
          | _ -> (
              let* m = to_map t in
              match m with
              | Error _ as e -> IO.return e
              | Ok m -> ok (list_of_map ?offset ?length m)))

    let bindings t =
      let+ m = to_map t in
      match m with Error _ as e -> e | Ok m -> Ok (StepMap.bindings m)

    type ('v, 'acc, 'r) folder =
      path:key -> 'acc -> int -> 'v -> ('acc, 'r) cont_io
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
        node:(key -> _ -> acc -> acc IO.t) ->
        contents:(key -> contents -> acc -> acc IO.t) ->
        t ->
        acc ->
        acc IO.t =
     fun ~force ~uniq ~pre ~post ~path ?depth ~node ~contents t acc ->
      let marks =
        match uniq with
        | `False -> dummy_marks
        | `True -> empty_marks ()
        | `Marks n -> n
      in
      let rec aux : type r. (t, acc, r) folder =
       fun ~path acc d t k ->
        let apply acc next =
          let* acc = node path t acc in
          next acc
        in
        let next acc =
          match force with
          | `True | `And_clear -> (
              let* m = to_map t in
              match m with
              | Ok m ->
                  if force = `And_clear then clear ~depth:0 t;
                  (map [@tailcall]) ~path acc d (Some m) k
              | Error (`Dangling_hash _) -> (map [@tailcall]) ~path acc d None k
              )
          | `False skip -> (
              match t.info.map with
              | Some n -> (map [@tailcall]) ~path acc d (Some n) k
              | _ ->
                  let* acc = skip path acc in
                  k acc)
        in
        match depth with
        | None -> apply acc next
        | Some (`Eq depth) -> if d < depth then next acc else apply acc k
        | Some (`Le depth) -> if d < depth then apply acc next else apply acc k
        | Some (`Lt depth) ->
            if d < depth - 1 then apply acc next else apply acc k
        | Some (`Ge depth) -> if d < depth then next acc else apply acc next
        | Some (`Gt depth) -> if d <= depth then next acc else apply acc next
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
              let* acc = Contents.fold ~force ~path contents (fst c) acc in
              k acc
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
            let* x = post path s acc in
            k x
      in
      aux_uniq ~path acc 0 t IO.return

    let update t step up =
      let of_map m =
        let m' =
          match up with
          | Remove -> StepMap.remove step m
          | Add v -> StepMap.add step v m
        in
        if m == m' then t else of_map m'
      in
      let of_value repo n updates =
        let updates' = StepMap.add step up updates in
        if updates == updates' then t else of_value repo n ~updates:updates'
      in
      match t.v with
      | Map m -> IO.return (of_map m)
      | Value (repo, n, None) -> IO.return (of_value repo n StepMap.empty)
      | Value (repo, n, Some um) -> IO.return (of_value repo n um)
      | Hash (repo, h) -> (
          match (cached_value t, cached_map t) with
          | Some v, _ -> IO.return (of_value repo v StepMap.empty)
          | _, Some m -> IO.return (of_map m)
          | None, None ->
              let+ v =
                let+ v = value_of_hash t repo h in
                match v with
                | Ok v -> v
                | Error (`Dangling_hash _) -> P.Node.Val.empty
              in
              of_value repo v StepMap.empty)

    let remove t step = update t step Remove
    let add t step v = update t step (Add v)
    let merge_metadata = Metadata.merge ()
    let merge_contents = Contents.merge ()

    let to_map_opt x =
      let+ v = to_map x in
      Option.of_result v

    let rec merge : type a. (t Merge.t -> a) -> a =
     fun k ->
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              let+ m = to_map_opt old in
              Ok (Some m))
        in
        let* x = to_map_opt x in
        let* y = to_map_opt y in
        let m =
          StepMap.merge elt_t (fun _step ->
              (merge_elt [@tailcall]) Merge.option)
        in
        let+ map = Merge.(f @@ option m) ~old x y in
        match map with
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
                  | `Contents (_, m) -> ok (Some m)
                  | `Node _ -> ok None)
            in
            Merge.f merge_metadata ~old:mold cx cy >>=* fun m ->
            let old =
              Merge.bind_promise old (fun old () ->
                  match old with
                  | `Contents (c, _) -> ok (Some c)
                  | `Node _ -> ok None)
            in
            Merge.f merge_contents ~old x y >>=* fun c ->
            Merge.ok (`Contents (c, m))
        | `Node x, `Node y ->
            (merge [@tailcall]) (fun m ->
                let old =
                  Merge.bind_promise old (fun old () ->
                      match old with
                      | `Contents _ -> ok None
                      | `Node n -> ok (Some n))
                in
                Merge.(f m ~old x y) >>=* fun n -> Merge.ok (`Node n))
        | _ -> Merge.conflict "add/add values"
      in
      k (Merge.seq [ Merge.default elt_t; Merge.v elt_t f ])

    let merge_elt = merge_elt (fun x -> x)
  end

  type node = Node.t [@@deriving irmin]
  type metadata = Metadata.t

  type t = [ `Node of node | `Contents of Contents.t * Metadata.t ]
  [@@deriving irmin { name = "tree_t" }]

  let of_private_node repo n = Node.of_value repo n
  let to_private_node = Node.to_value

  let dump ppf = function
    | `Node n -> Fmt.pf ppf "node: %a" Node.dump n
    | `Contents (c, _) -> Fmt.pf ppf "contents: %a" (Type.pp Contents.t) c

  let contents_equal ((c1, m1) as x1) ((c2, m2) as x2) =
    x1 == x2
    || (c1 == c2 && m1 == m2)
    || (Contents.equal c1 c2 && equal_metadata m1 m2)

  let equal (x : t) (y : t) =
    x == y
    ||
    match (x, y) with
    | `Node x, `Node y -> Node.equal x y
    | `Contents x, `Contents y -> contents_equal x y
    | `Node _, `Contents _ | `Contents _, `Node _ -> false

  let is_empty = function
    | `Node n ->
        let+ b = Node.is_empty n in
        get_ok "is_empty" b
    | `Contents _ -> IO.return false

  type elt = [ `Node of node | `Contents of contents * metadata ]

  let of_node n = `Node n

  let of_contents ?(metadata = Metadata.default) c =
    `Contents (Contents.of_value c, metadata)

  let v : elt -> t = function
    | `Contents (c, meta) -> `Contents (Contents.of_value c, meta)
    | `Node n -> `Node n

  let destruct x = x

  let clear ?depth = function
    | `Node n -> Node.clear ?depth n
    | `Contents _ -> ()

  let sub t path =
    let rec aux node path =
      match Path.decons path with
      | None -> some node
      | Some (h, p) -> (
          let* n = Node.findv node h in
          match n with
          | None | Some (`Contents _) -> none
          | Some (`Node n) -> (aux [@tailcall]) n p)
    in
    match t with `Node n -> (aux [@tailcall]) n path | `Contents _ -> none

  let find_tree (t : t) path =
    Log.debug (fun l -> l "Tree.find_tree %a" pp_path path);
    match (t, Path.rdecons path) with
    | v, None -> some v
    | _, Some (path, file) -> (
        let* n = sub t path in
        match n with None -> none | Some n -> Node.findv n file)

  let id _ _ acc = IO.return acc

  let fold ?(force = `And_clear) ?(uniq = `False) ?(pre = id) ?(post = id)
      ?depth ?(contents = id) ?(node = id) (t : t) acc =
    match t with
    | `Contents (c, _) -> Contents.fold ~force ~path:Path.empty contents c acc
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
    let+ tree = find_tree t path in
    match tree with None -> err_not_found "get_tree" path | Some v -> v

  let find_all t k =
    let* tree = find_tree t k in
    match tree with
    | None | Some (`Node _) -> none
    | Some (`Contents (c, m)) ->
        let+ c = Contents.to_value c in
        Some (get_ok "find_all" c, m)

  let find t k =
    let+ v = find_all t k in
    match v with None -> None | Some (c, _) -> Some c

  let get_all t k =
    let* v = find_all t k in
    match v with None -> err_not_found "get" k | Some v -> IO.return v

  let get t k =
    let+ c, _ = get_all t k in
    c

  let mem t k =
    let+ v = find t k in
    match v with None -> false | _ -> true

  let mem_tree t k =
    let+ v = find_tree t k in
    match v with None -> false | _ -> true

  let kind t path =
    Log.debug (fun l -> l "Tree.kind %a" pp_path path);
    match (t, Path.rdecons path) with
    | `Contents _, None -> some `Contents
    | `Node _, None -> some `Node
    | _, Some (dir, file) -> (
        let* tree = sub t dir in
        match tree with
        | None -> none
        | Some m -> (
            let* v = Node.findv m file in
            match v with
            | None -> none
            | Some (`Contents _) -> some `Contents
            | Some (`Node _) -> some `Node))

  let length = Node.length

  let list t ?offset ?length path : (step * t) list IO.t =
    Log.debug (fun l -> l "Tree.list %a" pp_path path);
    let* tree = sub t path in
    match tree with
    | None -> IO.return []
    | Some n -> (
        let+ ls = Node.list ?offset ?length n in
        match ls with Error _ -> [] | Ok l -> l)

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
          let+ is_emtpy = is_empty root_tree in
          if is_emtpy then root_tree else `Node empty_node
        in
        let* tr = f (Some root_tree) in
        (* Here we consider "deleting" a root contents value or node to consist
           of changing it to an empty node. Note that this introduces
           sensitivity to ordering of subtree operations: updating in a subtree
           and adding the subtree are not necessarily commutative. *)
        match tr with
        | None -> IO.return empty_tree
        | Some new_root -> (
            match maybe_equal root_tree new_root with
            | True -> IO.return root_tree
            | Maybe | False -> IO.return new_root))
    | Some (path, file) -> (
        let rec aux : type r. key -> node -> (node updated, r) cont_io =
         fun path parent_node k ->
          let changed n = k (Changed n) in
          match Path.decons path with
          | None -> (
              let with_new_child t =
                let* n = Node.add parent_node file t in
                changed n
              in
              let* old_binding = Node.findv parent_node file in
              let* new_binding = f old_binding in
              match (old_binding, new_binding) with
              | None, None -> k Unchanged
              | None, Some t -> with_new_child t
              | Some _, None ->
                  let* n = Node.remove parent_node file in
                  changed n
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
                  let* is_empty = Node.is_empty child in
                  match is_empty with
                  | Ok true ->
                      (* A [remove] has emptied previously non-empty child with
                         binding [h], so we remove the binding. *)
                      let* n = Node.remove parent_node step in
                      changed n
                  | Ok false | Error (`Dangling_hash _) ->
                      let* n = Node.add parent_node step (`Node child) in
                      changed n))
        in
        let top_node =
          match root_tree with `Node n -> n | `Contents _ -> empty_node
        in
        aux path top_node @@ function
        | Unchanged -> IO.return root_tree
        | Changed node -> IO.return (`Node node))

  let update t k ?(metadata = Metadata.default) f =
    Log.debug (fun l -> l "Tree.update %a" pp_path k);
    update_tree t k (fun t ->
        let+ old_contents =
          match t with
          | Some (`Node _) | None -> IO.return None
          | Some (`Contents (c, _)) ->
              let+ c = Contents.to_value c in
              Some (get_ok "update" c)
        in
        match f old_contents with
        | None -> None
        | Some c -> Some (`Contents (Contents.of_value c, metadata)))

  let add t k ?(metadata = Metadata.default) c =
    Log.debug (fun l -> l "Tree.add %a" pp_path k);
    update_tree t k (fun _ ->
        IO.return (Some (`Contents (Contents.of_value c, metadata))))

  let add_tree t k v =
    Log.debug (fun l -> l "Tree.add_tree %a" pp_path k);
    update_tree t k (fun _ -> IO.return (Some v))

  let remove t k =
    Log.debug (fun l -> l "Tree.remove %a" pp_path k);
    update_tree t k (fun _ -> IO.return None)

  let wrap1 f x1 = try IO.return (f x1) with exn -> IO.fail exn

  let update_tree t k f =
    Log.debug (fun l -> l "Tree.update_tree %a" pp_path k);
    update_tree t k (wrap1 f)

  let import repo = function
    | `Contents (k, m) ->
        let+ mem = P.Contents.mem (P.Repo.contents_t repo) k in
        if mem then Some (`Contents (Contents.of_hash repo k, m)) else None
    | `Node k ->
        cnt.node_mem <- cnt.node_mem + 1;
        let+ mem = P.Node.mem (P.Repo.node_t repo) k in
        if mem then Some (`Node (Node.of_hash repo k)) else None

  let import_no_check repo = function
    | `Node k -> `Node (Node.of_hash repo k)
    | `Contents (k, m) -> `Contents (Contents.of_hash repo k, m)

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
    let rec add_to_todo : type a. _ -> (unit -> a IO.t) -> a IO.t =
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
            let* mem = P.Node.mem node_t h in
            if mem then (
              Node.export ?clear repo n h;
              k ())
            else
              match n.v with
              | Hash _ | Value (_, _, None) ->
                  (* might happen if the node has already been added
                     (while the thread was block on P.Node.mem *)
                  k ()
              | Map children ->
                  let l = StepMap.bindings children |> List.map snd in
                  add_steps_to_todo l n k
              | Value (_, _, Some children) ->
                  let l =
                    StepMap.bindings children
                    |> List.filter_map (function
                         | _, Node.Remove -> None
                         | _, Node.Add v -> Some v)
                  in
                  add_steps_to_todo l n k))
    and add_steps_to_todo : type a. _ -> _ -> (unit -> a IO.t) -> a IO.t =
     fun l n k ->
      (* 1. convert partial values to total values *)
      let* () =
        match n.Node.v with
        | Value (_, _, Some _) -> (
            let+ v = Node.to_value n in
            match v with
            | Error (`Dangling_hash _) -> ()
            | Ok v -> n.v <- Value (repo, v, None))
        | _ -> IO.return ()
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
      List.iter
        (function
          | `Contents c -> contents := c :: !contents
          | `Node n -> nodes := n :: !nodes)
        l;

      (* 2. push the contents job on the stack. *)
      List.iter
        (fun (c, _) ->
          let h = Contents.hash c in
          if Hashes.mem seen h then ()
          else (
            Hashes.add seen h ();
            match c.Contents.v with
            | Contents.Hash _ -> ()
            | Contents.Value x -> Stack.push (add_contents c x) todo))
        !contents;

      (* 3. push the children jobs on the stack. *)
      List.iter
        (fun n ->
          Stack.push (fun () -> (add_to_todo [@tailcall]) n IO.return) todo)
        !nodes;
      k ()
    in

    let rec loop () =
      let task = try Some (Stack.pop todo) with Stack.Empty -> None in
      match task with
      | None -> IO.return ()
      | Some t ->
          let* () = t () in
          loop ()
    in
    (add_to_todo [@tailcall]) n @@ fun () ->
    let+ () = loop () in
    let x = Node.hash n in
    Log.debug (fun l -> l "Tree.export -> %a" pp_hash x);
    x

  let merge : unit -> t Merge.t =
   fun () ->
    let f ~old (x : t) y =
      let* m = Merge.(f Node.merge_elt) ~old x y in
      match m with Ok t -> Merge.ok t | Error e -> IO.return (Error e)
    in
    Merge.v tree_t f

  let entries path tree =
    let rec aux acc = function
      | [] -> IO.return acc
      | (path, h) :: todo ->
          let* childs =
            let+ es = Node.bindings h in
            get_ok "entries" es
          in
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
    if Node.contents_equal x y then IO.return []
    else
      let* cx = Contents.to_value (fst x) in
      let+ cy = Contents.to_value (fst y) in
      diff_force_result cx cy ~empty:[] ~diff_ok:(fun (cx, cy) ->
          [ `Updated ((cx, snd x), (cy, snd y)) ])

  let compare_step = Type.(unstage (compare Path.step_t))

  let diff_node (x : node) (y : node) =
    let bindings n =
      let+ m = Node.to_map n in
      match m with Ok m -> Ok (StepMap.bindings m) | Error _ as e -> e
    in
    let removed acc (k, (c, m)) =
      let+ c = Contents.to_value c in
      let c = get_ok "diff_node" c in
      (k, `Removed (c, m)) :: acc
    in
    let added acc (k, (c, m)) =
      let+ c = Contents.to_value c in
      let c = get_ok "diff_node" c in
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
                let+ xs = IO_list.fold_left_s removed !acc xs in
                acc := xs
            (* Right *)
            | `Right (`Contents y) ->
                let+ y = added !acc (path, y) in
                acc := y
            | `Right (`Node y) ->
                let* ys = entries path y in
                let+ ys = IO_list.fold_left_s added !acc ys in
                acc := ys
            (* Both *)
            | `Both (`Node x, `Node y) ->
                todo := (path, x, y) :: !todo;
                IO.return ()
            | `Both (`Contents x, `Node y) ->
                let* ys = entries path y in
                let* x = removed !acc (path, x) in
                let+ ys = IO_list.fold_left_s added x ys in
                acc := ys
            | `Both (`Node x, `Contents y) ->
                let* xs = entries path x in
                let* y = added !acc (path, y) in
                let+ ys = IO_list.fold_left_s removed y xs in
                acc := ys
            | `Both (`Contents x, `Contents y) ->
                let+ ds = diff_contents x y in
                let ds = List.map (fun d -> (path, d)) ds in
                acc := ds @ !acc)
          x y
      in
      (diff_node [@tailcall]) !acc !todo
    and diff_node acc = function
      | [] -> IO.return acc
      | (path, x, y) :: todo ->
          if Node.equal x y then (diff_node [@tailcall]) acc todo
          else
            let* x = bindings x in
            let* y = bindings y in
            diff_force_result ~empty:(IO.return [])
              ~diff_ok:(fun (x, y) -> diff_bindings acc todo path x y)
              x y
    in
    (diff_node [@tailcall]) [] [ (Path.empty, x, y) ]

  let contents_value_exn msg c =
    let+ v = Contents.to_value c in
    get_ok msg v

  let diff (x : t) (y : t) =
    match (x, y) with
    | `Contents ((c1, m1) as x), `Contents ((c2, m2) as y) ->
        if contents_equal x y then IO.return []
        else
          let* c1 = contents_value_exn "diff" c1 in
          let* c2 = contents_value_exn "diff" c2 in
          IO.return [ (Path.empty, `Updated ((c1, m1), (c2, m2))) ]
    | `Node x, `Node y -> diff_node x y
    | `Contents (x, m), `Node y ->
        let empty = Node.empty y in
        let* diff = diff_node empty y in
        let+ x = contents_value_exn "diff" x in
        (Path.empty, `Removed (x, m)) :: diff
    | `Node x, `Contents (y, m) ->
        let empty = Node.empty x in
        let* diff = diff_node x empty in
        let+ y = contents_value_exn "diff" y in
        (Path.empty, `Added (y, m)) :: diff

  type concrete =
    [ `Tree of (Path.step * concrete) list
    | `Contents of P.Contents.Val.t * Metadata.t ]
  [@@deriving irmin]

  let of_concrete c =
    let rec concrete : type r. concrete -> (t, r) cont =
     fun t k ->
      match t with
      | `Contents (c, m) -> k (`Contents (Contents.of_value c, m))
      | `Tree childs -> tree StepMap.empty childs (fun n -> k (`Node n))
    and tree :
        type r. Node.elt StepMap.t -> (step * concrete) list -> (node, r) cont =
     fun map t k ->
      match t with
      | [] -> k (Node.of_map map)
      | (s, n) :: t ->
          (concrete [@tailcall]) n (fun v ->
              (tree [@tailcall]) (StepMap.add s v map) t k)
    in
    (concrete [@tailcall]) c (fun x -> x)

  let to_concrete t =
    let rec tree : type r. t -> (concrete, r) cont_io =
     fun t k ->
      match t with
      | `Contents c -> contents c k
      | `Node n ->
          let* m = Node.to_map n in
          let bindings = m |> get_ok "to_concrete" |> StepMap.bindings in
          (node [@tailcall]) [] bindings (fun n ->
              let n = List.sort (fun (s, _) (s', _) -> compare_step s s') n in
              k (`Tree n))
    and contents : type r. Contents.t * metadata -> (concrete, r) cont_io =
     fun (c, m) k ->
      let* c = contents_value_exn "to_concrete" c in
      k (`Contents (c, m))
    and node :
        type r.
        (step * concrete) list ->
        (step * Node.elt) list ->
        ((step * concrete) list, r) cont_io =
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
    tree t (fun x -> IO.return x)

  let hash (t : t) =
    Log.debug (fun l -> l "Tree.hash");
    match t with
    | `Node n -> `Node (Node.hash n)
    | `Contents (c, m) -> `Contents (Contents.hash c, m)

  let stats ?(force = false) (t : t) =
    let force =
      if force then `True
      else `False (fun k s -> set_depth k s |> incr_skips |> IO.return)
    in
    let contents k _ s = set_depth k s |> incr_leafs |> IO.return in
    let pre k childs s =
      if childs = [] then IO.return s
      else set_depth k s |> set_width childs |> incr_nodes |> IO.return
    in
    let post _ _ acc = IO.return acc in
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
