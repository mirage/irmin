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
module Irmin_node = Node

let src = Logs.Src.create "irmin.tree" ~doc:"Persistent lazy trees for Irmin"

module Log = (val Logs.src_log src : Logs.LOG)

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

    let stdlib_merge = merge

    include Merge.Map (X)

    let to_array m =
      let length = cardinal m in
      if length = 0 then [||]
      else
        let arr = Array.make length (choose m) in
        let (_ : int) =
          fold
            (fun k v i ->
              arr.(i) <- (k, v);
              i + 1)
            m 0
        in
        arr
  end

  module Metadata = P.Node.Metadata

  type key = Path.t
  type hash = P.Hash.t

  let compare_hash = Type.(unstage (compare P.Hash.t))

  type error = [ `Dangling_hash of hash | `Pruned_hash of hash ]
  type 'a or_error = ('a, error) result
  type step = Path.step
  type contents = P.Contents.value
  type repo = P.Repo.t

  let pp_hash = Type.pp P.Hash.t
  let pp_step = Type.pp Path.step_t
  let pp_path = Type.pp Path.t

  module Hashes = Hashtbl.Make (struct
    type t = hash

    let hash = P.Hash.short_hash
    let equal = Type.(unstage (equal P.Hash.t))
  end)

  let dummy_marks = Hashes.create 0

  type marks = unit Hashes.t

  let empty_marks () = Hashes.create 39

  type 'a force = [ `True | `False of key -> 'a -> 'a Lwt.t ]
  type uniq = [ `False | `True | `Marks of marks ]
  type 'a node_fn = key -> step list -> 'a -> 'a Lwt.t

  type depth = [ `Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int ]
  [@@deriving irmin]

  let equal_contents = Type.(unstage (equal P.Contents.Val.t))
  let equal_metadata = Type.(unstage (equal Metadata.t))
  let equal_hash = Type.(unstage (equal P.Hash.t))
  let equal_node = Type.(unstage (equal P.Node.Val.t))

  exception Pruned_hash of { context : string; hash : hash }
  exception Dangling_hash of { context : string; hash : hash }

  let () =
    Printexc.register_printer (function
      | Dangling_hash { context; hash } ->
          Some
            (Fmt.str "Irmin.Tree.%s: encountered dangling hash %a" context
               pp_hash hash)
      | Pruned_hash { context; hash } ->
          Some
            (Fmt.str "Irmin.Tree.%s: encountered pruned hash %a" context pp_hash
               hash)
      | _ -> None)

  let err_pruned h = Error (`Pruned_hash h)
  let raise_pruned context hash = raise (Pruned_hash { context; hash })

  let get_ok : type a. string -> a or_error -> a =
   fun context -> function
    | Ok x -> x
    | Error (`Pruned_hash hash) -> raise_pruned context hash
    | Error (`Dangling_hash hash) -> raise (Dangling_hash { context; hash })

  module Env = Proof.Env (P.Hash) (P.Contents.Val) (P.Node.Val)

  module Contents = struct
    type v = Hash of repo option * hash | Value of contents

    type info = {
      mutable hash : hash option;
      mutable value : contents option;
      env : Env.t;
    }

    type t = { mutable v : v; info : info }

    let info_is_empty i = i.hash = None && i.value = None

    let v =
      let open Type in
      variant "Node.Contents.v" (fun hash value -> function
        | Hash (_, x) -> hash x | Value v -> value v)
      |~ case1 "hash" P.Hash.t (fun h -> Hash (None, h))
      |~ case1 "value" P.Contents.Val.t (fun v -> Value v)
      |> sealv

    let clear_info i =
      if not (info_is_empty i) then (
        i.value <- None;
        i.hash <- None)

    let clear t = clear_info t.info

    let of_v ~env ?hash v =
      let hash =
        match (v, hash) with
        | Hash (_, k), _ -> Some k
        | _, (Some _ as k) -> k
        | _ -> None
      in
      let value =
        match Env.find_contents_opt env hash with
        | Some _ as v -> v
        | None -> ( match v with Value v -> Some v | _ -> None)
      in
      let info = { hash; value; env } in
      let v =
        (* Use the value from [Env.find_contents_opt] in priority over the one
           input to [of_v]. *)
        match value with Some v -> Value v | _ -> v
      in
      { v; info }

    let export ?clear:(c = true) repo t k =
      let hash = t.info.hash in
      if c then clear t;
      match (t.v, hash) with
      | Hash (None, _), _ ->
          (* The main export function never exports a pruned position. *)
          assert false
      | Hash (Some repo', _), _ when repo == repo' -> ()
      | Hash (_, k), _ -> t.v <- Hash (Some repo, k)
      | Value _, None -> t.v <- Hash (Some repo, k)
      | Value _, Some k -> t.v <- Hash (Some repo, k)

    let of_value ?hash c = of_v ?hash (Value c)
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
      | Value v, None -> Some v
      | _, (Some _ as v) -> v
      | _ -> (
          match Env.find_contents_opt t.info.env (cached_hash t) with
          | Some _ as v ->
              t.info.value <- v;
              v
          | v -> v)

    let hash ?(cache = true) c =
      match cached_hash c with
      | Some k -> k
      | None -> (
          match cached_value c with
          | None -> assert false
          | Some v ->
              cnt.contents_hash <- cnt.contents_hash + 1;
              let k = P.Contents.Key.hash v in
              if cache then c.info.hash <- Some k;
              k)

    let value_of_hash ~cache t repo k =
      match cached_value t with
      | Some v -> Lwt.return_ok v
      | None -> (
          cnt.contents_find <- cnt.contents_find + 1;
          let+ some_v = P.Contents.find (P.Repo.contents_t repo) k in
          Env.add_contents_opt t.info.env k some_v;
          if cache then t.info.value <- some_v;
          match some_v with None -> Error (`Dangling_hash k) | Some v -> Ok v)

    let to_value_aux ~cache ~value_of_hash ~return t =
      match cached_value t with
      | Some v -> return (Ok v)
      | None -> (
          match t.v with
          | Value v -> return (Ok v)
          | Hash (Some repo, k) -> value_of_hash ~cache t repo k
          | Hash (None, h) -> return (err_pruned h))

    let to_value = to_value_aux ~value_of_hash ~return:Lwt.return
    let force = to_value ~cache:true

    let force_exn t =
      force t >|= function
      | Ok v -> v
      | Error (`Pruned_hash hash) ->
          raise (Pruned_hash { context = "force_exn"; hash })
      | Error (`Dangling_hash hash) ->
          raise (Dangling_hash { context = "force_exn"; hash })

    let equal (x : t) (y : t) =
      x == y
      ||
      match (cached_hash x, cached_hash y) with
      | Some x, Some y -> equal_hash x y
      | _ -> (
          match (cached_value x, cached_value y) with
          | Some x, Some y -> equal_contents x y
          | _ -> equal_hash (hash ~cache:true x) (hash ~cache:true y))

    let compare (x : t) (y : t) =
      if x == y then 0
      else compare_hash (hash ~cache:true x) (hash ~cache:true y)

    let t =
      let of_v v = of_v ~env:(Env.empty ()) v in
      Type.map ~equal ~compare v of_v (fun t -> t.v)

    let merge : t Merge.t =
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              let+ c = to_value ~cache:true old >|= Option.of_result in
              Ok (Some c))
        in
        let x_env = x.info.env in
        let y_env = y.info.env in
        let* x = to_value ~cache:true x >|= Option.of_result in
        let* y = to_value ~cache:true y >|= Option.of_result in
        Merge.(f P.Contents.Val.merge) ~old x y >|= function
        | Ok (Some c) ->
            Env.merge x_env y_env;
            Ok (of_value ~env:x_env c)
        | Ok None -> Error (`Conflict "empty contents")
        | Error _ as e -> e
      in
      Merge.v t f

    let fold ~force ~cache ~path f_value f_tree t acc =
      match force with
      | `True ->
          let* c = to_value ~cache t in
          f_value path (get_ok "fold" c) acc >>= f_tree path
      | `False skip -> (
          match cached_value t with
          | None -> skip path acc
          | Some c -> f_value path c acc >>= f_tree path)
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
      env : Env.t;
    }

    and v =
      | Map of map
      | Hash of repo option * hash
      | Value of repo option * value * updatemap option

    and t = { mutable v : v; info : info }
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
      |~ case1 "hash" P.Hash.t (fun h -> Hash (None, h))
      |~ case1 "value"
           (pair P.Node.Val.t (option um))
           (fun (v, o) -> Value (None, v, o))
      |> sealv

    let of_v ~env v =
      let hash = match v with Hash (_, k) -> Some k | _ -> None in
      let value =
        match Env.find_node_opt env hash with
        | Some _ as v -> v
        | None -> ( match v with Value (_, v, None) -> Some v | _ -> None)
      in
      let map = match v with Map m -> Some m | _ -> None in
      let findv_cache = None in
      let info = { hash; map; value; findv_cache; env } in
      let v =
        (* Use the value from [Env.find_node_opt] in priority over the one
           input to [of_v]. *)
        match (v, value) with
        | Value (repo, _, _), Some v -> Value (repo, v, None)
        | _ -> v
      in
      { v; info }

    let of_map m = of_v (Map m)
    let of_hash repo k = of_v (Hash (repo, k))
    let of_value ?updates repo v = of_v (Value (repo, v, updates))

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
      | Value (_, v, None), None -> Some v
      | _, (Some _ as v) -> v
      | _ -> (
          match Env.find_node_opt t.info.env (cached_hash t) with
          | Some _ as v ->
              t.info.value <- v;
              v
          | v -> v)

    let info_is_empty i =
      i.map = None && i.value = None && i.findv_cache = None && i.hash = None

    let clear_info_fields i =
      if not (info_is_empty i) then (
        i.value <- None;
        i.map <- None;
        i.hash <- None;
        i.findv_cache <- None)

    let rec clear_elt ~max_depth depth v =
      match v with
      | `Contents (c, _) -> if depth + 1 > max_depth then Contents.clear c
      | `Node t -> clear ~max_depth (depth + 1) t

    and clear_info ~max_depth ?v depth i =
      let clear _ v = clear_elt ~max_depth depth v in
      let () =
        match v with
        | Some (Value (_, _, Some um)) ->
            StepMap.iter
              (fun k -> function Remove -> () | Add v -> clear k v)
              um
        | _ -> ()
      in
      let () =
        match (v, i.map) with
        | Some (Map m), _ | _, Some m -> StepMap.iter clear m
        | _ -> ()
      in
      let () =
        match i.findv_cache with Some m -> StepMap.iter clear m | None -> ()
      in
      if depth >= max_depth then clear_info_fields i

    and clear ~max_depth depth t = clear_info ~v:t.v ~max_depth depth t.info

    (* export t to the given repo and clear the cache *)
    let export ?clear:(c = true) repo t k =
      let hash = t.info.hash in
      if c then clear_info_fields t.info;
      match t.v with
      | Hash (None, _) | Value (None, _, _) ->
          (* The main export function never exports a pruned position. *)
          assert false
      | Hash (repo', _) when repo' == repo -> ()
      | Hash (_, k) -> t.v <- Hash (repo, k)
      | _ -> (
          match hash with
          | None -> t.v <- Hash (repo, k)
          | Some k -> t.v <- Hash (repo, k))

    let map_of_value ~cache ~env repo (n : value) : map =
      cnt.node_val_list <- cnt.node_val_list + 1;
      let entries = P.Node.Val.seq ~cache n in
      let aux = function
        | `Node h -> `Node (of_hash ~env repo h)
        | `Contents (c, m) -> `Contents (Contents.of_hash ~env repo c, m)
      in
      Seq.fold_left
        (fun acc (k, v) -> StepMap.add k (aux v) acc)
        StepMap.empty entries

    let rec hash : type a. cache:bool -> t -> (hash -> a) -> a =
     fun ~cache t k ->
      match cached_hash t with
      | Some h -> k h
      | None -> (
          let a_of_value v =
            cnt.node_hash <- cnt.node_hash + 1;
            let h = P.Node.Key.hash v in
            if cache then t.info.hash <- Some h;
            k h
          in
          match cached_value t with
          | Some v -> a_of_value v
          | None -> (
              match t.v with
              | Hash (_, h) -> k h
              | Value (_, v, None) -> a_of_value v
              | Value (_, v, Some um) ->
                  value_of_updates ~cache t v um a_of_value
              | Map m -> value_of_map ~cache t m a_of_value))

    and value_of_map : type r. cache:bool -> t -> map -> (value, r) cont =
     fun ~cache t map k ->
      cnt.node_val_v <- cnt.node_val_v + 1;
      let v =
        StepMap.to_seq map
        |> Seq.map (function
             | step, `Contents (c, m) ->
                 (step, `Contents (Contents.hash ~cache c, m))
             | step, `Node n -> (step, hash ~cache n (fun h -> `Node h)))
        |> P.Node.Val.of_seq
      in
      if cache then t.info.value <- Some v;
      k v

    and value_of_elt : type r. cache:bool -> elt -> (P.Node.Val.value, r) cont =
     fun ~cache e k ->
      match e with
      | `Contents (c, m) -> k (`Contents (Contents.hash ~cache c, m))
      | `Node n -> hash ~cache n (fun h -> k (`Node h))

    and value_of_updates :
        type r. cache:bool -> t -> value -> _ -> (value, r) cont =
     fun ~cache t v updates k ->
      let updates = StepMap.bindings updates in
      let rec aux acc = function
        | [] ->
            if cache then t.info.value <- Some acc;
            k acc
        | (k, Add e) :: rest ->
            value_of_elt ~cache e (fun e -> aux (P.Node.Val.add acc k e) rest)
        | (k, Remove) :: rest -> aux (P.Node.Val.remove acc k) rest
      in
      aux v updates

    let hash ~cache k = hash ~cache k (fun x -> x)

    let value_of_hash ~cache t repo k =
      match cached_value t with
      | Some v -> Lwt.return_ok v
      | None -> (
          cnt.node_find <- cnt.node_find + 1;
          let+ some_v = P.Node.find (P.Repo.node_t repo) k in
          let some_v = Env.add_node_opt t.info.env k some_v in
          if cache then t.info.value <- some_v;
          match some_v with None -> Error (`Dangling_hash k) | Some v -> Ok v)

    let to_value_aux ~cache ~value_of_hash ~return t =
      let ok x = return (Ok x) in
      match cached_value t with
      | Some v -> ok v
      | None -> (
          match t.v with
          | Value (_, v, None) -> ok v
          | Value (_, v, Some um) -> value_of_updates ~cache t v um ok
          | Map m -> value_of_map ~cache t m ok
          | Hash (Some repo, h) -> value_of_hash ~cache t repo h
          | Hash (None, h) -> return (err_pruned h))

    let to_value = to_value_aux ~value_of_hash ~return:Lwt.return

    let to_map ~cache t =
      match cached_map t with
      | Some m -> Lwt.return (Ok m)
      | None -> (
          let env = t.info.env in
          let of_value repo v updates =
            let m = map_of_value ~cache ~env repo v in
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
            if cache then t.info.map <- Some m;
            m
          in
          match t.v with
          | Map m -> Lwt.return (Ok m)
          | Value (repo, v, m) -> Lwt.return (Ok (of_value repo v m))
          | Hash (Some repo, k) -> (
              value_of_hash ~cache t repo k >|= function
              | Error _ as e -> e
              | Ok v -> Ok (of_value (Some repo) v None))
          | Hash (None, h) -> Lwt.return (err_pruned h))

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
              | _ -> hash_equal (hash ~cache:true x) (hash ~cache:true y)))

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

    let empty () = of_map StepMap.empty ~env:(Env.empty ())
    let empty_hash = hash ~cache:false (empty ())

    (** Does [um] empties [v]?

        Gotcha: Some [Remove] entries in [um] might not be in [v]. *)
    let is_empty_after_updates ~cache v um =
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
            let entries = P.Node.Val.seq ~cache v in
            Seq.for_all (fun (step, _) -> StepMap.mem step um) entries)

    let length ~cache t =
      match cached_map t with
      | Some m -> StepMap.cardinal m |> Lwt.return
      | None ->
          let+ v = to_value ~cache t in
          get_ok "length" v |> P.Node.Val.length

    let is_empty ~cache t =
      match cached_map t with
      | Some m -> StepMap.is_empty m
      | None -> (
          match cached_value t with
          | Some v -> P.Node.Val.is_empty v
          | None -> (
              match t.v with
              | Value (_, v, Some um) -> is_empty_after_updates ~cache v um
              | Hash (_, h) -> hash_equal empty_hash h
              | Map _ -> assert false (* [cached_map <> None] *)
              | Value (_, _, None) -> assert false (* [cached_value <> None] *))
          )

    let add_to_findv_cache t step v =
      match t.info.findv_cache with
      | None -> t.info.findv_cache <- Some (StepMap.singleton step v)
      | Some m -> t.info.findv_cache <- Some (StepMap.add step v m)

    let findv_aux ~cache ~value_of_hash ~return ~bind ctx t step =
      let of_map m = try Some (StepMap.find step m) with Not_found -> None in
      let of_value repo v =
        let env = t.info.env in
        match P.Node.Val.find ~cache v step with
        | None -> None
        | Some (`Contents (c, m)) ->
            let c = Contents.of_hash ~env repo c in
            let (v : elt) = `Contents (c, m) in
            if cache then add_to_findv_cache t step v;
            Some v
        | Some (`Node n) ->
            let n = of_hash ~env repo n in
            let v = `Node n in
            if cache then add_to_findv_cache t step v;
            Some v
      in
      let of_t () =
        match t.v with
        | Map m -> return (of_map m)
        | Value (repo, v, None) -> return (of_value repo v)
        | Value (repo, v, Some um) -> (
            match StepMap.find_opt step um with
            | Some (Add v) -> return (Some v)
            | Some Remove -> return None
            | None -> return (of_value repo v))
        | Hash (repo, h) -> (
            match cached_value t with
            | Some v -> return (of_value repo v)
            | None -> (
                match repo with
                | None -> raise_pruned "Node.find" h
                | Some repo ->
                    bind (value_of_hash ~cache t repo h) (fun v ->
                        let v = get_ok ctx v in
                        return (of_value (Some repo) v))))
      in

      match cached_map t with
      | Some m -> return (of_map m)
      | None -> (
          match t.info.findv_cache with
          | None -> of_t ()
          | Some m -> (
              match of_map m with None -> of_t () | Some _ as r -> return r))

    let findv = findv_aux ~value_of_hash ~return:Lwt.return ~bind:Lwt.bind

    let seq_of_map ?(offset = 0) ?length m : (step * elt) Seq.t =
      let take seq =
        match length with None -> seq | Some n -> Seq.take n seq
      in
      StepMap.to_seq m |> Seq.drop offset |> take

    let seq_of_value ~env repo ?offset ?length ~cache v : (step * elt) Seq.t =
      cnt.node_val_list <- cnt.node_val_list + 1;
      let seq = P.Node.Val.seq ?offset ?length ~cache v in
      Seq.map
        (fun (k, v) ->
          match v with
          | `Node n ->
              let n = `Node (of_hash ~env repo n) in
              (k, n)
          | `Contents (c, m) ->
              let c = Contents.of_hash ~env repo c in
              (k, `Contents (c, m)))
        seq

    let seq ?offset ?length ~cache t : (step * elt) Seq.t or_error Lwt.t =
      let env = t.info.env in
      match cached_map t with
      | Some m -> ok (seq_of_map ?offset ?length m)
      | None -> (
          match t.v with
          | Value (repo, n, None) ->
              ok (seq_of_value ~env ?offset ?length ~cache repo n)
          | Hash (Some repo, h) -> (
              value_of_hash ~cache t repo h >>= function
              | Error _ as e -> Lwt.return e
              | Ok v ->
                  ok (seq_of_value ~env ?offset ?length ~cache (Some repo) v))
          | _ -> (
              to_map ~cache t >>= function
              | Error _ as e -> Lwt.return e
              | Ok m -> ok (seq_of_map ?offset ?length m)))

    let bindings ~cache t =
      (* XXX: If [t] is value, no need to [to_map]. Let's remove and inline
         this into Tree.entries. *)
      to_map ~cache t >|= function
      | Error _ as e -> e
      | Ok m -> Ok (StepMap.bindings m)

    let seq_of_updates updates value_bindings =
      (* This operation can be costly for large updates. *)
      if StepMap.is_empty updates then
        (* Short-circuit return if we have no more updates to apply. *)
        value_bindings
      else
        let value_bindings =
          Seq.filter (fun (s, _) -> not (StepMap.mem s updates)) value_bindings
        in
        let updates =
          StepMap.to_seq updates
          |> Seq.filter_map (fun (s, elt) ->
                 match elt with Remove -> None | Add e -> Some (s, e))
        in
        Seq.append value_bindings updates

    type ('v, 'acc, 'r) folder =
      path:key -> 'acc -> int -> 'v -> ('acc, 'r) cont_lwt
    (** A ('val, 'acc, 'r) folder is a CPS, threaded fold function over values
        of type ['v] producing an accumulator of type ['acc]. *)

    let fold :
        type acc.
        order:[ `Sorted | `Undefined | `Random of Random.State.t ] ->
        force:acc force ->
        cache:bool ->
        uniq:uniq ->
        pre:acc node_fn option ->
        post:acc node_fn option ->
        path:Path.t ->
        ?depth:depth ->
        node:(key -> _ -> acc -> acc Lwt.t) ->
        contents:(key -> contents -> acc -> acc Lwt.t) ->
        tree:(key -> _ -> acc -> acc Lwt.t) ->
        t ->
        acc ->
        acc Lwt.t =
     fun ~order ~force ~cache ~uniq ~pre ~post ~path ?depth ~node ~contents
         ~tree t acc ->
      let env = t.info.env in
      let marks =
        match uniq with
        | `False -> dummy_marks
        | `True -> empty_marks ()
        | `Marks n -> n
      in
      let pre path bindings acc =
        match pre with
        | None -> Lwt.return acc
        | Some pre ->
            let s = Seq.fold_left (fun acc (s, _) -> s :: acc) [] bindings in
            pre path s acc
      in
      let post path bindings acc =
        match post with
        | None -> Lwt.return acc
        | Some post ->
            let s = Seq.fold_left (fun acc (s, _) -> s :: acc) [] bindings in
            post path s acc
      in
      let rec aux : type r. (t, acc, r) folder =
       fun ~path acc d t k ->
        let apply acc = node path t acc >>= tree path (`Node t) in
        let next acc =
          match force with
          | `True -> (
              match (order, t.v) with
              | `Random state, _ ->
                  let* m = to_map ~cache t >|= get_ok "fold" in
                  let arr = StepMap.to_array m in
                  let () = shuffle state arr in
                  let s = Array.to_seq arr in
                  (seq [@tailcall]) ~path acc d s k
              | `Sorted, _ | `Undefined, Map _ ->
                  let* m = to_map ~cache t >|= get_ok "fold" in
                  (map [@tailcall]) ~path acc d (Some m) k
              | `Undefined, Value (repo, v, updates) ->
                  (value [@tailcall]) ~path acc d (repo, v, updates) k
              | `Undefined, Hash (repo, _) ->
                  let* v = to_value ~cache t >|= get_ok "fold" in
                  (value [@tailcall]) ~path acc d (repo, v, None) k)
          | `False skip -> (
              match cached_map t with
              | Some n -> (map [@tailcall]) ~path acc d (Some n) k
              | None ->
                  (* XXX: That node is skipped if is is of tag Value *)
                  skip path acc >>= k)
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
          let h = hash ~cache t in
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
              let tree path = tree path (`Contents c) in
              Contents.fold ~force ~cache ~path contents tree (fst c) acc >>= k
            in
            match depth with
            | None -> apply ()
            | Some (`Eq depth) -> if d = depth - 1 then apply () else k acc
            | Some (`Le depth) -> if d < depth then apply () else k acc
            | Some (`Lt depth) -> if d < depth - 1 then apply () else k acc
            | Some (`Ge depth) -> if d >= depth - 1 then apply () else k acc
            | Some (`Gt depth) -> if d >= depth then apply () else k acc)
      and steps : type r. ((step * elt) Seq.t, acc, r) folder =
       fun ~path acc d s k ->
        match s () with
        | Seq.Nil -> k acc
        | Seq.Cons (h, t) ->
            (step [@tailcall]) ~path acc d h (fun acc ->
                (steps [@tailcall]) ~path acc d t k)
      and map : type r. (map option, acc, r) folder =
       fun ~path acc d m k ->
        match m with
        | None -> k acc
        | Some m ->
            let bindings = StepMap.to_seq m in
            seq ~path acc d bindings k
      and value :
          type r. (repo option * value * updatemap option, acc, r) folder =
       fun ~path acc d (repo, v, updates) k ->
        let to_elt = function
          | `Node n -> `Node (of_hash ~env repo n)
          | `Contents (c, m) -> `Contents (Contents.of_hash ~env repo c, m)
        in
        let bindings =
          P.Node.Val.seq v |> Seq.map (fun (s, v) -> (s, to_elt v))
        in
        let bindings =
          match updates with
          | None -> bindings
          | Some updates -> seq_of_updates updates bindings
        in
        seq ~path acc d bindings k
      and seq : type r. ((step * elt) Seq.t, acc, r) folder =
       fun ~path acc d bindings k ->
        let* acc = pre path bindings acc in
        (steps [@tailcall]) ~path acc d bindings (fun acc ->
            post path bindings acc >>= k)
      in
      aux_uniq ~path acc 0 t Lwt.return

    let update t step up =
      let env = t.info.env in
      let of_map m =
        let m' =
          match up with
          | Remove -> StepMap.remove step m
          | Add v -> StepMap.add step v m
        in
        if m == m' then t else of_map ~env m'
      in
      let of_value repo n updates =
        let updates' = StepMap.add step up updates in
        if updates == updates' then t
        else of_value ~env repo n ~updates:updates'
      in
      match t.v with
      | Map m -> Lwt.return (of_map m)
      | Value (repo, n, None) -> Lwt.return (of_value repo n StepMap.empty)
      | Value (repo, n, Some um) -> Lwt.return (of_value repo n um)
      | Hash (repo, h) -> (
          match (cached_value t, cached_map t) with
          | Some v, _ -> Lwt.return (of_value repo v StepMap.empty)
          | _, Some m -> Lwt.return (of_map m)
          | None, None -> (
              match repo with
              | None -> raise_pruned "update" h
              | Some repo ->
                  let+ v =
                    value_of_hash ~cache:true t repo h >|= get_ok "update"
                  in
                  of_value (Some repo) v StepMap.empty))

    let remove t step = update t step Remove
    let add t step v = update t step (Add v)

    let compare (x : t) (y : t) =
      if x == y then 0
      else compare_hash (hash ~cache:true x) (hash ~cache:true y)

    let t node =
      let of_v v = of_v ~env:(Env.empty ()) v in
      Type.map ~equal ~compare node of_v (fun t -> t.v)

    let _, t =
      Type.mu2 (fun _ y ->
          let elt = elt_t y in
          let v = v_t elt in
          let t = t v in
          (v, t))

    let elt_t = elt_t t
    let dump = Type.pp_json ~minify:false t

    let rec merge : type a. (t Merge.t -> a) -> a =
     fun k ->
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              let+ m = to_map ~cache:true old >|= Option.of_result in
              Ok (Some m))
        in
        let x_env = x.info.env in
        let y_env = y.info.env in
        let* x = to_map ~cache:true x >|= Option.of_result in
        let* y = to_map ~cache:true y >|= Option.of_result in
        let m =
          StepMap.merge elt_t (fun _step ->
              (merge_elt [@tailcall]) Merge.option)
        in
        Merge.(f @@ option m) ~old x y >|= function
        | Ok (Some map) ->
            Env.merge x_env y_env;
            Ok (of_map ~env:x_env map)
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

  type t = [ `Node of node | `Contents of Contents.t * Metadata.t ]
  [@@deriving irmin { name = "tree_t" }]

  let of_private_node repo n = Node.of_value ~env:(Env.empty ()) (Some repo) n
  let to_private_node = Node.to_value ~cache:true

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
    | `Node n -> Node.is_empty ~cache:true n
    | `Contents _ -> false

  type elt = [ `Node of node | `Contents of contents * metadata ]

  let of_node n = `Node n

  let of_contents ?(metadata = Metadata.default) c =
    let env = Env.empty () in
    let c = Contents.of_value ~env c in
    `Contents (c, metadata)

  let v : elt -> t = function
    | `Contents (c, metadata) -> of_contents ~metadata c
    | `Node n -> `Node n

  type kinded_hash = [ `Contents of P.Hash.t * Metadata.t | `Node of P.Hash.t ]
  [@@deriving irmin ~equal]

  let pruned : kinded_hash -> t = function
    | `Contents (h, meta) ->
        `Contents (Contents.of_hash ~env:(Env.empty ()) None h, meta)
    | `Node h -> `Node (Node.of_hash ~env:(Env.empty ()) None h)

  let destruct x = x

  let clear ?(depth = 0) = function
    | `Node n -> Node.clear ~max_depth:depth 0 n
    | `Contents _ -> ()

  let sub ~cache ctx t path =
    let rec aux node path =
      match Path.decons path with
      | None -> Lwt.return_some node
      | Some (h, p) -> (
          Node.findv ~cache ctx node h >>= function
          | None | Some (`Contents _) -> Lwt.return_none
          | Some (`Node n) -> (aux [@tailcall]) n p)
    in
    match t with
    | `Node n -> (aux [@tailcall]) n path
    | `Contents _ -> Lwt.return_none

  let find_tree (t : t) path =
    let cache = true in
    Log.debug (fun l -> l "Tree.find_tree %a" pp_path path);
    match (t, Path.rdecons path) with
    | v, None -> Lwt.return_some v
    | _, Some (path, file) -> (
        sub ~cache "find_tree.sub" t path >>= function
        | None -> Lwt.return_none
        | Some n -> Node.findv ~cache "find_tree" n file)

  let id _ _ acc = Lwt.return acc

  let fold ?(order = `Sorted) ?(force = `True) ?(cache = false) ?(uniq = `False)
      ?pre ?post ?depth ?(contents = id) ?(node = id) ?(tree = id) (t : t) acc =
    match t with
    | `Contents (c, _) as c' ->
        let tree path = tree path c' in
        Contents.fold ~force ~cache ~path:Path.empty contents tree c acc
    | `Node n ->
        Node.fold ~order ~force ~cache ~uniq ~pre ~post ~path:Path.empty ?depth
          ~contents ~node ~tree n acc

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
    Fmt.kstr invalid_arg "Irmin.Tree.%s: %a not found" n pp_path k

  let get_tree (t : t) path =
    find_tree t path >|= function
    | None -> err_not_found "get_tree" path
    | Some v -> v

  let find_all t k =
    find_tree t k >>= function
    | None | Some (`Node _) -> Lwt.return_none
    | Some (`Contents (c, m)) ->
        let+ c = Contents.to_value ~cache:true c in
        Some (get_ok "find_all" c, m)

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
    let cache = true in
    Log.debug (fun l -> l "Tree.kind %a" pp_path path);
    match (t, Path.rdecons path) with
    | `Contents _, None -> Lwt.return_some `Contents
    | `Node _, None -> Lwt.return_some `Node
    | _, Some (dir, file) -> (
        sub "kind.sub" ~cache t dir >>= function
        | None -> Lwt.return_none
        | Some m -> (
            Node.findv "kind.findv" ~cache m file >>= function
            | None -> Lwt.return_none
            | Some (`Contents _) -> Lwt.return_some `Contents
            | Some (`Node _) -> Lwt.return_some `Node))

  let length t ?(cache = true) path =
    Log.debug (fun l -> l "Tree.length %a" pp_path path);
    sub ~cache "length" t path >>= function
    | None -> Lwt.return 0
    | Some n -> Node.length ~cache:true n

  let seq t ?offset ?length ~cache path : (step * t) Seq.t Lwt.t =
    Log.debug (fun l -> l "Tree.seq %a" pp_path path);
    sub ~cache "seq" t path >>= function
    | None -> Lwt.return Seq.empty
    | Some n -> (
        Node.seq ?offset ?length ~cache n >|= function
        | Error _ -> Seq.empty
        | Ok l -> l)

  let list t ?offset ?length ?(cache = true) path =
    seq t ?offset ?length ~cache path >|= List.of_seq

  let empty () = `Node (Node.empty ())

  (** During recursive updates, we keep track of whether or not we've made a
      modification in order to avoid unnecessary allocations of identical tree
      objects. *)
  type 'a updated = Changed of 'a | Unchanged

  let maybe_equal (x : t) (y : t) =
    if x == y then True
    else
      match (x, y) with
      | `Node x, `Node y -> Node.maybe_equal x y
      | _ -> if equal x y then True else False

  let get_env = function
    | `Node n -> n.Node.info.env
    | `Contents (c, _) -> c.Contents.info.env

  let update_tree ~cache ~f_might_return_empty_node ~f root_tree path =
    (* User-introduced empty nodes will be removed immediately if necessary. *)
    let prune_empty : node -> bool =
      if not f_might_return_empty_node then Fun.const false
      else Node.is_empty ~cache
    in
    match Path.rdecons path with
    | None -> (
        let empty_tree () =
          match is_empty root_tree with true -> root_tree | false -> empty ()
        in
        f (Some root_tree) >>= function
        (* Here we consider "deleting" a root contents value or node to consist
           of changing it to an empty node. Note that this introduces
           sensitivity to ordering of subtree operations: updating in a subtree
           and adding the subtree are not necessarily commutative. *)
        | None -> Lwt.return (empty_tree ())
        | Some (`Node _ as new_root) -> (
            match maybe_equal root_tree new_root with
            | True -> Lwt.return root_tree
            | Maybe | False -> Lwt.return new_root)
        | Some (`Contents c' as new_root) -> (
            match root_tree with
            | `Contents c when contents_equal c c' -> Lwt.return root_tree
            | _ -> Lwt.return new_root))
    | Some (path, file) -> (
        let rec aux : type r. key -> node -> (node updated, r) cont_lwt =
         fun path parent_node k ->
          let changed (n : node) = k (Changed n) in
          match Path.decons path with
          | None -> (
              let with_new_child t = Node.add parent_node file t >>= changed in
              let* old_binding =
                Node.findv ~cache "update_tree.findv" parent_node file
              in
              let* new_binding = f old_binding in
              match (old_binding, new_binding) with
              | None, None -> k Unchanged
              | None, Some (`Contents _ as t) -> with_new_child t
              | None, Some (`Node n as t) -> (
                  match prune_empty n with
                  | true -> k Unchanged
                  | false -> with_new_child t)
              | Some _, None -> Node.remove parent_node file >>= changed
              | Some old_value, Some (`Node n as t) -> (
                  match prune_empty n with
                  | true -> Node.remove parent_node file >>= changed
                  | false -> (
                      match maybe_equal old_value t with
                      | True -> k Unchanged
                      | Maybe | False -> with_new_child t))
              | Some (`Contents c), Some (`Contents c' as t) -> (
                  match contents_equal c c' with
                  | true -> k Unchanged
                  | false -> with_new_child t)
              | Some (`Node _), Some (`Contents _ as t) -> with_new_child t)
          | Some (step, key_suffix) ->
              let* old_binding =
                Node.findv ~cache "update_tree.findv" parent_node step
              in
              let to_recurse =
                match old_binding with
                | Some (`Node child) -> child
                | None | Some (`Contents _) -> Node.empty ()
              in
              (aux [@tailcall]) key_suffix to_recurse (function
                | Unchanged ->
                    (* This includes [remove]s in an empty node, in which case we
                       want to avoid adding a binding anyway. *)
                    k Unchanged
                | Changed child -> (
                    match Node.is_empty ~cache child with
                    | true ->
                        (* A [remove] has emptied previously non-empty child with
                           binding [h], so we remove the binding. *)
                        Node.remove parent_node step >>= changed
                    | false ->
                        Node.add parent_node step (`Node child) >>= changed))
        in
        let top_node =
          match root_tree with `Node n -> n | `Contents _ -> Node.empty ()
        in
        aux path top_node @@ function
        | Unchanged -> Lwt.return root_tree
        | Changed node ->
            Env.copy ~into:node.info.env (get_env root_tree);
            Lwt.return (`Node node))

  let update root k ?(metadata = Metadata.default) f =
    Log.debug (fun l -> l "Tree.update %a" pp_path k);
    let cache = true in
    update_tree ~cache root k ~f_might_return_empty_node:false ~f:(fun t ->
        let+ old_contents =
          match t with
          | Some (`Node _) | None -> Lwt.return_none
          | Some (`Contents (c, _)) ->
              let+ c = Contents.to_value ~cache c in
              Some (get_ok "update" c)
        in
        match f old_contents with
        | None -> None
        | Some c -> of_contents ~metadata c |> Option.some)

  let add t k ?(metadata = Metadata.default) c =
    Log.debug (fun l -> l "Tree.add %a" pp_path k);
    update_tree ~cache:true t k
      ~f:(fun _ -> Lwt.return_some (of_contents ~metadata c))
      ~f_might_return_empty_node:false

  let add_tree t k v =
    Log.debug (fun l -> l "Tree.add_tree %a" pp_path k);
    update_tree ~cache:true t k
      ~f:(fun _ -> Lwt.return_some v)
      ~f_might_return_empty_node:true

  let remove t k =
    Log.debug (fun l -> l "Tree.remove %a" pp_path k);
    update_tree ~cache:true t k
      ~f:(fun _ -> Lwt.return_none)
      ~f_might_return_empty_node:false

  let update_tree t k f =
    Log.debug (fun l -> l "Tree.update_tree %a" pp_path k);
    update_tree ~cache:true t k ~f:(Lwt.wrap1 f) ~f_might_return_empty_node:true

  let import repo = function
    | `Contents (k, m) -> (
        P.Contents.mem (P.Repo.contents_t repo) k >|= function
        | true ->
            let env = Env.empty () in
            let c = Contents.of_hash ~env (Some repo) k in
            Some (`Contents (c, m))
        | false -> None)
    | `Node k -> (
        let env = Env.empty () in
        cnt.node_mem <- cnt.node_mem + 1;
        P.Node.mem (P.Repo.node_t repo) k >|= function
        | true -> Some (`Node (Node.of_hash ~env (Some repo) k))
        | false -> None)

  let import_with_env ~env repo = function
    | `Node k -> `Node (Node.of_hash ~env (Some repo) k)
    | `Contents (k, m) -> `Contents (Contents.of_hash ~env (Some repo) k, m)

  let import_no_check repo f = import_with_env ~env:(Env.empty ()) repo f

  let export ?clear repo contents_t node_t n =
    let cache =
      match clear with
      | Some true | None ->
          (* This choice of [cache] flag has no impact, since we either
             immediately clear the corresponding cache or are certain that
             the it is already filled. *)
          false
      | Some false -> true
    in
    let skip n =
      match Node.cached_hash n with
      | Some h ->
          cnt.node_mem <- cnt.node_mem + 1;
          P.Node.mem node_t h
      | None -> Lwt.return_false
    in
    let rec on_node (`Node n) k =
      match n.Node.v with
      | Node.Hash (None, h) -> raise_pruned "export.node" h
      | Node.Value (None, _, _) ->
          let h = Node.hash ~cache:false n in
          raise_pruned "export" h
      | Node.Hash (Some _, h) ->
          Node.export ?clear (Some repo) n h;
          k ()
      | Node.Value (Some _, v, None) ->
          let h = P.Node.Key.hash v in
          Node.export ?clear (Some repo) n h;
          k ()
      | Node.Map _ | Node.Value (Some _, _, Some _) -> (
          skip n >>= function
          | true -> k ()
          | false ->
              let new_children_seq =
                let seq =
                  match n.Node.v with
                  | Node.Value (_, _, Some m) ->
                      StepMap.to_seq m
                      |> Seq.filter_map (function
                           | step, Node.Add v -> Some (step, v)
                           | _, Remove -> None)
                  | Node.Map m -> StepMap.to_seq m
                  | _ -> assert false
                in
                Seq.map (fun (_, x) -> x) seq
              in
              on_node_seq new_children_seq @@ fun () ->
              let* v = Node.to_value ~cache n in
              let v = get_ok "export" v in
              cnt.node_add <- cnt.node_add + 1;
              let* key = P.Node.add node_t v in
              let () =
                (* Sanity check: Did we just store the same hash as the one represented
                   by the Tree.Node [n]? *)
                match Node.cached_hash n with
                | None ->
                    (* No hash is in [n]. Computing it would result in getting it from
                       [v] or rebuilding a private node. *)
                    ()
                | Some key' -> assert (equal_hash key key')
              in

              Node.export ?clear (Some repo) n key;
              k ())
    and on_contents (`Contents (c, _)) k =
      match c.Contents.v with
      | Contents.Hash (None, h) -> raise_pruned "export.contents" h
      | Contents.Hash (Some repo, key) ->
          Contents.export ?clear repo c key;
          k ()
      | Contents.Value _ ->
          let* v = Contents.to_value ~cache c in
          let v = get_ok "export" v in
          let key = Contents.hash ~cache c in
          cnt.contents_add <- cnt.contents_add + 1;
          let* key' = P.Contents.add contents_t v in
          assert (equal_hash key key');
          Contents.export ?clear repo c key;
          k ()
    and on_node_seq seq k =
      match seq () with
      | Seq.Nil ->
          (* Have iterated on all children, let's export parent now *)
          k ()
      | Seq.Cons ((`Node _ as n), rest) ->
          on_node n (fun () -> on_node_seq rest k)
      | Seq.Cons ((`Contents _ as c), rest) ->
          on_contents c (fun () -> on_node_seq rest k)
    in
    let+ () = on_node (`Node n) (fun () -> Lwt.return_unit) in
    Node.hash ~cache n

  let merge : t Merge.t =
    let f ~old (x : t) y =
      Merge.(f Node.merge_elt) ~old x y >>= function
      | Ok t -> Merge.ok t
      | Error e -> Lwt.return (Error e)
    in
    Merge.v tree_t f

  let entries path tree =
    let rec aux acc = function
      | [] -> Lwt.return acc
      | (path, h) :: todo ->
          let* childs = Node.bindings ~cache:true h >|= get_ok "entries" in
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
    | Error (`Dangling_hash h1), Error (`Dangling_hash h2)
    | Error (`Pruned_hash h1), Error (`Dangling_hash h2)
    | Error (`Dangling_hash h1), Error (`Pruned_hash h2)
    | Error (`Pruned_hash h1), Error (`Pruned_hash h2) -> (
        match equal_hash h1 h2 with true -> empty | false -> assert false)
    | Error _, Ok _ -> assert false
    | Ok _, Error _ -> assert false
    | Ok x, Ok y -> diff_ok (x, y)

  let diff_contents x y =
    if Node.contents_equal x y then Lwt.return_nil
    else
      let* cx = Contents.to_value ~cache:true (fst x) in
      let+ cy = Contents.to_value ~cache:true (fst y) in
      diff_force_result cx cy ~empty:[] ~diff_ok:(fun (cx, cy) ->
          [ `Updated ((cx, snd x), (cy, snd y)) ])

  let compare_step = Type.(unstage (compare Path.step_t))

  let diff_node (x : node) (y : node) =
    let bindings n =
      Node.to_map ~cache:true n >|= function
      | Ok m -> Ok (StepMap.bindings m)
      | Error _ as e -> e
    in
    let removed acc (k, (c, m)) =
      let+ c = Contents.to_value ~cache:true c >|= get_ok "diff_node" in
      (k, `Removed (c, m)) :: acc
    in
    let added acc (k, (c, m)) =
      let+ c = Contents.to_value ~cache:true c >|= get_ok "diff_node" in
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
    | `Contents ((c1, m1) as x), `Contents ((c2, m2) as y) ->
        if contents_equal x y then Lwt.return_nil
        else
          let* c1 = Contents.to_value ~cache:true c1 >|= get_ok "diff" in
          let* c2 = Contents.to_value ~cache:true c2 >|= get_ok "diff" in
          Lwt.return [ (Path.empty, `Updated ((c1, m1), (c2, m2))) ]
    | `Node x, `Node y -> diff_node x y
    | `Contents (x, m), `Node y ->
        let* diff = diff_node (Node.empty ()) y in
        let+ x = Contents.to_value ~cache:true x >|= get_ok "diff" in
        (Path.empty, `Removed (x, m)) :: diff
    | `Node x, `Contents (y, m) ->
        let* diff = diff_node x (Node.empty ()) in
        let+ y = Contents.to_value ~cache:true y >|= get_ok "diff" in
        (Path.empty, `Added (y, m)) :: diff

  type concrete =
    [ `Tree of (Path.step * concrete) list
    | `Contents of P.Contents.Val.t * Metadata.t ]
  [@@deriving irmin]

  type 'a or_empty = Empty | Non_empty of 'a

  let of_concrete c =
    let rec concrete : type r. concrete -> (t or_empty, r) cont =
     fun t k ->
      match t with
      | `Contents (c, m) -> k (Non_empty (of_contents ~metadata:m c))
      | `Tree childs ->
          tree StepMap.empty childs (function
            | Empty -> k Empty
            | Non_empty n -> k (Non_empty (`Node n)))
    and tree :
        type r.
        Node.elt StepMap.t -> (step * concrete) list -> (node or_empty, r) cont
        =
     fun map t k ->
      match t with
      | [] ->
          k
            (if StepMap.is_empty map then Empty
            else Non_empty (Node.of_map ~env:(Env.empty ()) map))
      | (s, n) :: t ->
          (concrete [@tailcall]) n (fun v ->
              (tree [@tailcall])
                (StepMap.update s
                   (function
                     | None -> (
                         match v with
                         | Empty -> None (* Discard empty sub-directories *)
                         | Non_empty v -> Some v)
                     | Some _ ->
                         Fmt.invalid_arg
                           "of_concrete: duplicate bindings for step `%a`"
                           pp_step s)
                   map)
                t k)
    in
    (concrete [@tailcall]) c (function Empty -> empty () | Non_empty x -> x)

  let to_concrete t =
    let rec tree : type r. t -> (concrete, r) cont_lwt =
     fun t k ->
      match t with
      | `Contents c -> contents c k
      | `Node n ->
          let* m = Node.to_map ~cache:true n in
          let bindings = m |> get_ok "to_concrete" |> StepMap.bindings in
          (node [@tailcall]) [] bindings (fun n ->
              let n = List.sort (fun (s, _) (s', _) -> compare_step s s') n in
              k (`Tree n))
    and contents : type r. Contents.t * metadata -> (concrete, r) cont_lwt =
     fun (c, m) k ->
      let* c = Contents.to_value ~cache:true c >|= get_ok "to_concrete" in
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

  let hash ?(cache = true) (t : t) =
    Log.debug (fun l -> l "Tree.hash");
    match t with
    | `Node n -> `Node (Node.hash ~cache n)
    | `Contents (c, m) -> `Contents (Contents.hash ~cache c, m)

  let stats ?(force = false) (t : t) =
    let cache = true in
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
    fold ~force ~cache ~pre ~post ~contents t empty_stats

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

  module Proof = struct
    type irmin_tree = t

    include Proof.Make (P.Contents.Val) (P.Hash) (Path) (Metadata)

    type proof_tree = tree

    let bad_proof_exn c = Proof.bad_proof_exn ("Irmin.Tree." ^ c)

    type node_proof = P.Node.Val.proof
    (** The type of tree proofs. *)

    let value_of_hash ~cache:_ _node _repo h = Error (`Pruned_hash h)

    let to_value node =
      Node.to_value_aux ~cache:false ~value_of_hash ~return:Fun.id node

    let findv ctx node =
      let value_of_hash ~cache:_ _node _repo h = Error (`Pruned_hash h) in
      Node.findv_aux ~value_of_hash ~return:Fun.id
        ~bind:(fun x f -> f x)
        ~cache:false ctx node

    let rec proof_of_tree : type a. irmin_tree -> (proof_tree -> a) -> a =
     fun tree k ->
      match tree with
      | `Contents (c, h) -> proof_of_contents c h k
      | `Node node -> proof_of_node node k

    and proof_of_contents :
        type a. Contents.t -> metadata -> (proof_tree -> a) -> a =
     fun c m k ->
      match Contents.cached_value c with
      | Some v -> k (Contents (v, m))
      | None -> k (Blinded_contents (Contents.hash c, m))

    and proof_of_node : type a. node -> (proof_tree -> a) -> a =
     fun node k ->
      match to_value node with
      | Error (`Dangling_hash h) -> k (Blinded_node h)
      | Error (`Pruned_hash h) -> k (Blinded_node h)
      | Ok v -> proof_of_node_proof node (P.Node.Val.to_proof v) k

    (** [of_node_proof n np] is [p] (of type [Tree.Proof.t]) which is very
        similar to [np] (of type [P.Node.Val.proof]) except that the values
        loaded in [n] have been expanded.

        If [np] is of tag [Inode], [of_node_proof] will be called recursively
        for each of the proofs [np'] in [p.proofs], using [n] again (i.e.
        [of_node_proof n np']).

        If [np] is of tag [Values], the proofs contained in it will be ignored
        and instead be recomputed one value at a time, using the tag [Blinded]
        for the values non-loaded in [n], and some other tag for the values
        loaded in [n]. *)
    and proof_of_node_proof :
        type a. node -> node_proof -> (proof_tree -> a) -> a =
     fun node p k ->
      match p with
      | `Blinded h -> k (Blinded_node h)
      | `Inode (length, proofs) -> proof_of_inode node length proofs k
      | `Values vs -> proof_of_values node vs k

    and proof_of_inode :
        type a. node -> int -> (int * node_proof) list -> (proof_tree -> a) -> a
        =
     fun node length proofs k ->
      let rec aux acc = function
        | [] -> k (Inode { length; proofs = List.rev acc })
        | (index, proof) :: rest ->
            proof_of_node_proof node proof (fun proof ->
                aux ((index, proof) :: acc) rest)
      in
      aux [] proofs

    and proof_of_values :
        type a. node -> (step * P.Node.Val.value) list -> (proof_tree -> a) -> a
        =
     fun node steps k ->
      let findv = findv "Proof.proof_of_values" node in
      let rec aux acc = function
        | [] -> k (Node (List.rev acc))
        | (step, _) :: rest -> (
            match findv step with
            | None -> assert false
            | Some t -> proof_of_tree t (fun p -> aux ((step, p) :: acc) rest))
      in
      aux [] steps

    let proof_steps acc p =
      let rec aux acc : proof_tree -> (step * proof_tree) list = function
        | Blinded_node _ | Blinded_contents _ | Contents _ -> List.rev acc
        | Inode { proofs; _ } ->
            List.fold_left (fun acc (_, p) -> aux acc p) acc proofs
        | Node vs -> List.rev_append vs acc
      in
      aux acc p

    let hash_of_node_proof ~env (p : node_proof) =
      match p with
      | `Blinded h -> h
      | _ -> (
          match P.Node.Val.of_proof p with
          | None -> bad_proof_exn "hash_of_node_proof"
          | Some v ->
              let h = P.Node.Key.hash v in
              let _ = Env.add_node env h v in
              h)

    let of_tree t = proof_of_tree t Fun.id

    let rec tree_of_proof :
        type a. env:_ -> proof_tree -> (irmin_tree -> a) -> a =
     fun ~env p k ->
      match p with
      | Blinded_node h -> k (`Node (Node.of_hash ~env None h))
      | Node n -> tree_of_node ~env n k
      | Inode { length; proofs } -> tree_of_inode ~env length proofs k
      | Blinded_contents (c, h) ->
          k (`Contents (Contents.of_hash ~env None c, h))
      | Contents (c, m) ->
          let hash = P.Contents.Key.hash c in
          Env.add_contents env hash c;
          k (`Contents (Contents.of_value ~hash ~env c, m))

    and tree_of_node :
        type a. env:_ -> (step * proof_tree) list -> (irmin_tree -> a) -> a =
     fun ~env n k ->
      let rec aux acc = function
        | [] -> k (`Node (Node.of_map ~env acc))
        | (s, p) :: rest ->
            tree_of_proof ~env p (fun n -> aux (StepMap.add s n acc) rest)
      in
      aux StepMap.empty n

    (** [tree_of_inode] is solely called on the root of an inode tree *)
    and tree_of_inode :
        type a.
        env:_ -> int -> (int * proof_tree) list -> (irmin_tree -> a) -> a =
     fun ~env len proofs k ->
      let rev_proof_steps =
        (* Recursively blow up the [Inode] level(s) and compute a list of values
           in the [Node]s found. *)
        List.fold_left (fun acc (_, s) -> proof_steps acc s) [] proofs
      in
      let rec rev_elts acc proofs k =
        match proofs with
        | [] -> k acc
        | (s, p) :: rest ->
            tree_of_proof ~env p (fun p -> rev_elts ((s, p) :: acc) rest k)
      in
      rev_elts [] rev_proof_steps @@ fun elts ->
      let k n =
        List.iter (fun (s, elt) -> Node.add_to_findv_cache n s elt) elts;
        k (`Node n)
      in
      (* We have a partial proof, build a [node_proof] and then a private
         node from it. *)
      let rec aux acc proofs k =
        match proofs with
        | [] -> k (List.rev acc)
        | (i, p) :: rest ->
            node_proof_of_proof ~env p (fun p -> aux ((i, p) :: acc) rest k)
      in
      aux [] proofs @@ fun p ->
      let p = `Inode (len, p) in
      match P.Node.Val.of_proof p with
      | None -> bad_proof_exn "tree_of_inode"
      | Some n -> k (Node.of_value ~env None n)

    and node_proof_of_proof :
        type a. env:_ -> proof_tree -> (node_proof -> a) -> a =
     fun ~env t k ->
      match t with
      | Blinded_contents _ ->
          bad_proof_exn
            "Proof.to_node_proof: found Blinded_contents inside an inode"
      | Contents _ ->
          bad_proof_exn "Proof.to_node_proof: found Contents inside an inode"
      | Blinded_node x -> k (`Blinded x)
      | Inode { length; proofs } -> node_proof_of_inode ~env length proofs k
      | Node n -> node_proof_of_node ~env n k

    and node_proof_of_inode :
        type a.
        env:_ -> int -> (int * proof_tree) list -> (node_proof -> a) -> a =
     fun ~env length proofs k ->
      let rec aux acc = function
        | [] -> k (`Inode (length, List.rev acc))
        | (i, p) :: rest ->
            node_proof_of_proof ~env p (fun p -> aux ((i, p) :: acc) rest)
      in
      aux [] proofs

    and node_proof_of_node :
        type a. env:_ -> (step * proof_tree) list -> (node_proof -> a) -> a =
     fun ~env node k ->
      let rec aux acc = function
        | [] -> k (`Values (List.rev acc))
        | (s, n) :: rest ->
            node_value_of_proof ~env n (fun n -> aux ((s, n) :: acc) rest)
      in
      aux [] node

    and node_value_of_proof :
        type a. env:_ -> proof_tree -> (P.Node.Val.value -> a) -> a =
     fun ~env t k ->
      match t with
      | Blinded_contents (h, m) -> k (`Contents (h, m))
      | Contents (c, m) ->
          let h = P.Contents.Key.hash c in
          Env.add_contents env h c;
          k (`Contents (h, m))
      | t ->
          node_proof_of_proof ~env t (fun p ->
              let h = hash_of_node_proof ~env p in
              k (`Node h))

    let to_tree t =
      Env.track_reads_as_sets Consume @@ fun env ->
      tree_of_proof (state t) Fun.id ~env
  end

  let produce_proof repo kinded_hash f =
    Env.track_reads_as_sets_lwt Produce @@ fun env ->
    let tree = import_with_env ~env repo kinded_hash in
    let+ tree_after = f tree in
    (* Here, we build a proof from [tree] (on not from [tree_after]!)
       on purpose: we look at the effect on [f] on [tree]'s caches and
       we rely on the fact that the caches are env across
       copy-on-write copies of [tree]. *)
    let proof = Proof.of_tree tree in
    let after = hash tree_after in
    (* [tree_after] and [env] are dead now, so should avoid any
       memory leaks *)
    Proof.v ~before:kinded_hash ~after proof

  let verify_proof p f =
    Env.track_reads_as_sets Consume @@ fun env ->
    let before = Proof.before p in
    let after = Proof.after p in
    let tree = Proof.tree_of_proof (Proof.state p) Fun.id ~env in
    (* first check that [before] corresponds to [tree]'s hash. *)
    if not (equal_kinded_hash before (hash ~cache:false tree)) then
      Proof.bad_proof_exn "verify_proof: invalid before hash";
    Lwt.catch
      (fun () ->
        let+ tree_after = f tree in
        (* then check that [after] corresponds to [tree_after]'s hash. *)
        if not (equal_kinded_hash after (hash tree_after)) then
          Proof.bad_proof_exn "verify_proof: invalid before hash";
        tree_after)
      (function
        | Pruned_hash h ->
            (* finaly check that [f] only access valid parts of the proof. *)
            Fmt.kstr Proof.bad_proof_exn
              "verify_proof: %s is trying to read through a blinded node (%a)"
              h.context pp_hash h.hash
        | e -> raise e)
end
