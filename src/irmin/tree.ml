(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

exception Backend_invariant_violation of string
exception Assertion_failure of string

let backend_invariant_violation fmt =
  Fmt.kstr (fun s -> raise (Backend_invariant_violation s)) fmt

let assertion_failure fmt = Fmt.kstr (fun s -> raise (Assertion_failure s)) fmt

module Make (P : Backend.S) = struct
  type counters = {
    mutable contents_hash : int;
    mutable contents_find : int;
    mutable contents_add : int;
    mutable node_hash : int;
    mutable node_mem : int;
    mutable node_index : int;
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
      node_index = 0;
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
    t.node_index <- 0;
    t.node_add <- 0;
    t.node_find <- 0;
    t.node_val_v <- 0;
    t.node_val_find <- 0;
    t.node_val_list <- 0

  let cnt = fresh_counters ()

  module Path = struct
    include P.Node.Path

    let fold_right t ~f ~init =
      let steps = map t Fun.id in
      List.fold_right f steps init
  end

  module Metadata = P.Node.Metadata

  module Hashes = Hashtbl.Make (struct
    type t = P.Hash.t

    let hash x = P.Hash.short_hash x
    let equal = Type.(unstage (equal P.Hash.t))
  end)

  module StepMap = struct
    module X = struct
      type t = Path.step [@@deriving irmin ~compare]
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

  type metadata = Metadata.t [@@deriving irmin ~equal]
  type key = Path.t [@@deriving irmin ~pp]
  type hash = P.Hash.t [@@deriving irmin ~pp ~equal ~compare]
  type step = Path.step [@@deriving irmin ~pp ~compare]
  type contents = P.Contents.Val.t [@@deriving irmin ~equal ~pp]
  type repo = P.Repo.t
  type marks = unit Hashes.t
  type 'a or_error = ('a, [ `Dangling_hash of hash ]) result
  type 'a force = [ `True | `False of key -> 'a -> 'a Lwt.t ]
  type uniq = [ `False | `True | `Marks of marks ]
  type 'a node_fn = key -> step list -> 'a -> 'a Lwt.t

  type depth = [ `Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int ]
  [@@deriving irmin]

  let dummy_marks = Hashes.create 0
  let empty_marks () = Hashes.create 39

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

  let pruned_hash_exn context hash = raise (Pruned_hash { context; hash })

  let get_ok : type a. string -> a or_error -> a =
   fun context -> function
    | Ok x -> x
    | Error (`Dangling_hash hash) -> raise (Dangling_hash { context; hash })

  type 'key ptr_option = Key of 'key | Hash of hash | Ptr_none
  (* NOTE: given the choice, we prefer caching [Key] over [Hash] as it can
     be used to avoid storing duplicate contents values on export. *)

  module Contents = struct
    type key = P.Contents.Key.t [@@deriving irmin]
    type v = Key of repo * key | Value of contents | Pruned of hash
    type nonrec ptr_option = key ptr_option
    type info = { mutable ptr : ptr_option; mutable value : contents option }
    type t = { mutable v : v; info : info }

    let info_is_empty i = i.ptr = Ptr_none && i.value = None

    let v =
      let open Type in
      variant "Node.Contents.v" (fun key value pruned (v : v) ->
          match v with
          | Key (_, x) -> key x
          | Value v -> value v
          | Pruned h -> pruned h)
      |~ case1 "key" P.Contents.Key.t (fun _ -> assert false)
      |~ case1 "value" P.Contents.Val.t (fun v -> Value v)
      |~ case1 "pruned" hash_t (fun h -> Pruned h)
      |> sealv

    let clear_info i =
      if not (info_is_empty i) then (
        i.value <- None;
        i.ptr <- Ptr_none)

    let clear t = clear_info t.info

    let of_v (v : v) =
      let ptr, value =
        match v with
        | Key (_, k) -> ((Key k : ptr_option), None)
        | Value v -> (Ptr_none, Some v)
        | Pruned _ -> (Ptr_none, None)
      in
      let info = { ptr; value } in
      { v; info }

    let export ?clear:(c = true) repo t k =
      let ptr = t.info.ptr in
      if c then clear t;
      match (t.v, ptr) with
      | Key (repo', _), (Ptr_none | Hash _) ->
          if repo != repo' then t.v <- Key (repo, k)
      | Key (repo', _), Key k -> if repo != repo' then t.v <- Key (repo, k)
      | Value _, (Ptr_none | Hash _) -> t.v <- Key (repo, k)
      | Value _, Key k -> t.v <- Key (repo, k)
      | Pruned _, _ ->
          (* The main export function never exports a pruned position. *)
          assert false

    let of_value c = of_v (Value c)
    let of_key repo k = of_v (Key (repo, k))
    let pruned h = of_v (Pruned h)

    let cached_hash t =
      match (t.v, t.info.ptr) with
      | Pruned h, _ -> Some h
      | Key (_, k), (Ptr_none | Hash _) ->
          let h = Some (P.Contents.Key.to_hash k) in
          t.info.ptr <- Key k;
          h
      | _, Key k -> Some (P.Contents.Key.to_hash k)
      | _, Hash h -> Some h
      | _, Ptr_none -> None

    let cached_key t =
      match (t.v, t.info.ptr) with
      | Pruned _, _ -> None
      | Key (_, k), (Hash _ | Ptr_none) ->
          t.info.ptr <- Key k;
          Some k
      | _, Key k -> Some k
      | Value _, (Hash _ | Ptr_none) -> None

    let cached_value t =
      match (t.v, t.info.value) with
      | Value v, None ->
          let v = Some v in
          t.info.value <- v;
          v
      | _, v -> v

    let hash ?(cache = true) c =
      match cached_hash c with
      | Some k -> k
      | None -> (
          match cached_value c with
          | None -> assert false
          | Some v ->
              cnt.contents_hash <- cnt.contents_hash + 1;
              let h = P.Contents.Hash.hash v in
              assert (c.info.ptr = Ptr_none);
              if cache then c.info.ptr <- Hash h;
              h)

    let key t =
      match t.v with Key (_, k) -> Some k | Value _ | Pruned _ -> None

    let value_of_key ~cache t repo k =
      cnt.contents_find <- cnt.contents_find + 1;
      P.Contents.find (P.Repo.contents_t repo) k >|= function
      | None -> Error (`Dangling_hash (P.Contents.Key.to_hash k))
      | Some v as some_v ->
          if cache then t.info.value <- some_v;
          Ok v

    let to_value ~cache t =
      match cached_value t with
      | Some v -> Lwt.return (Ok v)
      | None -> (
          match t.v with
          | Value v -> Lwt.return (Ok v)
          | Key (repo, k) -> value_of_key ~cache t repo k
          | Pruned h -> pruned_hash_exn "Contents.to_value" h)

    let force = to_value ~cache:true

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
          | _ -> equal_hash (hash ~cache:true x) (hash ~cache:true y))

    let compare (x : t) (y : t) =
      if x == y then 0
      else compare_hash (hash ~cache:true x) (hash ~cache:true y)

    let t = Type.map ~equal ~compare v of_v (fun t -> t.v)

    let merge : t Merge.t =
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              let+ c = to_value ~cache:true old >|= Option.of_result in
              Ok (Some c))
        in
        let* x = to_value ~cache:true x >|= Option.of_result in
        let* y = to_value ~cache:true y >|= Option.of_result in
        Merge.(f P.Contents.Val.merge) ~old x y >|= function
        | Ok (Some c) -> Ok (of_value c)
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
    type value = P.Node.Val.t [@@deriving irmin ~equal ~pp]
    type key = P.Node.Key.t [@@deriving irmin]
    type nonrec ptr_option = key ptr_option

    type elt = [ `Node of t | `Contents of Contents.t * Metadata.t ]

    and update = Add of elt | Remove

    and updatemap = update StepMap.t

    and map = elt StepMap.t

    and info = {
      mutable value : value option;
      mutable map : map option;
      mutable ptr : ptr_option;
      mutable findv_cache : map option;
    }

    and v =
      | Map of map
      | Key of repo * key
      | Value of repo * value * updatemap option
      | Pruned of hash

    and t = { mutable v : v; info : info }
    (** [t.v] has 3 possible states:

        - A [Map], only after a [Tree.of_concrete] operation.
        - A [Value], only after an add, a remove, temporarily during an export
          or at the end of a merge.
        - It is otherwise a [Key].

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
      variant "Node.node" (fun map key value pruned -> function
        | Map m -> map m
        | Key (_, y) -> key y
        | Value (_, v, m) -> value (v, m)
        | Pruned h -> pruned h)
      |~ case1 "map" m (fun m -> Map m)
      |~ case1 "key" P.Node.Key.t (fun _ -> assert false)
      |~ case1 "value" (pair P.Node.Val.t (option um)) (fun _ -> assert false)
      |~ case1 "pruned" hash_t (fun h -> Pruned h)
      |> sealv

    let of_v v =
      let ptr, map, value =
        match v with
        | Map m -> (Ptr_none, Some m, None)
        | Key (_, k) -> (Key k, None, None)
        | Value (_, v, None) -> (Ptr_none, None, Some v)
        | Value _ | Pruned _ -> (Ptr_none, None, None)
      in
      let findv_cache = None in
      let info = { ptr; map; value; findv_cache } in
      { v; info }

    let of_map m = of_v (Map m)
    let of_key repo k = of_v (Key (repo, k))
    let of_value ?updates repo v = of_v (Value (repo, v, updates))
    let pruned h = of_v (Pruned h)

    let info_is_empty i =
      i.map = None && i.value = None && i.findv_cache = None && i.ptr = Ptr_none

    let clear_info_fields i =
      if not (info_is_empty i) then (
        i.value <- None;
        i.map <- None;
        i.ptr <- Ptr_none;
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
      let ptr = t.info.ptr in
      if c then clear_info_fields t.info;
      match t.v with
      | Key (repo', k) -> if repo != repo' then t.v <- Key (repo, k)
      | Value _ | Map _ -> (
          match ptr with
          | Ptr_none | Hash _ -> t.v <- Key (repo, k)
          | Key k -> t.v <- Key (repo, k))
      | Pruned _ ->
          (* The main export function never exports a pruned position. *)
          assert false

    let map_of_value ~cache repo (n : value) : map =
      cnt.node_val_list <- cnt.node_val_list + 1;
      let entries = P.Node.Val.seq ~cache n in
      let aux = function
        | `Node h -> `Node (of_key repo h)
        | `Contents (c, m) -> `Contents (Contents.of_key repo c, m)
      in
      Seq.fold_left
        (fun acc (k, v) -> StepMap.add k (aux v) acc)
        StepMap.empty entries

    let cached_hash t =
      match (t.v, t.info.ptr) with
      | Pruned h, _ -> Some h
      | Key (_, k), Ptr_none ->
          let h = Some (P.Node.Key.to_hash k) in
          t.info.ptr <- Key k;
          h
      | _, Key k -> Some (P.Node.Key.to_hash k)
      | _, Hash h -> Some h
      | _, Ptr_none -> None

    let cached_key t =
      match (t.v, t.info.ptr) with
      | Pruned _, _ -> None
      | Key (_, k), (Hash _ | Ptr_none) ->
          t.info.ptr <- Key k;
          Some k
      | _, Key k -> Some k
      | (Map _ | Value _), (Hash _ | Ptr_none) -> None

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

    let key t = match t.v with Key (_, k) -> Some k | _ -> None
    let unsafe_cache_key t k = t.info.ptr <- Key k

    open struct
      module Portable = P.Node_portable
      module Portable_hash = Hash.Typed (P.Hash) (P.Node_portable)
    end

    (* When computing hashes of nodes, we try to use [P.Node.Val.t] as a
       pre-image if possible so that this intermediate value can be cached
       within [t.info.value] (in case it is about to be written to the backend).

       This is only possible if all of the child pointers have pre-existing
       keys, otherwise we must convert to portable nodes as a fallback. *)
    type hash_preimage = Node of P.Node.Val.t | Pnode of Portable.t
    type node_value = P.Node.Val.value
    type pnode_value = Portable.value

    type hash_preimage_value =
      | Node_value of node_value
      | Pnode_value of pnode_value

    let weaken_value : node_value -> pnode_value = function
      | `Contents (key, m) -> `Contents (P.Contents.Key.to_hash key, m)
      | `Node key -> `Node (P.Node.Key.to_hash key)

    let rec hash : type a. cache:bool -> t -> (hash -> a) -> a =
     fun ~cache t k ->
      match cached_hash t with
      | Some h -> k h
      | None -> (
          let a_of_hashable hash v =
            cnt.node_hash <- cnt.node_hash + 1;
            let hash = hash v in
            assert (t.info.ptr = Ptr_none);
            if cache then t.info.ptr <- Hash hash;
            k hash
          in
          match cached_value t with
          | Some v -> a_of_hashable P.Node.Hash.hash v
          | None -> (
              match t.v with
              | Pruned h -> k h
              | Key (_, h) -> k (P.Node.Key.to_hash h)
              | Value (_, v, None) -> a_of_hashable P.Node.Hash.hash v
              | Value (_, v, Some um) ->
                  hash_preimage_of_updates ~cache t v um (function
                    | Node x -> a_of_hashable P.Node.Hash.hash x
                    | Pnode x -> a_of_hashable Portable_hash.hash x)
              | Map m ->
                  hash_preimage_of_map ~cache t m (function
                    | Node x -> a_of_hashable P.Node.Hash.hash x
                    | Pnode x -> a_of_hashable Portable_hash.hash x)))

    and hash_preimage_of_map :
        type r. cache:bool -> t -> map -> (hash_preimage, r) cont =
     fun ~cache t map k ->
      cnt.node_val_v <- cnt.node_val_v + 1;
      let bindings = StepMap.to_seq map in
      let must_build_portable_node =
        bindings
        |> Seq.exists (fun (_, v) ->
               match v with
               | `Node n -> Option.is_none (cached_key n)
               | `Contents (c, _) -> Option.is_none (Contents.cached_key c))
      in
      if must_build_portable_node then
        let pnode =
          bindings
          |> Seq.map (fun (step, v) ->
                 match v with
                 | `Contents (c, m) ->
                     let hash =
                       match Contents.key c with
                       | Some key -> P.Contents.Key.to_hash key
                       | None -> Contents.hash c
                     in
                     (step, `Contents (hash, m))
                 | `Node n -> (
                     match key n with
                     | Some key -> (step, `Node (P.Node.Key.to_hash key))
                     | None -> hash ~cache n (fun k -> (step, `Node k))))
          |> Portable.of_seq
        in
        k (Pnode pnode)
      else
        let node =
          bindings
          |> Seq.map (fun (step, v) ->
                 match v with
                 | `Contents (c, m) -> (
                     match Contents.cached_key c with
                     | Some k -> (step, `Contents (k, m))
                     | None ->
                         (* We checked that all child keys are cached above *)
                         assert false)
                 | `Node n -> (
                     match cached_key n with
                     | Some k -> (step, `Node k)
                     | None ->
                         (* We checked that all child keys are cached above *)
                         assert false))
          |> P.Node.Val.of_seq
        in
        t.info.value <- Some node;
        k (Node node)

    and hash_preimage_value_of_elt :
        type r. cache:bool -> elt -> (hash_preimage_value, r) cont =
     fun ~cache e k ->
      match e with
      | `Contents (c, m) -> (
          match Contents.key c with
          | Some key -> k (Node_value (`Contents (key, m)))
          | None -> k (Pnode_value (`Contents (Contents.hash c, m))))
      | `Node n -> (
          match key n with
          | Some key -> k (Node_value (`Node key))
          | None -> hash ~cache n (fun hash -> k (Pnode_value (`Node hash))))

    and hash_preimage_of_updates :
        type r. cache:bool -> t -> value -> updatemap -> (hash_preimage, r) cont
        =
     fun ~cache _t v updates k ->
      let updates = StepMap.bindings updates in
      let rec aux acc = function
        | [] -> k acc
        | (k, Add e) :: rest ->
            hash_preimage_value_of_elt ~cache e (fun e ->
                let acc =
                  match (acc, e) with
                  | Node n, Node_value v -> Node (P.Node.Val.add n k v)
                  | Node n, Pnode_value v ->
                      Pnode (Portable.add (Portable.of_node n) k v)
                  | Pnode n, Node_value v ->
                      Pnode (Portable.add n k (weaken_value v))
                  | Pnode n, Pnode_value v -> Pnode (Portable.add n k v)
                in
                aux acc rest)
        | (k, Remove) :: rest ->
            let acc =
              match acc with
              | Node n -> Node (P.Node.Val.remove n k)
              | Pnode n -> Pnode (Portable.remove n k)
            in
            aux acc rest
      in
      aux (Pnode (P.Node_portable.of_node v)) updates

    let hash ~cache k = hash ~cache k (fun x -> x)

    let value_of_key ~cache t repo k =
      match cached_value t with
      | Some v -> Lwt.return_ok v
      | None -> (
          cnt.node_find <- cnt.node_find + 1;
          P.Node.find (P.Repo.node_t repo) k >|= function
          | None -> Error (`Dangling_hash (P.Node.Key.to_hash k))
          | Some v as some_v ->
              if cache then t.info.value <- some_v;
              Ok v)

    let to_value ~cache t =
      match cached_value t with
      | Some v -> ok v
      | None -> (
          match t.v with
          | Value (_, v, None) -> ok v
          | Key (repo, h) -> value_of_key ~cache t repo h
          | Pruned h -> pruned_hash_exn "Node.to_value" h
          | Value (_, _, Some _) | Map _ ->
              invalid_arg
                "Tree.Node.to_value: the supplied node has not been written to \
                 disk. Either export it or convert it to a portable value \
                 instead.")

    let to_portable_value ~cache t =
      match cached_value t with
      | Some v -> ok (P.Node_portable.of_node v)
      | None -> (
          match t.v with
          | Value (_, v, None) -> ok (P.Node_portable.of_node v)
          | Value (_, v, Some um) ->
              hash_preimage_of_updates ~cache t v um (function
                | Node x -> ok (Portable.of_node x)
                | Pnode x -> ok x)
          | Map m ->
              hash_preimage_of_map ~cache t m (function
                | Node x -> ok (Portable.of_node x)
                | Pnode x -> ok x)
          | Key (repo, h) ->
              value_of_key ~cache t repo h
              |> Lwt_result.map P.Node_portable.of_node
          | Pruned h -> pruned_hash_exn "Node.to_portable_value" h)

    let to_map ~cache t =
      match cached_map t with
      | Some m -> Lwt.return (Ok m)
      | None -> (
          let of_value repo v updates =
            let m = map_of_value ~cache repo v in
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
          | Key (repo, k) -> (
              value_of_key ~cache t repo k >|= function
              | Error _ as e -> e
              | Ok v -> Ok (of_value repo v None))
          | Pruned h -> pruned_hash_exn "Node.to_map" h)

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
          | Some x, Some y -> equal_value x y
          | _ -> (
              match (cached_map x, cached_map y) with
              | Some x, Some y -> map_equal x y
              | _ -> equal_hash (hash ~cache:true x) (hash ~cache:true y)))

    (* same as [equal] but do not compare in-memory maps
       recursively. *)
    let maybe_equal (x : t) (y : t) =
      if x == y then True
      else
        match (cached_hash x, cached_hash y) with
        | Some x, Some y -> if equal_hash x y then True else False
        | _ -> (
            match (cached_value x, cached_value y) with
            | Some x, Some y -> if equal_value x y then True else False
            | _ -> Maybe)

    let empty () = of_map StepMap.empty
    let empty_hash = hash ~cache:false (empty ())
    let singleton k v = of_map (StepMap.singleton k v)

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
      | None -> (
          match cached_value t with
          | Some v -> P.Node.Val.length v |> Lwt.return
          | None -> (
              match t.v with
              | Map m -> StepMap.cardinal m |> Lwt.return
              | Key (r, k) ->
                  value_of_key ~cache t r k
                  >|= get_ok "length"
                  >|= P.Node.Val.length
              | Value (_, v, None) -> P.Node.Val.length v |> Lwt.return
              | Value (_, v, Some u) ->
                  hash_preimage_of_updates ~cache t v u (function
                    | Node x -> P.Node.Val.length x |> Lwt.return
                    | Pnode x -> P.Node_portable.length x |> Lwt.return)
              | Pruned h -> pruned_hash_exn "length" h))

    let is_empty ~cache t =
      match cached_map t with
      | Some m -> StepMap.is_empty m
      | None -> (
          match cached_value t with
          | Some v -> P.Node.Val.is_empty v
          | None -> (
              match t.v with
              | Value (_, v, Some um) -> is_empty_after_updates ~cache v um
              | Key (_, key) -> equal_hash (P.Node.Key.to_hash key) empty_hash
              | Pruned h -> equal_hash h empty_hash
              | Map _ -> assert false (* [cached_map <> None] *)
              | Value (_, _, None) -> assert false (* [cached_value <> None] *))
          )

    let add_to_findv_cache t step v =
      match t.info.findv_cache with
      | None -> t.info.findv_cache <- Some (StepMap.singleton step v)
      | Some m -> t.info.findv_cache <- Some (StepMap.add step v m)

    let findv ~cache ctx t step =
      let of_map m = try Some (StepMap.find step m) with Not_found -> None in
      let of_value repo v =
        match P.Node.Val.find ~cache v step with
        | None -> None
        | Some (`Contents (c, m)) ->
            let c = Contents.of_key repo c in
            let (v : elt) = `Contents (c, m) in
            if cache then add_to_findv_cache t step v;
            Some v
        | Some (`Node n) ->
            let n = of_key repo n in
            let v = `Node n in
            if cache then add_to_findv_cache t step v;
            Some v
      in
      let of_t () =
        match t.v with
        | Map m -> Lwt.return (of_map m)
        | Value (repo, v, None) -> Lwt.return (of_value repo v)
        | Value (repo, v, Some um) -> (
            match StepMap.find_opt step um with
            | Some (Add v) -> Lwt.return (Some v)
            | Some Remove -> Lwt.return None
            | None -> Lwt.return (of_value repo v))
        | Key (repo, h) -> (
            match cached_value t with
            | Some v -> Lwt.return (of_value repo v)
            | None ->
                let+ v = value_of_key ~cache t repo h >|= get_ok ctx in
                of_value repo v)
        | Pruned h -> pruned_hash_exn "Node.find" h
      in
      match cached_map t with
      | Some m -> Lwt.return (of_map m)
      | None -> (
          match t.info.findv_cache with
          | None -> of_t ()
          | Some m -> (
              match of_map m with
              | None -> of_t ()
              | Some _ as r -> Lwt.return r))

    let seq_of_map ?(offset = 0) ?length m : (step * elt) Seq.t =
      let take seq =
        match length with None -> seq | Some n -> Seq.take n seq
      in
      StepMap.to_seq m |> Seq.drop offset |> take

    let seq_of_value repo ?offset ?length ~cache v : (step * elt) Seq.t =
      cnt.node_val_list <- cnt.node_val_list + 1;
      let seq = P.Node.Val.seq ?offset ?length ~cache v in
      Seq.map
        (fun (k, v) ->
          match v with
          | `Node n ->
              let n = `Node (of_key repo n) in
              (k, n)
          | `Contents (c, m) ->
              let c = Contents.of_key repo c in
              (k, `Contents (c, m)))
        seq

    let seq ?offset ?length ~cache t : (step * elt) Seq.t or_error Lwt.t =
      match cached_map t with
      | Some m -> ok (seq_of_map ?offset ?length m)
      | None -> (
          match t.v with
          | Value (repo, n, None) ->
              ok (seq_of_value ?offset ?length ~cache repo n)
          | Key (repo, h) -> (
              value_of_key ~cache t repo h >>= function
              | Error _ as e -> Lwt.return e
              | Ok v -> ok (seq_of_value ?offset ?length ~cache repo v))
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
      path:Path.t -> 'acc -> int -> 'v -> ('acc, 'r) cont_lwt
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
        node:(Path.t -> _ -> acc -> acc Lwt.t) ->
        contents:(Path.t -> contents -> acc -> acc Lwt.t) ->
        tree:(Path.t -> _ -> acc -> acc Lwt.t) ->
        t ->
        acc ->
        acc Lwt.t =
     fun ~order ~force ~cache ~uniq ~pre ~post ~path ?depth ~node ~contents
         ~tree t acc ->
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
              | `Undefined, Key (repo, _) ->
                  let* v = to_value ~cache t >|= get_ok "fold" in
                  (value [@tailcall]) ~path acc d (repo, v, None) k
              | `Undefined, Pruned h -> pruned_hash_exn "fold" h)
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
      and value : type r. (repo * value * updatemap option, acc, r) folder =
       fun ~path acc d (repo, v, updates) k ->
        let to_elt = function
          | `Node n -> `Node (of_key repo n)
          | `Contents (c, m) -> `Contents (Contents.of_key repo c, m)
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
      | Map m -> Lwt.return (of_map m)
      | Value (repo, n, None) -> Lwt.return (of_value repo n StepMap.empty)
      | Value (repo, n, Some um) -> Lwt.return (of_value repo n um)
      | Key (repo, h) -> (
          match (cached_value t, cached_map t) with
          | Some v, _ -> Lwt.return (of_value repo v StepMap.empty)
          | _, Some m -> Lwt.return (of_map m)
          | None, None ->
              let+ v = value_of_key ~cache:true t repo h >|= get_ok "update" in
              of_value repo v StepMap.empty)
      | Pruned h -> pruned_hash_exn "update" h

    let remove t step = update t step Remove
    let add t step v = update t step (Add v)

    let compare (x : t) (y : t) =
      if x == y then 0
      else compare_hash (hash ~cache:true x) (hash ~cache:true y)

    let t node = Type.map ~equal ~compare node of_v (fun t -> t.v)

    let _, t =
      Type.mu2 (fun _ y ->
          let elt = elt_t y in
          let v = v_t elt in
          let t = t v in
          (v, t))

    let elt_t = elt_t t
    let dump = Type.pp_dump t

    let rec merge : type a. (t Merge.t -> a) -> a =
     fun k ->
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              let+ m = to_map ~cache:true old >|= Option.of_result in
              Ok (Some m))
        in
        let* x = to_map ~cache:true x >|= Option.of_result in
        let* y = to_map ~cache:true y >|= Option.of_result in
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

  type node = Node.t [@@deriving irmin ~pp]
  type node_key = Node.key [@@deriving irmin ~pp]
  type contents_key = Contents.key [@@deriving irmin ~pp]

  type kinded_key = [ `Contents of Contents.key * metadata | `Node of Node.key ]
  [@@deriving irmin]

  type kinded_hash = [ `Contents of hash * metadata | `Node of hash ]
  [@@deriving irmin]

  type t = [ `Node of node | `Contents of Contents.t * Metadata.t ]
  [@@deriving irmin]

  let to_backend_node n =
    Node.to_value ~cache:true n >|= get_ok "to_backend_node"

  let to_backend_portable_node n =
    Node.to_portable_value ~cache:true n >|= get_ok "to_backend_portable_node"

  let of_backend_node repo n = Node.of_value repo n

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
    `Contents (Contents.of_value c, metadata)

  let v : elt -> t = function
    | `Contents (c, meta) -> `Contents (Contents.of_value c, meta)
    | `Node n -> `Node n

  let pruned : kinded_hash -> t = function
    | `Contents (h, meta) -> `Contents (Contents.pruned h, meta)
    | `Node h -> `Node (Node.pruned h)

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
    [%log.debug "Tree.find_tree %a" pp_key path];
    match (t, Path.rdecons path) with
    | v, None -> Lwt.return_some v
    | _, Some (path, file) -> (
        sub ~cache "find_tree.sub" t path >>= function
        | None -> Lwt.return_none
        | Some n -> Node.findv ~cache "find_tree.findv" n file)

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
    Fmt.kstr invalid_arg "Irmin.Tree.%s: %a not found" n pp_key k

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
    [%log.debug "Tree.kind %a" pp_key path];
    match (t, Path.rdecons path) with
    | `Contents _, None -> Lwt.return_some `Contents
    | `Node _, None -> Lwt.return_some `Node
    | _, Some (dir, file) -> (
        sub ~cache "kind.sub" t dir >>= function
        | None -> Lwt.return_none
        | Some m -> (
            Node.findv ~cache "kind.findv" m file >>= function
            | None -> Lwt.return_none
            | Some (`Contents _) -> Lwt.return_some `Contents
            | Some (`Node _) -> Lwt.return_some `Node))

  let length = Node.length ~cache:true

  let seq t ?offset ?length ~cache path : (step * t) Seq.t Lwt.t =
    [%log.debug "Tree.seq %a" pp_key path];
    sub ~cache "seq.sub" t path >>= function
    | None -> Lwt.return Seq.empty
    | Some n -> Node.seq ?offset ?length ~cache n >|= get_ok "seq"

  let list t ?offset ?length ?(cache = true) path =
    seq t ?offset ?length ~cache path >|= List.of_seq

  let empty () = `Node (Node.empty ())

  let singleton k ?(metadata = Metadata.default) c =
    [%log.debug "Tree.singleton %a" pp_key k];
    let base_tree = `Contents (Contents.of_value c, metadata) in
    Path.fold_right k
      ~f:(fun step child -> `Node (Node.singleton step child))
      ~init:base_tree

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

  let update_tree ~cache ~f_might_return_empty_node ~f root_tree path =
    (* User-introduced empty nodes will be removed immediately if necessary. *)
    let prune_empty : node -> bool =
      if not f_might_return_empty_node then Fun.const false
      else Node.is_empty ~cache
    in
    match Path.rdecons path with
    | None -> (
        let empty_tree =
          match is_empty root_tree with
          | true -> root_tree
          | false -> `Node (Node.of_map StepMap.empty)
        in
        f (Some root_tree) >>= function
        (* Here we consider "deleting" a root contents value or node to consist
           of changing it to an empty node. Note that this introduces
           sensitivity to ordering of subtree operations: updating in a subtree
           and adding the subtree are not necessarily commutative. *)
        | None -> Lwt.return empty_tree
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
          let changed n = k (Changed n) in
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
        | Changed node -> Lwt.return (`Node node))

  let update t k ?(metadata = Metadata.default) f =
    let cache = true in
    [%log.debug "Tree.update %a" pp_key k];
    update_tree ~cache t k ~f_might_return_empty_node:false ~f:(fun t ->
        let+ old_contents =
          match t with
          | Some (`Node _) | None -> Lwt.return_none
          | Some (`Contents (c, _)) ->
              let+ c = Contents.to_value ~cache c in
              Some (get_ok "update" c)
        in
        match f old_contents with
        | None -> None
        | Some c -> Some (`Contents (Contents.of_value c, metadata)))

  let add t k ?(metadata = Metadata.default) c =
    [%log.debug "Tree.add %a" pp_key k];
    update_tree ~cache:true t k
      ~f:(fun _ -> Lwt.return_some (`Contents (Contents.of_value c, metadata)))
      ~f_might_return_empty_node:false

  let add_tree t k v =
    [%log.debug "Tree.add_tree %a" pp_key k];
    update_tree ~cache:true t k
      ~f:(fun _ -> Lwt.return_some v)
      ~f_might_return_empty_node:true

  let remove t k =
    [%log.debug "Tree.remove %a" pp_key k];
    update_tree ~cache:true t k
      ~f:(fun _ -> Lwt.return_none)
      ~f_might_return_empty_node:false

  let update_tree t k f =
    [%log.debug "Tree.update_tree %a" pp_key k];
    update_tree ~cache:true t k ~f:(Lwt.wrap1 f) ~f_might_return_empty_node:true

  let import repo = function
    | `Contents (k, m) -> (
        P.Contents.mem (P.Repo.contents_t repo) k >|= function
        | true -> Some (`Contents (Contents.of_key repo k, m))
        | false -> None)
    | `Node k -> (
        cnt.node_mem <- cnt.node_mem + 1;
        P.Node.mem (P.Repo.node_t repo) k >|= function
        | true -> Some (`Node (Node.of_key repo k))
        | false -> None)

  let import_no_check repo = function
    | `Node k -> `Node (Node.of_key repo k)
    | `Contents (k, m) -> `Contents (Contents.of_key repo k, m)

  (* Given an arbitrary tree value, persist its contents to the given contents
     and node stores via a depth-first {i post-order} traversal. We must export
     a node's children before the node itself in order to get the {i keys} of
     any un-persisted child values. *)
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
      match Node.cached_key n with
      | Some k ->
          cnt.node_mem <- cnt.node_mem + 1;
          P.Node.mem node_t k
      | None -> (
          (* XXX: should we compute the actual hash here if it's not cached? *)
          match Node.cached_hash n with
          | None -> Lwt.return_false
          | Some h -> (
              cnt.node_index <- cnt.node_index + 1;
              P.Node.index node_t h >>= function
              | None ->
                  (* NOTE: it's possible that this value already has a key in the
                     store, but it's not indexed. If so, we're adding a duplicate
                     here – this isn't an issue for correctness, but does waste
                     space. *)
                  Lwt.return_false
              | Some k ->
                  (* We always prefer caching keys over hashes. In this case,
                     caching the key ensures that any yet-to-be-exported parent
                     nodes can reference this value. *)
                  Node.unsafe_cache_key n k;
                  cnt.node_mem <- cnt.node_mem + 1;
                  P.Node.mem node_t k))
    in

    let add_node n v k =
      cnt.node_add <- cnt.node_add + 1;
      let* key = P.Node.add node_t v in
      let () =
        let h = P.Node.Key.to_hash key in
        let h' = Node.hash ~cache:true n in
        if not (equal_hash h h') then
          backend_invariant_violation
            "@[<v 2>Tree.export: added inconsistent node binding@,\
             key: %a@,\
             value: %a@,\
             computed hash: %a@]" pp_node_key key Node.pp_value v pp_hash h'
      in
      Node.export ?clear repo n key;
      k ()
    in

    let add_node_map n (x : Node.map) k =
      let node =
        (* Since we traverse in post-order, all children of [x] have already
           been added. Thus, their keys are cached and we can retrieve them. *)
        StepMap.to_seq x
        |> Seq.map (fun (step, v) ->
               match v with
               | `Node n -> (
                   match Node.cached_key n with
                   | Some k -> (step, `Node k)
                   | None ->
                       assertion_failure
                         "Encountered child node value with uncached key \
                          during export:@,\
                          @ @[%a@]" dump v)
               | `Contents (c, m) -> (
                   match Contents.cached_key c with
                   | Some k -> (step, `Contents (k, m))
                   | None ->
                       assertion_failure
                         "Encountered child contents value with uncached key \
                          during export:@,\
                          @ @[%a@]" dump v))
        |> P.Node.Val.of_seq
      in
      add_node n node k
    in

    let add_updated_node n (v : Node.value) (updates : Node.updatemap) k =
      let node =
        StepMap.fold
          (fun k v acc ->
            match v with
            | Node.Remove -> P.Node.Val.remove acc k
            | Node.Add (`Node n as v) -> (
                match Node.cached_key n with
                | Some ptr -> P.Node.Val.add acc k (`Node ptr)
                | None ->
                    assertion_failure
                      "Encountered child node value with uncached key during \
                       export:@,\
                       @ @[%a@]" dump v)
            | Add (`Contents (c, m) as v) -> (
                match Contents.cached_key c with
                | Some ptr -> P.Node.Val.add acc k (`Contents (ptr, m))
                | None ->
                    assertion_failure
                      "Encountered child contents value with uncached key \
                       during export:@,\
                       @ @[%a@]" dump v))
          updates v
      in
      add_node n node k
    in

    let rec on_node : type r. [ `Node of node ] -> (unit, r) cont_lwt =
     fun (`Node n) k ->
      (* Invariant: when [k] is called, [Node.cached_key n = Some _]. *)
      let k () =
        (match Node.cached_key n with
        | None ->
            assertion_failure "After trying to export %a, key isn't cached."
              dump (`Node n)
        | Some _ -> ());
        k ()
      in
      match n.Node.v with
      | Node.Key (_, key) ->
          Node.export ?clear repo n key;
          k ()
      | Pruned h -> pruned_hash_exn "export" h
      | _ -> (
          skip n >>= function
          | true -> k ()
          | false -> (
              let new_children_seq =
                let seq =
                  match n.Node.v with
                  | Value (_, _, Some m) ->
                      StepMap.to_seq m
                      |> Seq.filter_map (function
                           | step, Node.Add v -> Some (step, v)
                           | _, Remove -> None)
                  | Map m -> StepMap.to_seq m
                  | Value (_, _, None) -> Seq.empty
                  | Key _ | Pruned _ ->
                      (* [n.v = (Key _ | Pruned _)] is excluded above. *)
                      assert false
                in
                Seq.map (fun (_, x) -> x) seq
              in
              on_node_seq new_children_seq @@ fun () ->
              match (n.Node.v, Node.cached_value n) with
              | Map x, _ -> add_node_map n x k
              | Value (_, v, None), None | _, Some v -> add_node n v k
              | Value (_, v, Some um), _ -> add_updated_node n v um k
              | (Key _ | Pruned _), _ ->
                  (* [n.v = (Key _ | Pruned _)] is excluded above. *)
                  assert false))
    and on_contents :
        type r. [ `Contents of Contents.t * metadata ] -> (unit, r) cont_lwt =
     fun (`Contents (c, _)) k ->
      match c.Contents.v with
      | Contents.Key (_, key) ->
          Contents.export ?clear repo c key;
          k ()
      | Contents.Value _ ->
          let* v = Contents.to_value ~cache c in
          let v = get_ok "export" v in
          cnt.contents_add <- cnt.contents_add + 1;
          let* key = P.Contents.add contents_t v in
          let () =
            let h = P.Contents.Key.to_hash key in
            let h' = Contents.hash ~cache c in
            if not (equal_hash h h') then
              backend_invariant_violation
                "@[<v 2>Tree.export: added inconsistent contents binding@,\
                 key: %a@,\
                 value: %a@,\
                 computed hash: %a@]" pp_contents_key key pp_contents v pp_hash
                h'
          in
          Contents.export ?clear repo c key;
          k ()
      | Contents.Pruned h -> pruned_hash_exn "export" h
    and on_node_seq : type r. Node.elt Seq.t -> (unit, r) cont_lwt =
     fun seq k ->
      match seq () with
      | Seq.Nil ->
          (* Have iterated on all children, let's export parent now *)
          k ()
      | Seq.Cons ((`Node _ as n), rest) ->
          on_node n (fun () -> on_node_seq rest k)
      | Seq.Cons ((`Contents _ as c), rest) ->
          on_contents c (fun () -> on_node_seq rest k)
    in
    on_node (`Node n) (fun () ->
        match Node.cached_key n with
        | Some x -> Lwt.return x
        | None ->
            (* We just exported this node: it must have a key. *)
            assert false)

  let merge : t Merge.t =
    let f ~old (x : t) y =
      Merge.(f Node.merge_elt) ~old x y >>= function
      | Ok t -> Merge.ok t
      | Error e -> Lwt.return (Error e)
    in
    Merge.v t f

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
    | Error (`Dangling_hash h1), Error (`Dangling_hash h2) -> (
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
      | `Contents (c, m) -> k (Non_empty (`Contents (Contents.of_value c, m)))
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
            else Non_empty (Node.of_map map))
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

  let key (t : t) =
    [%log.debug "Tree.key"];
    match t with
    | `Node n -> (
        match Node.key n with Some key -> Some (`Node key) | None -> None)
    | `Contents (c, m) -> (
        match Contents.key c with
        | Some key -> Some (`Contents (key, m))
        | None -> None)

  let hash ?(cache = true) (t : t) =
    [%log.debug "Tree.hash"];
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
          | Key _ -> `Key
          | Pruned _ -> `Pruned)
end
