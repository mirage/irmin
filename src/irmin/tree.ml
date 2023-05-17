(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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
    mutable contents_mem : int;
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
      contents_mem = 0;
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
    t.contents_mem <- 0;
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
  module Irmin_proof = Proof
  module Tree_proof = Proof.Make (P.Contents.Val) (P.Hash) (Path) (Metadata)
  module Env = Proof.Env (P) (Tree_proof)

  let merge_env x y =
    match (Env.is_empty x, Env.is_empty y) with
    | true, _ -> Ok y
    | _, true -> Ok x
    | false, false -> Error (`Conflict "merge env")

  module Hashes = Hash.Set.Make (P.Hash)

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
  type path = Path.t [@@deriving irmin ~pp]
  type hash = P.Hash.t [@@deriving irmin ~pp ~equal ~compare]
  type step = Path.step [@@deriving irmin ~pp ~compare]
  type contents = P.Contents.Val.t [@@deriving irmin ~equal ~pp]
  type repo = P.Repo.t
  type marks = Hashes.t

  type error =
    [ `Dangling_hash of hash | `Pruned_hash of hash | `Portable_value ]

  type 'a or_error = ('a, error) result
  type 'a force = [ `True | `False of path -> 'a -> 'a Lwt.t ]
  type uniq = [ `False | `True | `Marks of marks ]
  type ('a, 'b) folder = path -> 'b -> 'a -> 'a Lwt.t

  type depth = [ `Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int ]
  [@@deriving irmin]

  let dummy_marks = Hashes.create ~initial_slots:0 ()
  let empty_marks () = Hashes.create ~initial_slots:39 ()

  exception Pruned_hash of { context : string; hash : hash }
  exception Dangling_hash of { context : string; hash : hash }
  exception Portable_value of { context : string }

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
      | Portable_value { context } ->
          Some
            (Fmt.str "Irmin.Tree.%s: unsupported operation on portable tree."
               context)
      | _ -> None)

  let err_pruned_hash h = Error (`Pruned_hash h)
  let err_dangling_hash h = Error (`Dangling_hash h)
  let err_portable_value = Error `Portable_value
  let pruned_hash_exn context hash = raise (Pruned_hash { context; hash })
  let portable_value_exn context = raise (Portable_value { context })

  let get_ok : type a. string -> a or_error -> a =
   fun context -> function
    | Ok x -> x
    | Error (`Pruned_hash hash) -> pruned_hash_exn context hash
    | Error (`Dangling_hash hash) -> raise (Dangling_hash { context; hash })
    | Error `Portable_value -> portable_value_exn context

  type 'key ptr_option = Key of 'key | Hash of hash | Ptr_none
  (* NOTE: given the choice, we prefer caching [Key] over [Hash] as it can
     be used to avoid storing duplicate contents values on export. *)

  module Contents = struct
    type key = P.Contents.Key.t [@@deriving irmin]
    type v = Key of repo * key | Value of contents | Pruned of hash
    type nonrec ptr_option = key ptr_option

    type info = {
      mutable ptr : ptr_option;
      mutable value : contents option;
      env : Env.t;
    }

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

    let of_v ~env (v : v) =
      let ptr, value =
        match v with
        | Key (_, k) -> ((Key k : ptr_option), None)
        | Value v -> (Ptr_none, Some v)
        | Pruned _ -> (Ptr_none, None)
      in
      let info = { ptr; value; env } in
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
      | Key (_, k), _ -> Some (P.Contents.Key.to_hash k)
      | Value _, Key k -> Some (P.Contents.Key.to_hash k)
      | Pruned h, _ -> Some h
      | Value _, Hash h -> Some h
      | Value _, Ptr_none -> None

    let cached_key t =
      match (t.v, t.info.ptr) with
      | Key (_, k), _ -> Some k
      | (Value _ | Pruned _), Key k -> Some k
      | (Value _ | Pruned _), (Hash _ | Ptr_none) -> None

    let cached_value t =
      match (t.v, t.info.value) with
      | Value v, None -> Some v
      | (Key _ | Value _ | Pruned _), (Some _ as v) -> v
      | (Key _ | Pruned _), None -> (
          match cached_hash t with
          | None -> None
          | Some h -> (
              match Env.find_contents t.info.env h with
              | None -> None
              | Some c -> Some c))

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
      let h = P.Contents.Key.to_hash k in
      let+ v_opt = P.Contents.find (P.Repo.contents_t repo) k in
      Option.iter (Env.add_contents_from_store t.info.env h) v_opt;
      match v_opt with
      | None -> err_dangling_hash h
      | Some v ->
          if cache then t.info.value <- v_opt;
          Ok v

    let to_value ~cache t =
      match cached_value t with
      | Some v -> ok v
      | None -> (
          match t.v with
          | Value _ -> assert false (* [cached_value == None] *)
          | Key (repo, k) -> value_of_key ~cache t repo k
          | Pruned h -> err_pruned_hash h |> Lwt.return)

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
        match merge_env x.info.env y.info.env with
        | Error _ as e -> Lwt.return e
        | Ok env -> (
            let* x = to_value ~cache:true x >|= Option.of_result in
            let* y = to_value ~cache:true y >|= Option.of_result in
            Merge.(f P.Contents.Val.merge) ~old x y >|= function
            | Ok (Some c) -> Ok (of_value ~env c)
            | Ok None -> Error (`Conflict "empty contents")
            | Error _ as e -> e)
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

    open struct
      module Portable = P.Node_portable
    end

    type portable = Portable.t [@@deriving irmin ~equal ~pp]

    (* [elt] is a tree *)
    type elt = [ `Node of t | `Contents of Contents.t * Metadata.t ]
    and update = Add of elt | Remove
    and updatemap = update StepMap.t
    and map = elt StepMap.t

    and info = {
      mutable value : value option;
      mutable map : map option;
      mutable ptr : ptr_option;
      mutable findv_cache : map option;
      mutable length : int Lazy.t option;
      env : Env.t;
    }

    and v =
      | Map of map
      | Key of repo * key
      | Value of repo * value * updatemap option
      | Portable_dirty of portable * updatemap
      | Pruned of hash

    and t = { mutable v : v; info : info }
    (** For discussion of [t.v]'s states, see {!Tree_intf.S.inspect}.

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
      variant "Node.node" (fun map key value pruned portable_dirty -> function
        | Map m -> map m
        | Key (_, y) -> key y
        | Value (_, v, m) -> value (v, m)
        | Pruned h -> pruned h
        | Portable_dirty (v, m) -> portable_dirty (v, m))
      |~ case1 "map" m (fun m -> Map m)
      |~ case1 "key" P.Node.Key.t (fun _ -> assert false)
      |~ case1 "value" (pair P.Node.Val.t (option um)) (fun _ -> assert false)
      |~ case1 "pruned" hash_t (fun h -> Pruned h)
      |~ case1 "portable_dirty" (pair portable_t um) (fun (v, m) ->
             Portable_dirty (v, m))
      |> sealv

    let of_v ?length ~env v =
      let ptr, map, value =
        match v with
        | Map m -> (Ptr_none, Some m, None)
        | Key (_, k) -> (Key k, None, None)
        | Value (_, v, None) -> (Ptr_none, None, Some v)
        | Value _ | Portable_dirty _ | Pruned _ -> (Ptr_none, None, None)
      in
      let findv_cache = None in
      let info = { ptr; map; value; findv_cache; env; length } in
      { v; info }

    let of_map m = of_v (Map m)
    let of_key repo k = of_v (Key (repo, k))

    let of_value ?length ?updates repo v =
      of_v ?length (Value (repo, v, updates))

    let of_portable_dirty p updates = of_v (Portable_dirty (p, updates))
    let pruned h = of_v (Pruned h)

    let info_is_empty i =
      i.map = None && i.value = None && i.findv_cache = None && i.ptr = Ptr_none

    let add_to_findv_cache t step v =
      match t.info.findv_cache with
      | None -> t.info.findv_cache <- Some (StepMap.singleton step v)
      | Some m -> t.info.findv_cache <- Some (StepMap.add step v m)

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

    and clear_info ~max_depth ~v depth i =
      let clear _ v = clear_elt ~max_depth depth v in
      let () =
        match v with
        | Value (_, _, Some um) ->
            StepMap.iter
              (fun k -> function Remove -> () | Add v -> clear k v)
              um
        | Value (_, _, None) | Map _ | Key _ | Portable_dirty _ | Pruned _ -> ()
      in
      let () =
        match (v, i.map) with
        | Map m, _ | (Key _ | Value _ | Portable_dirty _ | Pruned _), Some m ->
            StepMap.iter clear m
        | (Key _ | Value _ | Portable_dirty _ | Pruned _), None -> ()
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
      | Portable_dirty _ | Pruned _ ->
          (* The main export function never exports a pruned position. *)
          assert false

    module Core_value
        (N : Node.Generic_key.Core
               with type step := step
                and type hash := hash
                and type metadata := metadata)
        (To_elt : sig
          type repo

          val t : env:Env.t -> repo -> N.value -> elt
        end) =
    struct
      let to_map ~cache ~env repo t =
        cnt.node_val_list <- cnt.node_val_list + 1;
        let entries = N.seq ~cache t in
        Seq.fold_left
          (fun acc (k, v) -> StepMap.add k (To_elt.t ~env repo v) acc)
          StepMap.empty entries

      (** Does [um] empties [v]?

          Gotcha: Some [Remove] entries in [um] might not be in [v]. *)
      let is_empty_after_updates ~cache t um =
        let any_add =
          StepMap.to_seq um
          |> Seq.exists (function _, Remove -> false | _, Add _ -> true)
        in
        if any_add then false
        else
          let val_is_empty = N.is_empty t in
          if val_is_empty then true
          else
            let remove_count = StepMap.cardinal um in
            if (not val_is_empty) && remove_count = 0 then false
            else if N.length t > remove_count then false
            else (
              (* Starting from this point the function is expensive, but there is
                 no alternative. *)
              cnt.node_val_list <- cnt.node_val_list + 1;
              let entries = N.seq ~cache t in
              Seq.for_all (fun (step, _) -> StepMap.mem step um) entries)

      let findv ~cache ~env step node repo t =
        match N.find ~cache t step with
        | None -> None
        | Some v ->
            let tree = To_elt.t ~env repo v in
            if cache then add_to_findv_cache node step tree;
            Some tree

      let seq ~env ?offset ?length ~cache repo v =
        cnt.node_val_list <- cnt.node_val_list + 1;
        let seq = N.seq ?offset ?length ~cache v in
        Seq.map (fun (k, v) -> (k, To_elt.t ~env repo v)) seq
    end

    module Regular_value =
      Core_value
        (P.Node.Val)
        (struct
          type nonrec repo = repo

          let t ~env repo = function
            | `Node k -> `Node (of_key ~env repo k)
            | `Contents (k, m) -> `Contents (Contents.of_key ~env repo k, m)
        end)

    module Portable_value =
      Core_value
        (P.Node_portable)
        (struct
          type repo = unit

          let t ~env () = function
            | `Node h -> `Node (pruned ~env h)
            | `Contents (h, m) -> `Contents (Contents.pruned ~env h, m)
        end)

    (** This [Scan] module contains function that scan the content of [t.v] and
        [t.info], looking for specific patterns. *)
    module Scan = struct
      let iter_hash t hit miss miss_arg =
        match (t.v, t.info.ptr) with
        | Key (_, k), _ -> hit (P.Node.Key.to_hash k)
        | (Map _ | Value _ | Portable_dirty _), Key k ->
            hit (P.Node.Key.to_hash k)
        | Pruned h, _ -> hit h
        | (Map _ | Value _ | Portable_dirty _), Hash h -> hit h
        | (Map _ | Value _ | Portable_dirty _), Ptr_none -> miss t miss_arg

      let iter_key t hit miss miss_arg =
        match (t.v, t.info.ptr) with
        | Key (_, k), _ -> hit k
        | (Map _ | Value _ | Portable_dirty _ | Pruned _), Key k -> hit k
        | (Map _ | Value _ | Portable_dirty _ | Pruned _), (Hash _ | Ptr_none)
          ->
            miss t miss_arg

      let iter_map t hit miss miss_arg =
        match (t.v, t.info.map) with
        | (Key _ | Value _ | Portable_dirty _ | Pruned _), Some m -> hit m
        | Map m, _ -> hit m
        | (Key _ | Value _ | Portable_dirty _ | Pruned _), None ->
            miss t miss_arg

      let iter_value t hit miss miss_arg =
        match (t.v, t.info.value) with
        | Value (_, v, None), None -> hit v
        | (Map _ | Key _ | Value _ | Portable_dirty _ | Pruned _), Some v ->
            hit v
        | ( (Map _ | Key _ | Value (_, _, Some _) | Portable_dirty _ | Pruned _),
            None ) ->
            iter_hash t
              (fun h ->
                (* The need for [t], [miss] and [miss_arg] allocates a closure *)
                match Env.find_node t.info.env h with
                | None -> miss t miss_arg
                | Some v -> hit v)
              miss miss_arg

      let iter_portable t hit miss miss_arg =
        match t.v with
        | Pruned h -> (
            match Env.find_pnode t.info.env h with
            | None -> miss t miss_arg
            | Some v -> hit v)
        | Map _ | Key _ | Value _ | Portable_dirty _ ->
            (* No need to peek in [env]in these cases because [env]
               is in practice expected to only hit on [Pruned]. *)
            miss t miss_arg

      let iter_repo_key t hit miss miss_arg =
        match (t.v, t.info.ptr) with
        | Key (repo, k), _ -> hit repo k
        | Value (repo, _, _), Key k -> hit repo k
        | (Map _ | Portable_dirty _ | Pruned _ | Value _), _ -> miss t miss_arg

      let iter_repo_value t hit miss miss_arg =
        match (t.v, t.info.value) with
        | Value (repo, v, None), _ -> hit repo v
        | (Value (repo, _, _) | Key (repo, _)), Some v -> hit repo v
        | (Value (repo, _, _) | Key (repo, _)), None ->
            iter_hash t
              (fun h ->
                match Env.find_node t.info.env h with
                | None -> miss t miss_arg
                | Some v -> hit repo v)
              miss miss_arg
        | (Map _ | Portable_dirty _ | Pruned _), _ -> miss t miss_arg

      type node = t

      (** An instance of [t] is expected to be the result of a chain of [to_*]
          function calls.

          The [to_*] functions scan a [node] and look for a specific pattern.
          The first function in the chain to match a pattern will return the
          instance of [t] and ignore the rest of the chain.

          The functions in the chain should be carefuly ordered so that the
          computation that follows is as quick as possible (e.g. if the goal is
          to convert a [node] to hash, [to_hash] should be checked before
          [to_map]).

          [cascade] may be used in order to build chains. *)

      type _ t =
        | Hash : hash -> [> `hash ] t
        | Map : map -> [> `map ] t
        | Value : value -> [> `value ] t
        | Value_dirty : (repo * value * updatemap) -> [> `value_dirty ] t
        | Portable : portable -> [> `portable ] t
        | Portable_dirty : (portable * updatemap) -> [> `portable_dirty ] t
        | Pruned : hash -> [> `pruned ] t
        | Repo_key : (repo * key) -> [> `repo_key ] t
        | Repo_value : (repo * value) -> [> `repo_value ] t
        | Any : [> `any ] t

      module View_kind = struct
        type _ t =
          | Hash : [> `hash ] t
          | Map : [> `map ] t
          | Value : [> `value ] t
          | Value_dirty : [> `value_dirty ] t
          | Portable : [> `portable ] t
          | Portable_dirty : [> `portable_dirty ] t
          | Pruned : [> `pruned ] t
          | Repo_key : [> `repo_key ] t
          | Repo_value : [> `repo_value ] t
          | Any : [> `any ] t
      end

      let to_hash t miss = iter_hash t (fun h -> Hash h) miss
      let to_map t miss = iter_map t (fun m -> Map m) miss
      let to_value t miss = iter_value t (fun v -> Value v) miss
      let to_portable t miss = iter_portable t (fun v -> Portable v) miss

      let to_value_dirty t miss miss_arg =
        match t.v with
        | Value (repo, v, Some um) -> Value_dirty (repo, v, um)
        | Map _ | Key _ | Value (_, _, None) | Portable_dirty _ | Pruned _ ->
            miss t miss_arg

      let to_portable_dirty t miss miss_arg =
        match t.v with
        | Portable_dirty (v, um) -> Portable_dirty (v, um)
        | Map _ | Key _ | Value _ | Pruned _ -> miss t miss_arg

      let to_pruned t miss miss_arg =
        match t.v with
        | Pruned h -> Pruned h
        | Map _ | Key _ | Value _ | Portable_dirty _ -> miss t miss_arg

      let to_repo_key t miss miss_arg =
        iter_repo_key t (fun repo k -> Repo_key (repo, k)) miss miss_arg

      let to_repo_value t miss miss_arg =
        iter_repo_value t (fun repo v -> Repo_value (repo, v)) miss miss_arg

      let rec cascade : type k. node -> k View_kind.t list -> k t =
       fun t -> function
        | [] ->
            (* The declared cascade doesn't cover all cases *)
            assert false
        | x :: xs -> (
            match x with
            | Hash -> to_hash t cascade xs
            | Map -> to_map t cascade xs
            | Value -> to_value t cascade xs
            | Value_dirty -> to_value_dirty t cascade xs
            | Portable -> to_portable t cascade xs
            | Portable_dirty -> to_portable_dirty t cascade xs
            | Pruned -> to_pruned t cascade xs
            | Repo_key -> to_repo_key t cascade xs
            | Repo_value -> to_repo_value t cascade xs
            | Any -> Any)
    end

    let get_none _ () = None
    let cached_hash t = Scan.iter_hash t Option.some get_none ()
    let cached_key t = Scan.iter_key t Option.some get_none ()
    let cached_map t = Scan.iter_map t Option.some get_none ()
    let cached_value t = Scan.iter_value t Option.some get_none ()
    let cached_portable t = Scan.iter_portable t Option.some get_none ()

    let key t =
      match t.v with
      | Key (_, k) -> Some k
      | Map _ | Value _ | Portable_dirty _ | Pruned _ -> None

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
      let a_of_hashable hash v =
        cnt.node_hash <- cnt.node_hash + 1;
        let hash = hash v in
        assert (t.info.ptr = Ptr_none);
        if cache then t.info.ptr <- Hash hash;
        k hash
      in
      match
        (Scan.cascade t [ Hash; Value; Value_dirty; Portable_dirty; Map ]
          : [ `hash | `value | `value_dirty | `portable_dirty | `map ] Scan.t)
      with
      | Hash h -> k h
      | Value v -> a_of_hashable P.Node.Val.hash_exn v
      | Value_dirty (_repo, v, um) ->
          hash_preimage_of_updates ~cache t (Node v) um (function
            | Node x -> a_of_hashable P.Node.Val.hash_exn x
            | Pnode x -> a_of_hashable P.Node_portable.hash_exn x)
      | Portable_dirty (p, um) ->
          hash_preimage_of_updates ~cache t (Pnode p) um (function
            | Node x -> a_of_hashable P.Node.Val.hash_exn x
            | Pnode x -> a_of_hashable P.Node_portable.hash_exn x)
      | Map m ->
          hash_preimage_of_map ~cache t m (function
            | Node x -> a_of_hashable P.Node.Val.hash_exn x
            | Pnode x -> a_of_hashable P.Node_portable.hash_exn x)

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
                 | `Contents (c, m) -> (step, `Contents (Contents.hash c, m))
                 | `Node n -> hash ~cache n (fun k -> (step, `Node k)))
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
        if cache then t.info.value <- Some node;
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
        type r.
        cache:bool -> t -> hash_preimage -> updatemap -> (hash_preimage, r) cont
        =
     fun ~cache t v updates k ->
      let updates = StepMap.bindings updates in
      let rec aux acc = function
        | [] ->
            (if cache then
             match acc with Node n -> t.info.value <- Some n | Pnode _ -> ());
            k acc
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
      aux v updates

    let hash ~cache k = hash ~cache k (fun x -> x)

    let value_of_key ~cache t repo k =
      match cached_value t with
      | Some v -> ok v
      | None -> (
          cnt.node_find <- cnt.node_find + 1;
          let+ v_opt = P.Node.find (P.Repo.node_t repo) k in
          let h = P.Node.Key.to_hash k in
          let v_opt = Option.map (Env.add_node_from_store t.info.env h) v_opt in
          match v_opt with
          | None -> err_dangling_hash h
          | Some v ->
              if cache then t.info.value <- v_opt;
              Ok v)

    let to_value ~cache t =
      match
        (Scan.cascade t [ Value; Repo_key; Any ]
          : [ `value | `repo_key | `any ] Scan.t)
      with
      | Value v -> ok v
      | Repo_key (repo, k) -> value_of_key ~cache t repo k
      | Any -> (
          match t.v with
          | Key _ | Value (_, _, None) -> assert false
          | Pruned h -> err_pruned_hash h |> Lwt.return
          | Portable_dirty _ -> err_portable_value |> Lwt.return
          | Map _ | Value (_, _, Some _) ->
              invalid_arg
                "Tree.Node.to_value: the supplied node has not been written to \
                 disk. Either export it or convert it to a portable value \
                 instead.")

    let to_portable_value_aux ~cache ~value_of_key ~return ~bind:( let* ) t =
      let ok x = return (Ok x) in
      match
        (Scan.cascade t
           [
             Portable; Value; Repo_key; Portable_dirty; Value_dirty; Map; Pruned;
           ]
          : [ `portable
            | `value
            | `repo_key
            | `portable_dirty
            | `value_dirty
            | `map
            | `pruned ]
            Scan.t)
      with
      | Portable p -> ok p
      | Value v -> ok (P.Node_portable.of_node v)
      | Portable_dirty (p, um) ->
          hash_preimage_of_updates ~cache t (Pnode p) um (function
            | Node _ -> assert false
            | Pnode x -> ok x)
      | Repo_key (repo, k) ->
          let* value_res = value_of_key ~cache t repo k in
          Result.map P.Node_portable.of_node value_res |> return
      | Value_dirty (_repo, v, um) ->
          hash_preimage_of_updates ~cache t (Node v) um (function
            | Node x -> ok (Portable.of_node x)
            | Pnode x -> ok x)
      | Map m ->
          hash_preimage_of_map ~cache t m (function
            | Node x -> ok (Portable.of_node x)
            | Pnode x -> ok x)
      | Pruned h -> err_pruned_hash h |> return

    let to_portable_value =
      to_portable_value_aux ~value_of_key ~return:Lwt.return ~bind:Lwt.bind

    let to_map ~cache t =
      let of_maps m updates =
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
      let of_value repo v um =
        let env = t.info.env in
        let m = Regular_value.to_map ~env ~cache repo v in
        of_maps m um
      in
      let of_portable_value v um =
        let env = t.info.env in
        let m = Portable_value.to_map ~env ~cache () v in
        of_maps m um
      in
      match
        (Scan.cascade t
           [
             Map;
             Repo_value;
             Repo_key;
             Value_dirty;
             Portable;
             Portable_dirty;
             Pruned;
           ]
          : [ `map
            | `repo_key
            | `repo_value
            | `value_dirty
            | `portable
            | `portable_dirty
            | `pruned ]
            Scan.t)
      with
      | Map m -> ok m
      | Repo_value (repo, v) -> ok (of_value repo v None)
      | Repo_key (repo, k) -> (
          value_of_key ~cache t repo k >|= function
          | Error _ as e -> e
          | Ok v -> Ok (of_value repo v None))
      | Value_dirty (repo, v, um) -> ok (of_value repo v (Some um))
      | Portable p -> ok (of_portable_value p None)
      | Portable_dirty (p, um) -> ok (of_portable_value p (Some um))
      | Pruned h -> err_pruned_hash h |> Lwt.return

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
              match (cached_portable x, cached_portable y) with
              | Some x, Some y -> equal_portable x y
              | _ -> (
                  match (cached_map x, cached_map y) with
                  | Some x, Some y -> map_equal x y
                  | _ -> equal_hash (hash ~cache:true x) (hash ~cache:true y))))

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
            | _ -> (
                match (cached_portable x, cached_portable y) with
                | Some x, Some y -> if equal_portable x y then True else False
                | _ -> Maybe))

    let empty () = of_map StepMap.empty ~env:(Env.empty ())
    let empty_hash = hash ~cache:false (empty ())
    let singleton k v = of_map (StepMap.singleton k v)

    let slow_length ~cache t =
      match
        (Scan.cascade t
           [
             Map; Value; Portable; Repo_key; Value_dirty; Portable_dirty; Pruned;
           ]
          : [ `map
            | `value
            | `portable
            | `repo_key
            | `value_dirty
            | `portable_dirty
            | `pruned ]
            Scan.t)
      with
      | Map m -> StepMap.cardinal m |> Lwt.return
      | Value v -> P.Node.Val.length v |> Lwt.return
      | Portable p -> P.Node_portable.length p |> Lwt.return
      | Repo_key (repo, k) ->
          value_of_key ~cache t repo k >|= get_ok "length" >|= P.Node.Val.length
      | Value_dirty (_repo, v, um) ->
          hash_preimage_of_updates ~cache t (Node v) um (function
            | Node x -> P.Node.Val.length x |> Lwt.return
            | Pnode x -> P.Node_portable.length x |> Lwt.return)
      | Portable_dirty (p, um) ->
          hash_preimage_of_updates ~cache t (Pnode p) um (function
            | Node _ -> assert false
            | Pnode x -> P.Node_portable.length x |> Lwt.return)
      | Pruned h -> pruned_hash_exn "length" h

    let length ~cache t =
      match t.info.length with
      | Some (lazy len) -> Lwt.return len
      | None ->
          let+ len = slow_length ~cache t in
          t.info.length <- Some (Lazy.from_val len);
          len

    let is_empty ~cache t =
      match
        (Scan.cascade t
           [ Map; Value; Portable; Hash; Value_dirty; Portable_dirty ]
          : [ `map
            | `value
            | `portable
            | `hash
            | `value_dirty
            | `portable_dirty ]
            Scan.t)
      with
      | Map m -> StepMap.is_empty m
      | Value v -> P.Node.Val.is_empty v
      | Portable p -> P.Node_portable.is_empty p
      | Hash h -> equal_hash h empty_hash
      | Value_dirty (_repo, v, um) ->
          Regular_value.is_empty_after_updates ~cache v um
      | Portable_dirty (p, um) ->
          Portable_value.is_empty_after_updates ~cache p um

    let findv_aux ~cache ~value_of_key ~return ~bind:( let* ) ctx t step =
      let of_map m = try Some (StepMap.find step m) with Not_found -> None in
      let of_value = Regular_value.findv ~cache ~env:t.info.env step t in
      let of_portable = Portable_value.findv ~cache ~env:t.info.env step t () in
      let of_t () =
        match
          (Scan.cascade t
             [
               Map;
               Repo_value;
               Repo_key;
               Value_dirty;
               Portable;
               Portable_dirty;
               Pruned;
             ]
            : [ `map
              | `repo_key
              | `repo_value
              | `value_dirty
              | `portable
              | `portable_dirty
              | `pruned ]
              Scan.t)
        with
        | Map m -> return (of_map m)
        | Repo_value (repo, v) -> return (of_value repo v)
        | Repo_key (repo, k) ->
            let* v = value_of_key ~cache t repo k in
            let v = get_ok ctx v in
            return (of_value repo v)
        | Value_dirty (repo, v, um) -> (
            match StepMap.find_opt step um with
            | Some (Add v) -> return (Some v)
            | Some Remove -> return None
            | None -> return (of_value repo v))
        | Portable p -> return (of_portable p)
        | Portable_dirty (p, um) -> (
            match StepMap.find_opt step um with
            | Some (Add v) -> return (Some v)
            | Some Remove -> return None
            | None -> return (of_portable p))
        | Pruned h -> pruned_hash_exn ctx h
      in
      match t.info.findv_cache with
      | None -> of_t ()
      | Some m -> (
          match of_map m with None -> of_t () | Some _ as r -> return r)

    let findv = findv_aux ~value_of_key ~return:Lwt.return ~bind:Lwt.bind

    let seq_of_map ?(offset = 0) ?length m : (step * elt) Seq.t =
      let take seq =
        match length with None -> seq | Some n -> Seq.take n seq
      in
      StepMap.to_seq m |> Seq.drop offset |> take

    let seq ?offset ?length ~cache t : (step * elt) Seq.t or_error Lwt.t =
      let env = t.info.env in
      match
        (Scan.cascade t
           [
             Map;
             Repo_value;
             Repo_key;
             Value_dirty;
             Portable;
             Portable_dirty;
             Pruned;
           ]
          : [ `map
            | `repo_key
            | `repo_value
            | `value_dirty
            | `portable
            | `portable_dirty
            | `pruned ]
            Scan.t)
      with
      | Map m -> ok (seq_of_map ?offset ?length m)
      | Repo_value (repo, v) ->
          ok (Regular_value.seq ~env ?offset ?length ~cache repo v)
      | Repo_key (repo, k) -> (
          value_of_key ~cache t repo k >>= function
          | Error _ as e -> Lwt.return e
          | Ok v -> ok (Regular_value.seq ~env ?offset ?length ~cache repo v))
      | Value_dirty _ | Portable_dirty _ -> (
          to_map ~cache t >>= function
          | Error _ as e -> Lwt.return e
          | Ok m -> ok (seq_of_map ?offset ?length m))
      | Portable p -> ok (Portable_value.seq ~env ?offset ?length ~cache () p)
      | Pruned h -> err_pruned_hash h |> Lwt.return

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

    type ('v, 'acc, 'r) cps_folder =
      path:Path.t -> 'acc -> int -> 'v -> ('acc, 'r) cont_lwt
    (** A ('val, 'acc, 'r) cps_folder is a CPS, threaded fold function over
        values of type ['v] producing an accumulator of type ['acc]. *)

    let fold :
        type acc.
        order:[ `Sorted | `Undefined | `Random of Random.State.t ] ->
        force:acc force ->
        cache:bool ->
        uniq:uniq ->
        pre:(acc, step list) folder option ->
        post:(acc, step list) folder option ->
        path:Path.t ->
        ?depth:depth ->
        node:(acc, _) folder ->
        contents:(acc, contents) folder ->
        tree:(acc, _) folder ->
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
      let rec aux : type r. (t, acc, r) cps_folder =
       fun ~path acc d t k ->
        let apply acc = node path t acc >>= tree path (`Node t) in
        let next acc =
          match force with
          | `True -> (
              match order with
              | `Random state ->
                  let* m = to_map ~cache t >|= get_ok "fold" in
                  let arr = StepMap.to_array m in
                  let () = shuffle state arr in
                  let s = Array.to_seq arr in
                  (seq [@tailcall]) ~path acc d s k
              | `Sorted ->
                  let* m = to_map ~cache t >|= get_ok "fold" in
                  (map [@tailcall]) ~path acc d (Some m) k
              | `Undefined -> (
                  match
                    (Scan.cascade t
                       [
                         Map;
                         Repo_value;
                         Repo_key;
                         Value_dirty;
                         Portable;
                         Portable_dirty;
                         Pruned;
                       ]
                      : [ `map
                        | `repo_key
                        | `repo_value
                        | `value_dirty
                        | `portable
                        | `portable_dirty
                        | `pruned ]
                        Scan.t)
                  with
                  | Map m -> (map [@tailcall]) ~path acc d (Some m) k
                  | Repo_value (repo, v) ->
                      (value [@tailcall]) ~path acc d (repo, v, None) k
                  | Repo_key (repo, _key) ->
                      let* v = to_value ~cache t >|= get_ok "fold" in
                      (value [@tailcall]) ~path acc d (repo, v, None) k
                  | Value_dirty (repo, v, um) ->
                      (value [@tailcall]) ~path acc d (repo, v, Some um) k
                  | Portable p -> (portable [@tailcall]) ~path acc d (p, None) k
                  | Portable_dirty (p, um) ->
                      (portable [@tailcall]) ~path acc d (p, Some um) k
                  | Pruned h -> pruned_hash_exn "fold" h))
          | `False skip -> (
              match cached_map t with
              | Some n -> (
                  match order with
                  | `Sorted | `Undefined ->
                      (map [@tailcall]) ~path acc d (Some n) k
                  | `Random state ->
                      let arr = StepMap.to_array n in
                      shuffle state arr;
                      let s = Array.to_seq arr in
                      (seq [@tailcall]) ~path acc d s k)
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
      and aux_uniq : type r. (t, acc, r) cps_folder =
       fun ~path acc d t k ->
        if uniq = `False then (aux [@tailcall]) ~path acc d t k
        else
          let h = hash ~cache t in
          match Hashes.add marks h with
          | `Duplicate -> k acc
          | `Ok -> (aux [@tailcall]) ~path acc d t k
      and step : type r. (step * elt, acc, r) cps_folder =
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
      and steps : type r. ((step * elt) Seq.t, acc, r) cps_folder =
       fun ~path acc d s k ->
        match s () with
        | Seq.Nil -> k acc
        | Seq.Cons (h, t) ->
            (step [@tailcall]) ~path acc d h (fun acc ->
                (steps [@tailcall]) ~path acc d t k)
      and map : type r. (map option, acc, r) cps_folder =
       fun ~path acc d m k ->
        match m with
        | None -> k acc
        | Some m ->
            let bindings = StepMap.to_seq m in
            seq ~path acc d bindings k
      and value : type r. (repo * value * updatemap option, acc, r) cps_folder =
       fun ~path acc d (repo, v, updates) k ->
        let bindings = Regular_value.seq ~env ~cache repo v in
        let bindings =
          match updates with
          | None -> bindings
          | Some updates -> seq_of_updates updates bindings
        in
        seq ~path acc d bindings k
      and portable : type r. (portable * updatemap option, acc, r) cps_folder =
       fun ~path acc d (v, updates) k ->
        let bindings = Portable_value.seq ~env ~cache () v in
        let bindings =
          match updates with
          | None -> bindings
          | Some updates -> seq_of_updates updates bindings
        in
        seq ~path acc d bindings k
      and seq : type r. ((step * elt) Seq.t, acc, r) cps_folder =
       fun ~path acc d bindings k ->
        let* acc = pre path bindings acc in
        (steps [@tailcall]) ~path acc d bindings (fun acc ->
            post path bindings acc >>= k)
      in
      aux_uniq ~path acc 0 t Lwt.return

    let incremental_length t step up n updates =
      match t.info.length with
      | None -> None
      | Some len ->
          Some
            (lazy
              (let len = Lazy.force len in
               let exists =
                 match StepMap.find_opt step updates with
                 | Some (Add _) -> true
                 | Some Remove -> false
                 | None -> (
                     match P.Node.Val.find n step with
                     | None -> false
                     | Some _ -> true)
               in
               match up with
               | Add _ when not exists -> len + 1
               | Remove when exists -> len - 1
               | _ -> len))

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
        else
          let length = incremental_length t step up n updates in
          of_value ?length ~env repo n ~updates:updates'
      in
      let of_portable n updates =
        let updates' = StepMap.add step up updates in
        if updates == updates' then t else of_portable_dirty ~env n updates'
      in
      match
        (Scan.cascade t
           [
             Map;
             Repo_value;
             Repo_key;
             Value_dirty;
             Portable;
             Portable_dirty;
             Pruned;
           ]
          : [ `map
            | `repo_key
            | `repo_value
            | `value_dirty
            | `portable
            | `portable_dirty
            | `pruned ]
            Scan.t)
      with
      | Map m -> Lwt.return (of_map m)
      | Repo_value (repo, v) -> Lwt.return (of_value repo v StepMap.empty)
      | Repo_key (repo, k) ->
          let+ v = value_of_key ~cache:true t repo k >|= get_ok "update" in
          of_value repo v StepMap.empty
      | Value_dirty (repo, v, um) -> Lwt.return (of_value repo v um)
      | Portable p -> Lwt.return (of_portable p StepMap.empty)
      | Portable_dirty (p, um) -> Lwt.return (of_portable p um)
      | Pruned h -> pruned_hash_exn "update" h

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
    let dump = Type.pp_dump t

    let rec merge : type a. (t Merge.t -> a) -> a =
     fun k ->
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              let+ m = to_map ~cache:true old >|= Option.of_result in
              Ok (Some m))
        in
        match merge_env x.info.env y.info.env with
        | Error _ as e -> Lwt.return e
        | Ok env -> (
            let* x = to_map ~cache:true x >|= Option.of_result in
            let* y = to_map ~cache:true y >|= Option.of_result in
            let m =
              StepMap.merge elt_t (fun _step ->
                  (merge_elt [@tailcall]) Merge.option)
            in
            Merge.(f @@ option m) ~old x y >|= function
            | Ok (Some map) -> Ok (of_map ~env map)
            | Ok None -> Error (`Conflict "empty map")
            | Error _ as e -> e)
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
            Merge.(f Metadata.merge) ~old:mold cx cy >>=* fun m ->
            let old =
              Merge.bind_promise old (fun old () ->
                  match old with
                  | `Contents (c, _) -> ok (Some c)
                  | `Node _ -> ok None)
            in
            Merge.(f Contents.merge) ~old x y >>=* fun c ->
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

  type node = Node.t [@@deriving irmin ~pp]
  type node_key = Node.key [@@deriving irmin ~pp]
  type contents_key = Contents.key [@@deriving irmin ~pp]

  type kinded_key = [ `Contents of Contents.key * metadata | `Node of Node.key ]
  [@@deriving irmin]

  type kinded_hash = [ `Contents of hash * metadata | `Node of hash ]
  [@@deriving irmin ~equal]

  type t = [ `Node of node | `Contents of Contents.t * Metadata.t ]
  [@@deriving irmin]

  let to_backend_node n =
    Node.to_value ~cache:true n >|= get_ok "to_backend_node"

  let to_backend_portable_node n =
    Node.to_portable_value ~cache:true n >|= get_ok "to_backend_portable_node"

  let of_backend_node repo n =
    let env = Env.empty () in
    let length = lazy (P.Node.Val.length n) in
    Node.of_value ~length ~env repo n

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

  let pruned_with_env ~env = function
    | `Contents (h, meta) -> `Contents (Contents.pruned ~env h, meta)
    | `Node h -> `Node (Node.pruned ~env h)

  let pruned h =
    let env = Env.empty () in
    pruned_with_env ~env h

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
    [%log.debug "Tree.find_tree %a" pp_path path];
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
    [%log.debug "Tree.kind %a" pp_path path];
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

  let length t ?(cache = true) path =
    [%log.debug "Tree.length %a" pp_path path];
    sub ~cache "length" t path >>= function
    | None -> Lwt.return 0
    | Some n -> Node.length ~cache:true n

  let seq t ?offset ?length ?(cache = true) path =
    [%log.debug "Tree.seq %a" pp_path path];
    sub ~cache "seq.sub" t path >>= function
    | None -> Lwt.return Seq.empty
    | Some n -> Node.seq ?offset ?length ~cache n >|= get_ok "seq"

  let list t ?offset ?length ?(cache = true) path =
    seq t ?offset ?length ~cache path >|= List.of_seq

  let empty () = `Node (Node.empty ())

  let singleton k ?(metadata = Metadata.default) c =
    [%log.debug "Tree.singleton %a" pp_path k];
    let env = Env.empty () in
    let base_tree = `Contents (Contents.of_value ~env c, metadata) in
    Path.fold_right k
      ~f:(fun step child -> `Node (Node.singleton ~env step child))
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
        let empty_tree =
          match is_empty root_tree with
          | true -> root_tree
          | false -> `Node (Node.empty ())
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
        let rec aux : type r. path -> node -> (node updated, r) cont_lwt =
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
        | Changed node ->
            Env.copy ~into:node.info.env (get_env root_tree);
            Lwt.return (`Node node))

  let update t k ?(metadata = Metadata.default) f =
    let cache = true in
    [%log.debug "Tree.update %a" pp_path k];
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
        | Some c -> of_contents ~metadata c |> Option.some)

  let add t k ?(metadata = Metadata.default) c =
    [%log.debug "Tree.add %a" pp_path k];
    update_tree ~cache:true t k
      ~f:(fun _ -> Lwt.return_some (of_contents ~metadata c))
      ~f_might_return_empty_node:false

  let add_tree t k v =
    [%log.debug "Tree.add_tree %a" pp_path k];
    update_tree ~cache:true t k
      ~f:(fun _ -> Lwt.return_some v)
      ~f_might_return_empty_node:true

  let remove t k =
    [%log.debug "Tree.remove %a" pp_path k];
    update_tree ~cache:true t k
      ~f:(fun _ -> Lwt.return_none)
      ~f_might_return_empty_node:false

  let update_tree t k f =
    [%log.debug "Tree.update_tree %a" pp_path k];
    update_tree ~cache:true t k ~f:(Lwt.wrap1 f) ~f_might_return_empty_node:true

  let import repo = function
    | `Contents (k, m) -> (
        cnt.contents_mem <- cnt.contents_mem + 1;
        P.Contents.mem (P.Repo.contents_t repo) k >|= function
        | true ->
            let env = Env.empty () in
            Some (`Contents (Contents.of_key ~env repo k, m))
        | false -> None)
    | `Node k -> (
        cnt.node_mem <- cnt.node_mem + 1;
        P.Node.mem (P.Repo.node_t repo) k >|= function
        | true ->
            let env = Env.empty () in
            Some (`Node (Node.of_key ~env repo k))
        | false -> None)

  let import_with_env ~env repo = function
    | `Node k -> `Node (Node.of_key ~env repo k)
    | `Contents (k, m) -> `Contents (Contents.of_key ~env repo k, m)

  let import_no_check repo f =
    let env = Env.empty () in
    import_with_env ~env repo f

  let same_repo r1 r2 =
    r1 == r2 || Conf.equal (P.Repo.config r1) (P.Repo.config r2)

  (* Given an arbitrary tree value, persist its contents to the given contents
     and node stores via a depth-first {i post-order} traversal. We must export
     a node's children before the node itself in order to get the {i keys} of
     any un-persisted child values. *)
  let export ?clear repo contents_t node_t n =
    [%log.debug "Tree.export clear=%a" Fmt.(option bool) clear];
    let cache =
      match clear with
      | Some true | None ->
          (* This choice of [cache] flag has no impact, since we either
             immediately clear the corresponding cache or are certain that
             the it is already filled. *)
          false
      | Some false -> true
    in

    let add_node n v k =
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
        | Some h' ->
            let h = P.Node.Key.to_hash key in
            if not (equal_hash h h') then
              backend_invariant_violation
                "@[<v 2>Tree.export: added inconsistent node binding@,\
                 key: %a@,\
                 value: %a@,\
                 computed hash: %a@]" pp_node_key key Node.pp_value v pp_hash h'
      in
      k key
    in

    let add_node_map n (x : Node.map) k =
      let node =
        (* Since we traverse in post-order, all children of [x] have already
           been added. Thus, their keys are cached and we can retrieve them. *)
        cnt.node_val_v <- cnt.node_val_v + 1;
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

    let rec on_node : type r. [ `Node of node ] -> (node_key, r) cont_lwt =
     fun (`Node n) k ->
      let k key =
        (* All the nodes in the exported tree should be cleaned using
           [Node.export]. This ensures that [key] is stored in [n]. *)
        Node.export ?clear repo n key;
        k key
      in
      let has_repo =
        match n.Node.v with
        | Node.Key (repo', _) ->
            if same_repo repo repo' then true
            else
              (* Case 1. [n] is a key from another repo. Let's crash.

                 We could also only crash if the hash in the key is unknown to
                 [repo], or completely ignore the issue. *)
              failwith "Can't export the node key from another repo"
        | Value (repo', _, _) ->
            if same_repo repo repo' then true
            else
              (* Case 2. [n] is a value from another repo. Let's crash.

                 We could also ignore the issue. *)
              failwith "Can't export a node value from another repo"
        | Pruned _ | Portable_dirty _ | Map _ -> false
      in
      match n.Node.v with
      | Pruned h ->
          (* Case 3. [n] is a pruned hash. [P.Node.index node_t h] could be
             different than [None], but let's always crash. *)
          pruned_hash_exn "export" h
      | Portable_dirty _ ->
          (* Case 4. [n] is a portable value with diffs. The hash of the
             reconstructed portable value could be known by [repo], but let's
             always crash. *)
          portable_value_exn "export"
      | Map _ | Value _ | Key _ -> (
          match Node.cached_key n with
          | Some key ->
              if has_repo then
                (* Case 5. [n] is a key that is accompanied by the [repo]. Let's
                   assume that [P.Node.mem node_t key] is [true] for performance
                   reason (not benched). *)
                k key
              else (
                cnt.node_mem <- cnt.node_mem + 1;
                let* mem = P.Node.mem node_t key in
                if not mem then
                  (* Case 6. [n] contains a key that is not known by [repo].
                     Let's abort. *)
                  failwith "Can't export a key unkown from the repo"
                else
                  (* Case 7. [n] contains a key that is known by the [repo]. *)
                  k key)
          | None -> (
              let* skip_when_some =
                match Node.cached_hash n with
                | None ->
                    (* No pre-computed hash. *)
                    Lwt.return_none
                | Some h -> (
                    cnt.node_index <- cnt.node_index + 1;
                    P.Node.index node_t h >>= function
                    | None ->
                        (* Pre-computed hash is unknown by repo.

                           NOTE: it's possible that this value already has a key
                           in the store, but it's not indexed. If so, we're
                           adding a duplicate here – this isn't an issue for
                           correctness, but does waste space. *)
                        Lwt.return_none
                    | Some key ->
                        cnt.node_mem <- cnt.node_mem + 1;
                        let+ mem = P.Node.mem node_t key in
                        if mem then
                          (* Case 8. The pre-computed hash is converted into
                             a key *)
                          Some key
                        else
                          (* The backend could produce a key from [h] but
                             doesn't know [h]. *)
                          None)
              in
              match skip_when_some with
              | Some key -> k key
              | None -> (
                  (* Only [Map _ | Value _] possible now.

                     Case 9. Let's export it to the backend. *)
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
                      | Key _ | Portable_dirty _ | Pruned _ ->
                          (* [n.v = (Key _ | Portable_dirty _ | Pruned _)] is
                             excluded above. *)
                          assert false
                    in
                    Seq.map (fun (_, x) -> x) seq
                  in
                  on_node_seq new_children_seq @@ fun `Node_children_exported ->
                  match (n.Node.v, Node.cached_value n) with
                  | Map x, _ -> add_node_map n x k
                  | Value (_, v, None), None | _, Some v -> add_node n v k
                  | Value (_, v, Some um), _ -> add_updated_node n v um k
                  | (Key _ | Portable_dirty _ | Pruned _), _ ->
                      (* [n.v = (Key _ | Portable_dirty _ | Pruned _)] is
                         excluded above. *)
                      assert false)))
    and on_contents :
        type r.
        [ `Contents of Contents.t * metadata ] ->
        ([ `Content_exported ], r) cont_lwt =
     fun (`Contents (c, _)) k ->
      match c.Contents.v with
      | Contents.Key (_, key) ->
          Contents.export ?clear repo c key;
          k `Content_exported
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
          k `Content_exported
      | Contents.Pruned h -> pruned_hash_exn "export" h
    and on_node_seq :
        type r. Node.elt Seq.t -> ([ `Node_children_exported ], r) cont_lwt =
     fun seq k ->
      match seq () with
      | Seq.Nil ->
          (* Have iterated on all children, let's export parent now *)
          k `Node_children_exported
      | Seq.Cons ((`Node _ as n), rest) ->
          on_node n (fun _node_key -> on_node_seq rest k)
      | Seq.Cons ((`Contents _ as c), rest) ->
          on_contents c (fun `Content_exported -> on_node_seq rest k)
    in
    on_node (`Node n) (fun key -> Lwt.return key)

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
    | ( Error (`Dangling_hash h1 | `Pruned_hash h1),
        Error (`Dangling_hash h2 | `Pruned_hash h2) ) -> (
        match equal_hash h1 h2 with true -> empty | false -> assert false)
    | Error _, Ok _ -> assert false
    | Ok _, Error _ -> assert false
    | Ok x, Ok y -> diff_ok (x, y)
    | Error _, Error _ -> assert false

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
          | Portable_dirty _ -> `Portable_dirty
          | Pruned _ -> `Pruned)

  module Proof = struct
    type irmin_tree = t

    include Tree_proof

    type proof_tree = tree
    type proof_inode = inode_tree
    type node_proof = P.Node_portable.proof

    let proof_of_iproof : proof_inode -> proof_tree = function
      | Blinded_inode h -> Blinded_node h
      | Inode_values l -> Node l
      | Inode_tree i -> Inode i
      | Inode_extender ext -> Extender ext

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
      (* Let's convert [node] to [node_proof].

         As [node] might not be exported, we can only turn it into a portable
         node. *)
      let to_portable_value =
        let value_of_key ~cache:_ _node _repo k =
          let h = P.Node.Key.to_hash k in
          err_dangling_hash h
        in
        Node.to_portable_value_aux ~cache:false ~value_of_key ~return:Fun.id
          ~bind:(fun x f -> f x)
      in
      match to_portable_value node with
      | Error (`Dangling_hash h) -> k (Blinded_node h)
      | Error (`Pruned_hash h) -> k (Blinded_node h)
      | Ok v ->
          (* [to_proof] may trigger reads. This is fine. *)
          let node_proof = P.Node_portable.to_proof v in
          proof_of_node_proof node node_proof k

    (** [of_node_proof n np] is [p] (of type [Tree.Proof.t]) which is very
        similar to [np] (of type [P.Node.Val.proof]) except that the values
        loaded in [n] have been expanded. *)
    and proof_of_node_proof :
        type a. node -> node_proof -> (proof_tree -> a) -> a =
     fun node p k ->
      match p with
      | `Blinded h -> k (Blinded_node h)
      | `Inode (length, proofs) ->
          iproof_of_inode node length proofs (fun p -> proof_of_iproof p |> k)
      | `Values vs -> iproof_of_values node vs (fun p -> proof_of_iproof p |> k)

    and iproof_of_node_proof :
        type a. node -> node_proof -> (proof_inode -> a) -> a =
     fun node p k ->
      match p with
      | `Blinded h -> k (Blinded_inode h)
      | `Inode (length, proofs) -> iproof_of_inode node length proofs k
      | `Values vs -> iproof_of_values node vs k

    and iproof_of_inode :
        type a. node -> int -> (_ * node_proof) list -> (proof_inode -> a) -> a
        =
     fun node length proofs k ->
      let rec aux acc = function
        | [] -> k (Inode_tree { length; proofs = List.rev acc })
        | (index, proof) :: rest ->
            iproof_of_node_proof node proof (fun proof ->
                aux ((index, proof) :: acc) rest)
      in
      (* We are dealing with an inode A.
         Its children are Bs.
         The children of Bs are Cs.
      *)
      match proofs with
      | [ (index, proof) ] ->
          (* A has 1 child. *)
          iproof_of_node_proof node proof (function
            | Inode_tree { length = length'; proofs = [ (i, p) ] } ->
                (* B is an inode with 1 child, C isn't. *)
                assert (length = length');
                k
                  (Inode_extender { length; segments = [ index; i ]; proof = p })
            | Inode_extender { length = length'; segments; proof } ->
                (* B is an inode with 1 child, so is C. *)
                assert (length = length');
                k
                  (Inode_extender
                     { length; segments = index :: segments; proof })
            | (Blinded_inode _ | Inode_values _ | Inode_tree _) as p ->
                (* B is not an inode with 1 child. *)
                k (Inode_tree { length; proofs = [ (index, p) ] }))
      | _ -> aux [] proofs

    and iproof_of_values :
        type a.
        node -> (step * Node.pnode_value) list -> (proof_inode -> a) -> a =
      let findv =
        let value_of_key ~cache:_ _node _repo k =
          let h = P.Node.Key.to_hash k in
          err_dangling_hash h
        in
        Node.findv_aux ~value_of_key ~return:Fun.id ~bind:(fun x f -> f x)
      in
      fun node steps k ->
        let rec aux acc = function
          | [] -> k (Inode_values (List.rev acc))
          | (step, _) :: rest -> (
              match findv ~cache:false "Proof.iproof_of_values" node step with
              | None -> assert false
              | Some t ->
                  let k p = aux ((step, p) :: acc) rest in
                  proof_of_tree t k)
        in
        aux [] steps

    let of_tree t = proof_of_tree t Fun.id

    let rec load_proof : type a. env:_ -> proof_tree -> (kinded_hash -> a) -> a
        =
     fun ~env p k ->
      match p with
      | Blinded_node h -> k (`Node h)
      | Node n -> load_node_proof ~env n k
      | Inode { length; proofs } -> load_inode_proof ~env length proofs k
      | Blinded_contents (h, m) -> k (`Contents (h, m))
      | Contents (v, m) ->
          let h = P.Contents.Hash.hash v in
          Env.add_contents_from_proof env h v;
          k (`Contents (h, m))
      | Extender { length; segments; proof } ->
          load_extender_proof ~env length segments proof k

    (* Recontruct private node from [P.Node.Val.proof] *)
    and load_extender_proof :
        type a.
        env:_ -> int -> int list -> proof_inode -> (kinded_hash -> a) -> a =
     fun ~env len segments p k ->
      node_proof_of_proof ~env p (fun p ->
          let np = proof_of_extender len segments p in
          let v = P.Node_portable.of_proof ~depth:0 np in
          let v =
            match v with
            | None -> Proof.bad_proof_exn "Invalid proof"
            | Some v -> v
          in
          let h = P.Node_portable.hash_exn v in
          Env.add_pnode_from_proof env h v;
          k (`Node h))

    and proof_of_extender len segments p : node_proof =
      List.fold_left
        (fun acc index -> `Inode (len, [ (index, acc) ]))
        p (List.rev segments)

    (* Recontruct private node from [P.Node.Val.empty] *)
    and load_node_proof :
        type a. env:_ -> (step * proof_tree) list -> (kinded_hash -> a) -> a =
     fun ~env n k ->
      let rec aux acc = function
        | [] ->
            let h = P.Node_portable.hash_exn acc in
            Env.add_pnode_from_proof env h acc;
            k (`Node h)
        | (s, p) :: rest ->
            let k h = aux (P.Node_portable.add acc s h) rest in
            load_proof ~env p k
      in
      aux (P.Node_portable.empty ()) n

    (* Recontruct private node from [P.Node.Val.proof] *)
    and load_inode_proof :
        type a.
        env:_ -> int -> (_ * proof_inode) list -> (kinded_hash -> a) -> a =
     fun ~env len proofs k ->
      let rec aux : _ list -> _ list -> a =
       fun acc proofs ->
        match proofs with
        | [] ->
            let np = `Inode (len, List.rev acc) in
            let v = P.Node_portable.of_proof ~depth:0 np in
            let v =
              match v with
              | None -> Proof.bad_proof_exn "Invalid proof"
              | Some v -> v
            in
            let h = P.Node_portable.hash_exn v in
            Env.add_pnode_from_proof env h v;
            k (`Node h)
        | (i, p) :: rest ->
            let k p = aux ((i, p) :: acc) rest in
            node_proof_of_proof ~env p k
      in
      aux [] proofs

    and node_proof_of_proof :
        type a. env:_ -> proof_inode -> (node_proof -> a) -> a =
     fun ~env t k ->
      match t with
      | Blinded_inode x -> k (`Blinded x)
      | Inode_tree { length; proofs } ->
          node_proof_of_inode ~env length proofs k
      | Inode_values n -> node_proof_of_node ~env n k
      | Inode_extender { length; segments; proof } ->
          node_proof_of_proof ~env proof (fun p ->
              k (proof_of_extender length segments p))

    and node_proof_of_inode :
        type a. env:_ -> int -> (_ * proof_inode) list -> (node_proof -> a) -> a
        =
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
        | (s, p) :: rest ->
            load_proof ~env p (fun n -> aux ((s, n) :: acc) rest)
      in
      aux [] node

    let to_tree p =
      let env = Env.empty () in
      Env.set_mode env Env.Set Env.Deserialise;
      let h = load_proof ~env (state p) Fun.id in
      let tree = pruned_with_env ~env h in
      Env.set_mode env Env.Set Env.Consume;
      tree
  end

  let produce_proof repo kinded_key f =
    Env.with_set_produce @@ fun env ~start_serialise ->
    let tree = import_with_env ~env repo kinded_key in
    let+ tree_after, result = f tree in
    let after = hash tree_after in
    (* Here, we build a proof from [tree] (not from [tree_after]!), on purpose:
       we look at the effect on [f] on [tree]'s caches and we rely on the fact
       that the caches are env across copy-on-write copies of [tree]. *)
    clear tree;
    start_serialise ();
    let proof = Proof.of_tree tree in
    (* [env] will be purged when leaving the scope, that should avoid any memory
       leaks *)
    let kinded_hash = Node.weaken_value kinded_key in
    (Proof.v ~before:kinded_hash ~after proof, result)

  let produce_stream repo kinded_key f =
    Env.with_stream_produce @@ fun env ~to_stream ->
    let tree = import_with_env ~env repo kinded_key in
    let+ tree_after, result = f tree in
    let after = hash tree_after in
    clear tree;
    let proof = to_stream () in
    let kinded_hash = Node.weaken_value kinded_key in
    (Proof.v ~before:kinded_hash ~after proof, result)

  let verify_proof_exn p f =
    Env.with_set_consume @@ fun env ~stop_deserialise ->
    let before = Proof.before p in
    let after = Proof.after p in
    (* First convert to proof to [Env] *)
    let h = Proof.(load_proof ~env (state p) Fun.id) in
    (* Then check that the consistency of the proof *)
    if not (equal_kinded_hash before h) then
      Irmin_proof.bad_proof_exn "verify_proof: invalid before hash";
    let tree = pruned_with_env ~env h in
    Lwt.catch
      (fun () ->
        stop_deserialise ();
        (* Then apply [f] on a cleaned tree, an exception will be raised if [f]
           reads out of the proof. *)
        let+ tree_after, result = f tree in
        (* then check that [after] corresponds to [tree_after]'s hash. *)
        if not (equal_kinded_hash after (hash tree_after)) then
          Irmin_proof.bad_proof_exn "verify_proof: invalid after hash";
        (tree_after, result))
      (function
        | Pruned_hash h ->
            (* finaly check that [f] only access valid parts of the proof. *)
            Fmt.kstr Irmin_proof.bad_proof_exn
              "verify_proof: %s is trying to read through a blinded node or \
               object (%a)"
              h.context pp_hash h.hash
        | e -> raise e)

  type verifier_error =
    [ `Proof_mismatch of string
    | `Stream_too_long of string
    | `Stream_too_short of string ]
  [@@deriving irmin]

  let verify_proof p f =
    Lwt.catch
      (fun () ->
        let+ r = verify_proof_exn p f in
        Ok r)
      (function
        | Irmin_proof.Bad_proof e ->
            Lwt.return (Error (`Proof_mismatch e.context))
        | e -> Lwt.fail e)

  let verify_stream_exn p f =
    let before = Proof.before p in
    let after = Proof.after p in
    let stream = Proof.state p in
    Env.with_stream_consume stream @@ fun env ~is_empty ->
    let tree = pruned_with_env ~env before in
    Lwt.catch
      (fun () ->
        let+ tree_after, result = f tree in
        if not (is_empty ()) then
          Irmin_proof.bad_stream_too_long "verify_stream"
            "did not consume the full stream";
        if not (equal_kinded_hash after (hash tree_after)) then
          Irmin_proof.bad_stream_exn "verify_stream" "invalid after hash";
        (tree_after, result))
      (function
        | Pruned_hash h ->
            Fmt.kstr
              (Irmin_proof.bad_stream_exn "verify_stream")
              "%s is trying to read through a blinded node or object (%a)"
              h.context pp_hash h.hash
        | e -> raise e)

  let verify_stream p f =
    Lwt.catch
      (fun () ->
        let+ r = verify_stream_exn p f in
        Ok r)
      (function
        | Irmin_proof.Bad_stream (Stream_too_long e) ->
            Fmt.kstr
              (fun e -> Lwt.return (Error (`Stream_too_long e)))
              "Bad_stream %s: %s" e.context e.reason
        | Irmin_proof.Bad_stream (Stream_too_short e) ->
            Fmt.kstr
              (fun e -> Lwt.return (Error (`Stream_too_short e)))
              "Bad_stream %s: %s" e.context e.reason
        | Irmin_proof.Bad_stream (Proof_mismatch e) ->
            Fmt.kstr
              (fun e -> Lwt.return (Error (`Proof_mismatch e)))
              "Bad_stream %s: %s" e.context e.reason
        | e -> Lwt.fail e)

  let hash_of_proof_state state =
    let env = Env.empty () in
    Proof.load_proof ~env state Fun.id

  module Private = struct
    let get_env = get_env

    module Env = Env
  end
end
