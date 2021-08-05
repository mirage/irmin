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

exception Backend_invariant_violation

module Make (P : Private.S) = struct
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

  module Path = P.Node.Path
  module Metadata = P.Node.Metadata

  module StepMap = struct
    module X = struct
      type t = Path.step [@@deriving irmin ~compare]
    end

    include Map.Make (X)

    let stdlib_merge = merge

    include Merge.Map (X)
  end

  type metadata = Metadata.t [@@deriving irmin ~equal]
  type key = Path.t [@@deriving irmin ~pp]
  type hash = P.Hash.t [@@deriving irmin ~pp ~equal]
  type step = Path.step [@@deriving irmin ~pp ~compare]
  type contents = P.Contents.Val.t [@@deriving irmin ~equal ~pp]
  type repo = P.Repo.t

  module Hashes = Hashtbl.Make (struct
    type t = P.Hash.t

    let hash x = P.Hash.short_hash x
    let equal = Type.(unstage (equal P.Hash.t))
  end)

  type marks = unit Hashes.t
  type 'a or_error = ('a, [ `Dangling_hash of hash ]) result
  type 'a force = [ `True | `False of key -> 'a -> 'a Lwt.t | `And_clear ]
  type uniq = [ `False | `True | `Marks of marks ]
  type 'a node_fn = key -> step list -> 'a -> 'a Lwt.t

  type depth = [ `Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int ]
  [@@deriving irmin]

  let dummy_marks = Hashes.create 0
  let empty_marks () = Hashes.create 39

  exception Dangling_hash of { context : string; hash : hash }

  let () =
    Printexc.register_printer (function
      | Dangling_hash { context; hash } ->
          Some
            (Fmt.str "Irmin.Tree.%s: encountered dangling hash %a" context
               pp_hash hash)
      | _ -> None)

  let get_ok : type a. string -> a or_error -> a =
   fun context -> function
    | Ok x -> x
    | Error (`Dangling_hash hash) -> raise (Dangling_hash { context; hash })

  type 'key ptr_option = Key of 'key | Hash of hash | Ptr_none
  (* NOTE: given the choice, we prefer caching [Key] over [Hash] as it can
     be used to avoid storing duplicate contents values on export. *)

  module Contents = struct
    type key = P.Contents.Key.t [@@deriving irmin]
    type v = Key of repo * key | Value of contents
    type nonrec ptr_option = key ptr_option
    type info = { mutable ptr : ptr_option; mutable value : contents option }
    type t = { mutable v : v; mutable info : info }

    let info_is_empty i = i.ptr = Ptr_none && i.value = None

    let v =
      let open Type in
      variant "Node.Contents.v" (fun key value (v : v) ->
          match v with Key (_, x) -> key x | Value v -> value v)
      |~ case1 "key" P.Contents.Key.t (fun _ -> assert false)
      |~ case1 "value" P.Contents.Val.t (fun v -> Value v)
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
      in
      let info = { ptr; value } in
      { v; info }

    let export ?clear:(c = true) repo t k =
      if c then clear t;
      t.info.ptr <- Key k;
      t.v <- Key (repo, k)

    let of_value c = of_v (Value c)
    let of_key repo k = of_v (Key (repo, k))

    let cached_hash t =
      match (t.v, t.info.ptr) with
      | Key (_, k), Ptr_none ->
          let h = Some (P.Contents.Key.to_hash k) in
          t.info.ptr <- Key k;
          h
      | _, Key k -> Some (P.Contents.Key.to_hash k)
      | _, Hash h -> Some h
      | _, Ptr_none -> None

    let cached_key t =
      match (t.v, t.info.ptr) with
      | Key (_, k), Ptr_none ->
          t.info.ptr <- Key k;
          Some k
      | _, Key k -> Some k
      | _, (Hash _ | Ptr_none) -> None

    let cached_value t =
      match (t.v, t.info.value) with
      | Value v, None ->
          let v = Some v in
          t.info.value <- v;
          v
      | _, v -> v

    let hash t =
      match cached_hash t with
      | Some k -> k
      | None -> (
          match cached_value t with
          | None -> assert false
          | Some v ->
              cnt.contents_hash <- cnt.contents_hash + 1;
              let h = P.Contents.Hash.hash v in
              assert (t.info.ptr = Ptr_none);
              t.info.ptr <- Hash h;
              h)

    let key t = match t.v with Key (_, k) -> Some k | _ -> None

    let value_of_key t repo k =
      cnt.contents_find <- cnt.contents_find + 1;
      P.Contents.find (P.Repo.contents_t repo) k >|= function
      | None -> Error (`Dangling_hash (P.Contents.Key.to_hash k))
      | Some v as some_v ->
          t.info.value <- some_v;
          Ok v

    let to_value t =
      match cached_value t with
      | Some v -> Lwt.return (Ok v)
      | None -> (
          match t.v with
          | Value v -> Lwt.return (Ok v)
          | Key (repo, k) -> value_of_key t repo k)

    let force = to_value

    let force_exn t =
      let+ v = force t in
      get_ok "force" v

    let equal_hash = Type.(unstage (equal P.Hash.t))
    let compare_hash = Type.(unstage (compare P.Hash.t))

    let equal (x : t) (y : t) =
      x == y
      ||
      match (cached_hash x, cached_hash y) with
      | Some x, Some y -> equal_hash x y
      | _ -> (
          match (cached_value x, cached_value y) with
          | Some x, Some y -> equal_contents x y
          | _ -> equal_hash (hash x) (hash y))

    let compare (x : t) (y : t) = compare_hash (hash x) (hash y)

    let t =
      Type.map ~equal:(Type.stage equal) ~compare:(Type.stage compare) v of_v
        (fun t -> t.v)

    let merge : t Merge.t =
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              let+ c = to_value old >|= Option.of_result in
              Ok (Some c))
        in
        let* x = to_value x >|= Option.of_result in
        let* y = to_value y >|= Option.of_result in
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
          f path (get_ok "fold" c) acc
      | `False skip -> (
          match cached_value t with
          | None -> skip path acc
          | Some c -> f path c acc)
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

    and t = { mutable v : v; mutable info : info }
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
      variant "Node.node" (fun map key value -> function
        | Map m -> map m | Key (_, y) -> key y | Value (_, v, m) -> value (v, m))
      |~ case1 "map" m (fun m -> Map m)
      |~ case1 "key" P.Node.Key.t (fun _ -> assert false)
      |~ case1 "value" (pair P.Node.Val.t (option um)) (fun _ -> assert false)
      |> sealv

    let info_is_empty i =
      i.map = None && i.value = None && i.findv_cache = None && i.ptr = Ptr_none

    let of_v v =
      let ptr, map, value =
        match v with
        | Map m -> (Ptr_none, Some m, None)
        | Key (_, k) -> (Key k, None, None)
        | Value (_, v, None) -> (Ptr_none, None, Some v)
        | Value _ -> (Ptr_none, None, None)
      in
      let findv_cache = None in
      let info = { ptr; map; value; findv_cache } in
      { v; info }

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
        i.ptr <- Ptr_none;
        i.findv_cache <- None);
      clear_maps ~max_depth depth [ map; added; findv ]

    and clear ~max_depth depth t = clear_info ~v:t.v ~max_depth depth t.info

    let clear ?depth:d n =
      match n.v with
      | Map m when StepMap.is_empty m -> ()
      | _ ->
          let max_depth =
            match d with None -> 0 | Some max_depth -> max_depth
          in
          clear ~max_depth 0 n

    let of_map m = of_v (Map m)
    let of_key repo k = of_v (Key (repo, k))
    let of_value ?updates repo v = of_v (Value (repo, v, updates))

    (* Use a stable represetation for empty trees. *)
    let empty = of_map StepMap.empty

    let map_of_value repo (n : value) : map =
      cnt.node_val_list <- cnt.node_val_list + 1;
      let entries = P.Node.Val.list n in
      let aux = function
        | `Node h -> `Node (of_key repo h)
        | `Contents (c, m) -> `Contents (Contents.of_key repo c, m)
      in
      List.fold_left
        (fun acc (k, v) -> StepMap.add k (aux v) acc)
        StepMap.empty entries

    let cached_hash t =
      match (t.v, t.info.ptr) with
      | Key (_, k), Ptr_none ->
          let h = Some (P.Node.Key.to_hash k) in
          t.info.ptr <- Key k;
          h
      | _, Key k -> Some (P.Node.Key.to_hash k)
      | _, Hash h -> Some h
      | _, Ptr_none -> None

    let cached_key t =
      match (t.v, t.info.ptr) with
      | Key (_, k), Ptr_none ->
          t.info.ptr <- Key k;
          Some k
      | _, Key k -> Some k
      | _, (Hash _ | Ptr_none) -> None

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

    open struct
      module Portable = P.Node_portable
      module Portable_hash = Hash.Typed (P.Hash) (P.Node_portable)
    end

    let rec hash : type a. t -> (hash -> a) -> a =
     fun t k ->
      match cached_hash t with
      | Some h -> k h
      | None -> (
          let a_of_hashable hash v =
            cnt.node_hash <- cnt.node_hash + 1;
            assert (t.info.ptr = Ptr_none);
            let hash = hash v in
            t.info.ptr <- Hash hash;
            k hash
          in
          match cached_value t with
          | Some v -> a_of_hashable P.Node.Hash.hash v
          | None -> (
              match t.v with
              | Key (_, h) -> k (P.Node.Key.to_hash h)
              | Value (_, v, None) -> a_of_hashable P.Node.Hash.hash v
              | Value (_, v, Some um) ->
                  portable_value_of_updates t v um (fun v ->
                      a_of_hashable Portable_hash.hash v)
              | Map m ->
                  portable_value_of_map t m (fun v ->
                      a_of_hashable Portable_hash.hash v)))

    and portable_value_of_map : type r. t -> map -> (Portable.t, r) cont =
     fun t map k ->
      if StepMap.is_empty map then (
        t.info.value <- Some P.Node.Val.empty;
        k P.Node.Val.(Portable.of_node empty))
      else
        let alist = StepMap.bindings map in
        let rec aux acc = function
          | [] ->
              cnt.node_val_v <- cnt.node_val_v + 1;
              let v = P.Node_portable.v (List.rev acc) in
              (* t.info.value <- Some v; XXX *)
              k v
          | (step, v) :: rest -> (
              match v with
              | `Contents (c, m) ->
                  let hash =
                    match Contents.key c with
                    | Some key -> P.Contents.Key.to_hash key
                    | None -> Contents.hash c
                  in
                  let v : P.Node_portable.value = `Contents (hash, m) in
                  (aux [@tailcall]) ((step, v) :: acc) rest
              | `Node n -> (
                  let return hash = aux ((step, `Node hash) :: acc) rest in
                  match key n with
                  | Some key -> return (P.Node.Key.to_hash key)
                  | None -> hash n return))
        in
        aux [] alist

    and portable_value_of_elt : type r. elt -> (P.Node_portable.value, r) cont =
     fun e k ->
      match e with
      | `Contents (c, m) ->
          let hash = Contents.hash c in
          k (`Contents (hash, m))
      | `Node n -> hash n (fun h -> k (`Node h))

    (* and portable_value_of_updates : type r. t -> value -> _ -> (P.Node_portable.t, r) cont
     *     fun t v updates k ->
     *       let updates = StepMap.bindings updates in
     *       let rec aux = function
     *         | *)
    and portable_value_of_updates :
        type r. t -> value -> updatemap -> (P.Node_portable.t, r) cont =
     fun _t v updates k ->
      let updates = StepMap.bindings updates in
      let rec aux acc = function
        | [] -> k acc
        | (k, Add e) :: rest ->
            portable_value_of_elt e (fun e ->
                aux (P.Node_portable.add acc k e) rest)
        | (k, Remove) :: rest -> aux (P.Node_portable.remove acc k) rest
      in
      aux (P.Node_portable.of_node v) updates

    let hash k = hash k (fun x -> x)

    let value_of_key t repo k =
      match cached_value t with
      | Some v -> Lwt.return_ok v
      | None -> (
          cnt.node_find <- cnt.node_find + 1;
          P.Node.find (P.Repo.node_t repo) k >|= function
          | None -> Error (`Dangling_hash (P.Node.Key.to_hash k))
          | Some v as some_v ->
              t.info.value <- some_v;
              Ok v)

    let to_value t =
      match cached_value t with
      | Some v -> ok (P.Node_portable.of_node v)
      | None -> (
          match t.v with
          | Value (_, v, None) -> ok (P.Node_portable.of_node v)
          | Value (_, v, Some um) -> portable_value_of_updates t v um ok
          | Map m -> portable_value_of_map t m ok
          | Key (repo, h) ->
              value_of_key t repo h |> Lwt_result.map P.Node_portable.of_node)

    let to_map t =
      match cached_map t with
      | Some m -> Lwt.return (Ok m)
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
          | Map m -> Lwt.return (Ok m)
          | Value (repo, v, m) -> Lwt.return (Ok (of_value repo v m))
          | Key (repo, k) -> (
              value_of_key t repo k >|= function
              | Error _ as e -> e
              | Ok v -> Ok (of_value repo v None)))

    let equal_hash = Type.(unstage (equal P.Hash.t))
    let compare_hash = Type.(unstage (compare P.Hash.t))

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
              | _ -> equal_hash (hash x) (hash y)))

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

    let compare (x : t) (y : t) =
      match maybe_equal x y with
      | True -> 0
      | _ -> compare_hash (hash x) (hash y)

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

    let apply_updates updates v =
      StepMap.fold
        (fun k v acc ->
          match v with
          | Remove -> P.Node.Val.remove acc k
          | Add (`Node n) -> (
              match cached_key n with
              | None -> assert false
              | Some ptr -> P.Node.Val.add acc k (`Node ptr))
          | Add (`Contents (c, m)) -> (
              match Contents.cached_key c with
              | None -> assert false
              | Some ptr -> P.Node.Val.add acc k (`Contents (ptr, m))))
        updates v

    let length _ = assert false
    (* match cached_map t with
     * | Some m -> StepMap.cardinal m |> Lwt.return
     * | None -> (
     *     match cached_value t with
     *     | Some v -> P.Node.Val.length v |> Lwt.return
     *     | None -> (
     *         match t.v with
     *         | Map m -> StepMap.cardinal m |> Lwt.return
     *         | Key (r, k) ->
     *             value_of_key t r k >|= get_ok "length" >|= P.Node.Val.length
     *         | Value (_, v, None) -> P.Node.Val.length v |> Lwt.return
     *         | Value (_, v, Some u) ->
     *             value_of_updates t v u (fun x ->
     *                 P.Node.Val.length x |> Lwt.return))) *)

    let is_empty =
      let empty_hash = hash (of_map StepMap.empty) in
      fun t ->
        match cached_map t with
        | Some m -> StepMap.is_empty m
        | None -> (
            match cached_value t with
            | Some v -> P.Node.Val.is_empty v
            | None -> (
                match t.v with
                | Value (_, v, Some um) -> is_empty_after_updates v um
                | Key (_, key) -> equal_hash (P.Node.Key.to_hash key) empty_hash
                | Map _ -> assert false (* [cached_map <> None] *)
                | Value (_, _, None) ->
                    assert false (* [cached_value <> None] *)))

    let add_to_findv_cache t step v =
      match t.info.findv_cache with
      | None -> t.info.findv_cache <- Some (StepMap.singleton step v)
      | Some m -> t.info.findv_cache <- Some (StepMap.add step v m)

    let findv ctx t step =
      let of_map m = try Some (StepMap.find step m) with Not_found -> None in
      let of_value repo v =
        match P.Node.Val.find v step with
        | None -> None
        | Some (`Contents (c, m)) ->
            let c = Contents.of_key repo c in
            let (v : elt) = `Contents (c, m) in
            add_to_findv_cache t step v;
            Some v
        | Some (`Node n) ->
            let n = of_key repo n in
            let v = `Node n in
            add_to_findv_cache t step v;
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
                let+ v = value_of_key t repo h >|= get_ok ctx in
                of_value repo v)
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
              let n = `Node (of_key repo n) in
              (k, n) :: acc
          | `Contents (c, m) ->
              let c = Contents.of_key repo c in
              (k, `Contents (c, m)) :: acc)
        [] (List.rev t)

    let list ?offset ?length t : (step * elt) list or_error Lwt.t =
      match cached_map t with
      | Some m -> ok (list_of_map ?offset ?length m)
      | None -> (
          match t.v with
          | Value (repo, n, None) -> ok (list_of_value ?offset ?length repo n)
          | Key (repo, h) -> (
              value_of_key t repo h >>= function
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
      path:Path.t -> 'acc -> int -> 'v -> ('acc, 'r) cont_lwt
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
        node:(Path.t -> _ -> acc -> acc Lwt.t) ->
        contents:(Path.t -> contents -> acc -> acc Lwt.t) ->
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
          | `True | `And_clear ->
              let* m = to_map t >|= get_ok "fold" in
              if force = `And_clear then clear ~depth:0 t;
              (map [@tailcall]) ~path acc d (Some m) k
          | `False skip -> (
              match cached_map t with
              | Some n -> (map [@tailcall]) ~path acc d (Some n) k
              | None -> skip path acc >>= k)
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
              let+ v = value_of_key t repo h >|= get_ok "update" in
              of_value repo v StepMap.empty)

    let t node =
      Type.map ~equal:(Type.stage equal) ~compare:(Type.stage compare) node of_v
        (fun t -> t.v)

    let _, t =
      Type.mu2 (fun _ y ->
          let elt = elt_t y in
          let v = v_t elt in
          let t = t v in
          (v, t))

    let elt_t = elt_t t
    let dump = Type.pp_dump t
    let remove t step = update t step Remove

    let add t step v =
      let* r = update t step (Add v) in
      Lwt.return r

    let rec merge : type a. (t Merge.t -> a) -> a =
     fun k ->
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              let+ m = to_map old >|= Option.of_result in
              Ok (Some m))
        in
        let* x = to_map x >|= Option.of_result in
        let* y = to_map y >|= Option.of_result in
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

    (* export t to the given repo and clear the cache *)
    let export ?clear:(c = true) repo t k =
      if c then clear t;
      let is_empty =
        match t.v with
        | Value (_, v, None) when P.Node.Val.is_empty v -> true
        | Map m -> StepMap.is_empty m
        | _ -> false
      in
      if is_empty then t.info.value <- Some P.Node.Val.empty;
      t.info.ptr <- Key k;
      t.v <- Key (repo, k)
  end

  type node = Node.t [@@deriving irmin ~pp]
  type node_key = Node.key [@@deriving irmin ~pp]
  type contents_key = Contents.key [@@deriving irmin ~pp]

  type kinded_key = [ `Contents of Contents.key * metadata | `Node of Node.key ]
  [@@deriving irmin]

  type t = [ `Node of node | `Contents of Contents.t * Metadata.t ]
  [@@deriving irmin]

  let to_private_portable_node n =
    Node.to_value n >|= get_ok "to_private_portable_node"

  let of_private_node repo n = Node.of_value repo n

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

  let is_empty = function `Node n -> Node.is_empty n | `Contents _ -> false

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

  let sub ctx t path =
    let rec aux node path =
      match Path.decons path with
      | None -> Lwt.return_some node
      | Some (h, p) -> (
          Node.findv ctx node h >>= function
          | None | Some (`Contents _) -> Lwt.return_none
          | Some (`Node n) -> (aux [@tailcall]) n p)
    in
    match t with
    | `Node n -> (aux [@tailcall]) n path
    | `Contents _ -> Lwt.return_none

  let find_tree (t : t) path =
    Log.debug (fun l -> l "Tree.find_tree %a" pp_key path);
    match (t, Path.rdecons path) with
    | v, None -> Lwt.return_some v
    | _, Some (path, file) -> (
        sub "find_tree.sub" t path >>= function
        | None -> Lwt.return_none
        | Some n -> Node.findv "find_tree.findv" n file)

  let noop _ _ acc = Lwt.return acc

  let fold ?(force = `And_clear) ?(uniq = `False) ?(pre = noop) ?(post = noop)
      ?depth ?(contents = noop) ?(node = noop) (t : t) acc =
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
    Fmt.kstrf invalid_arg "Irmin.Tree.%s: %a not found" n pp_key k

  let get_tree (t : t) path =
    find_tree t path >|= function
    | None -> err_not_found "get_tree" path
    | Some v -> v

  let find_all t k =
    find_tree t k >>= function
    | None | Some (`Node _) -> Lwt.return_none
    | Some (`Contents (c, m)) ->
        let+ c = Contents.to_value c in
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
    Log.debug (fun l -> l "Tree.kind %a" pp_key path);
    match (t, Path.rdecons path) with
    | `Contents _, None -> Lwt.return_some `Contents
    | `Node _, None -> Lwt.return_some `Node
    | _, Some (dir, file) -> (
        sub "kind.sub" t dir >>= function
        | None -> Lwt.return_none
        | Some m -> (
            Node.findv "kind.findv" m file >>= function
            | None -> Lwt.return_none
            | Some (`Contents _) -> Lwt.return_some `Contents
            | Some (`Node _) -> Lwt.return_some `Node))

  let length = Node.length

  let list t ?offset ?length path : (step * t) list Lwt.t =
    Log.debug (fun l -> l "Tree.list %a" pp_key path);
    sub "list.sub" t path >>= function
    | None -> Lwt.return []
    | Some n -> (
        Node.list ?offset ?length n >|= function Error _ -> [] | Ok l -> l)

  let empty = `Node Node.empty

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

  let update_tree ~f_might_return_empty_node ~f root_tree path =
    (* User-introduced empty nodes will be removed immediately if necessary. *)
    let prune_empty : node -> bool =
      if not f_might_return_empty_node then Fun.const false else Node.is_empty
    in
    match Path.rdecons path with
    | None -> (
        let empty_tree =
          match is_empty root_tree with
          | true -> root_tree
          | false -> `Node (Node.of_map StepMap.empty)
          (* XXX: Node.empty *)
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
                Node.findv "update_tree.findv" parent_node file
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
          | Some (step, key_suffix) -> (
              let* old_binding =
                Node.findv "update_tree.findv" parent_node step
              in
              let to_recurse =
                match old_binding with
                | Some (`Node child) -> child
                | None | Some (`Contents _) -> Node.empty
              in
              (aux [@tailcall]) key_suffix to_recurse @@ function
              | Unchanged ->
                  (* This includes [remove]s in an empty node, in which case we
                     want to avoid adding a binding anyway. *)
                  k Unchanged
              | Changed child -> (
                  match Node.is_empty child with
                  | true ->
                      (* A [remove] has emptied previously non-empty child with
                         binding [h], so we remove the binding. *)
                      Node.remove parent_node step >>= changed
                  | false -> Node.add parent_node step (`Node child) >>= changed
                  ))
        in
        let top_node =
          match root_tree with `Node n -> n | `Contents _ -> Node.empty
        in
        aux path top_node @@ function
        | Unchanged -> Lwt.return root_tree
        | Changed node -> Lwt.return (`Node node))

  let update t k ?(metadata = Metadata.default) f =
    Log.debug (fun l -> l "Tree.update %a" pp_key k);
    update_tree t k ~f_might_return_empty_node:false ~f:(fun t ->
        let+ old_contents =
          match t with
          | Some (`Node _) | None -> Lwt.return_none
          | Some (`Contents (c, _)) ->
              let+ c = Contents.to_value c in
              Some (get_ok "update" c)
        in
        match f old_contents with
        | None -> None
        | Some c -> Some (`Contents (Contents.of_value c, metadata)))

  let add t k ?(metadata = Metadata.default) c =
    Log.debug (fun l -> l "Tree.add %a" pp_key k);
    update_tree t k
      ~f:(fun _ -> Lwt.return_some (`Contents (Contents.of_value c, metadata)))
      ~f_might_return_empty_node:false

  let add_tree t k v =
    Log.debug (fun l -> l "Tree.add_tree %a" pp_key k);
    update_tree t k
      ~f:(fun _ -> Lwt.return_some v)
      ~f_might_return_empty_node:true

  let remove t k =
    Log.debug (fun l -> l "Tree.remove %a" pp_key k);
    update_tree t k
      ~f:(fun _ -> Lwt.return_none)
      ~f_might_return_empty_node:false

  let update_tree t k f =
    Log.debug (fun l -> l "Tree.update_tree %a" pp_key k);
    update_tree t k ~f:(Lwt.wrap1 f) ~f_might_return_empty_node:true

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
    (* XXX Logs.warn (fun f ->
     *     f "@[<v 0>Exporting (%s)@,%a@, which has hash: %a@]@." __LOC__
     *       (Type.pp_dump Node.t) n pp_hash (Node.hash n)); *)

    (* Keep track of the hashes we've seen to avoid duplicate exporting. After
       finishing the export of a value, we must add its key so that duplicate
       occurrences can reference it. *)
    let seen : [ `Contents of contents_key | `Node of node_key ] Hashes.t =
      Hashes.create 127
    in

    let add_node n v () =
      cnt.node_add <- cnt.node_add + 1;
      let+ k = P.Node.add node_t v in
      let () =
        let h = P.Node.Key.to_hash k in
        let h' = Node.hash n in
        if not (Node.equal_hash h h') then (
          Logs.err (fun f ->
              f
                "@[<v 2>Tree.export: added inconsistent node binding@,\
                 key: %a@,\
                 value: %a@,\
                 computed hash: %a@]" pp_node_key k Node.pp_value v pp_hash h');
          raise Backend_invariant_violation)
      in

      Node.export ?clear repo n k;
      k
    in

    let add_contents c x () =
      cnt.contents_add <- cnt.contents_add + 1;
      let+ k = P.Contents.add contents_t x in
      (* XXX Logs.debug (fun f ->
       *     f "Tree.export: added contents binding %a -> %a" pp_contents_key k
       *       pp_contents x); *)
      assert (Contents.equal_hash (P.Contents.Key.to_hash k) (Contents.hash c));
      Contents.export ?clear repo c k;
      k
    in

    let add_node_map n (x : Node.map) () =
      let node =
        (* Since we traverse in post-order, all children of [x] have already been
           added. Thus, their keys are cached in memory, and we can retrieve
           them. *)
        StepMap.map
          (function
            | `Node n -> (
                match Node.cached_key n with
                | None -> assert false
                | Some k -> `Node k)
            | `Contents (c, m) -> (
                match Contents.cached_key c with
                | None ->
                    Logs.err (fun f ->
                        f "Not cached: %a" (Type.pp_dump Contents.t) c);
                    assert false (* XXX *)
                | Some k -> `Contents (k, m)))
          x
        |> StepMap.to_rev_seq
        |> Seq.fold_left (fun acc x -> x :: acc) []
        |> P.Node.Val.v
      in
      add_node n node ()
    in

    let add_updated_node n (v : Node.value) (updates : Node.updatemap) () =
      let updated_node = Node.apply_updates updates v in
      add_node n updated_node ()
    in

    let todo : (unit -> unit Lwt.t) Stack.t = Stack.create () in
    let rec add_to_todo : type a. _ -> (unit -> a Lwt.t) -> a Lwt.t =
     fun n k ->
      let h = Node.hash n in
      match Hashes.mem seen h with
      | true -> k ()
      | false -> (
          match n.Node.v with
          | Node.Key (_, key) ->
              Node.export ?clear repo n key;
              Hashes.add seen h (`Node key);
              k ()
          | Node.Value (_, x, None) ->
              (* We don't add the hash until the node job is finished. This is
                 OK only becuase the hash tree is acyclic. *)
              Stack.push
                (fun () ->
                  let+ key = add_node n x () in
                  Hashes.add seen h (`Node key))
                todo;
              k ()
          | Map _ | Value (_, _, Some _) -> (
              let add_children () =
                match n.v with
                | Key _ | Value (_, _, None) ->
                    (* might happen if the node has already been added
                       (while the thread was blocked on [P.Node.index] or
                       [P.Node.mem] below). *)
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
                    add_steps_to_todo l n k
              in
              cnt.node_index <- cnt.node_index + 1;
              P.Node.index node_t h >>= function
              | None -> add_children ()
              | Some key -> (
                  cnt.node_mem <- cnt.node_mem + 1;
                  P.Node.mem node_t key >>= function
                  | false -> add_children ()
                  | true ->
                      Node.export ?clear repo n key;
                      k ()
                  (* NOTE: it's possible that this value already has a key in the
                     store, but it's not indexed. If so, we're adding a duplicate
                     here – this isn't an issue for correctness, but does waste
                     space. *))))
    and add_steps_to_todo : type a. _ -> _ -> (unit -> a Lwt.t) -> a Lwt.t =
     fun l n k ->
      (* 1. convert partial values to total values *)
      (* XXX *)
      (* let () =
       *   match n.Node.v with
       *   | Value (repo, v, Some um) ->
       *       Node.value_of_updates n v um (fun v -> n.v <- Value (repo, v, None))
       *   | _ -> ()
       * in *)
      (* 2. push the current node job on the stack. *)
      let () =
        match (n.Node.v, Node.cached_value n) with
        | Value (_, v, Some um), _ ->
            Stack.push
              (fun () ->
                let+ _ = add_updated_node n v um () in
                ())
              todo
        | Map x, _ ->
            Stack.push
              (fun () ->
                let+ _ = add_node_map n x () in
                ())
              todo
        | _, Some v ->
            Stack.push
              (fun () ->
                let+ _ = add_node n v () in
                ())
              todo
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
          else
            match c.Contents.v with
            | Contents.Key (_, key) -> Hashes.add seen h (`Contents key)
            | Contents.Value x ->
                Stack.push
                  (fun () ->
                    let+ key = add_contents c x () in
                    Hashes.add seen h (`Contents key))
                  todo)
        !contents;

      (* 3. push the children jobs on the stack. *)
      List.iter
        (fun n ->
          Stack.push (fun () -> (add_to_todo [@tailcall]) n Lwt.return) todo)
        !nodes;
      k ()
    in

    let rec loop () =
      let task = try Some (Stack.pop todo) with Stack.Empty -> None in
      match task with None -> Lwt.return_unit | Some t -> t () >>= loop
    in
    (add_to_todo [@tailcall]) n @@ fun () ->
    loop () >|= fun () ->
    match Node.key n with
    | Some x ->
        (* XXX Log.debug (fun l -> l "Tree.export -> %a" Node.pp_key x); *)
        x
    | None -> failwith "export"

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
          let* childs = Node.bindings h >|= get_ok "entries" in
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

  let diff_node (x : node) (y : node) =
    let bindings n =
      Node.to_map n >|= function
      | Ok m -> Ok (StepMap.bindings m)
      | Error _ as e -> e
    in
    let removed acc (k, (c, m)) =
      let+ c = Contents.to_value c >|= get_ok "diff_node" in
      (k, `Removed (c, m)) :: acc
    in
    let added acc (k, (c, m)) =
      let+ c = Contents.to_value c >|= get_ok "diff_node" in
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
          let* c1 = Contents.to_value c1 >|= get_ok "diff" in
          let* c2 = Contents.to_value c2 >|= get_ok "diff" in
          Lwt.return [ (Path.empty, `Updated ((c1, m1), (c2, m2))) ]
    | `Node x, `Node y -> diff_node x y
    | `Contents (x, m), `Node y ->
        let* diff = diff_node Node.empty y in
        let+ x = Contents.to_value x >|= get_ok "diff" in
        (Path.empty, `Removed (x, m)) :: diff
    | `Node x, `Contents (y, m) ->
        let* diff = diff_node x Node.empty in
        let+ y = Contents.to_value y >|= get_ok "diff" in
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
    (concrete [@tailcall]) c (function Empty -> empty | Non_empty x -> x)

  let to_concrete t =
    let rec tree : type r. t -> (concrete, r) cont_lwt =
     fun t k ->
      match t with
      | `Contents c -> contents c k
      | `Node n ->
          let* m = Node.to_map n in
          let bindings = m |> get_ok "to_concrete" |> StepMap.bindings in
          (node [@tailcall]) [] bindings (fun n ->
              let n = List.sort (fun (s, _) (s', _) -> compare_step s s') n in
              k (`Tree n))
    and contents : type r. Contents.t * metadata -> (concrete, r) cont_lwt =
     fun (c, m) k ->
      let* c = Contents.to_value c >|= get_ok "to_concrete" in
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
    Log.debug (fun l -> l "Tree.key");
    match t with
    | `Node n -> (
        match Node.key n with Some key -> Some (`Node key) | None -> None)
    | `Contents (c, m) -> (
        match Contents.key c with
        | Some key -> Some (`Contents (key, m))
        | None -> None)

  let hash (t : t) =
    Log.debug (fun l -> l "Tree.hash");
    match t with `Node n -> Node.hash n | `Contents (c, _) -> Contents.hash c

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
          | Key _ -> `Key)
end
