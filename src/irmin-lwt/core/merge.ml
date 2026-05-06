(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Printf

let src = Logs.Src.create "irmin.merge" ~doc:"Irmin merging"

module Log = (val Logs.src_log src : Logs.LOG)

type conflict = [ `Conflict of string ]
type 'a promise = unit -> ('a option, conflict) result Lwt.t

let promise t : 'a promise = fun () -> Lwt.return (Ok (Some t))

let memo fn =
  let r = ref None in
  fun () ->
    match !r with
    | Some x -> x
    | None ->
        let* x = fn () in
        r := Some (Lwt.return x);
        Lwt.return x

type 'a f = old:'a promise -> 'a -> 'a -> ('a, conflict) result Lwt.t
type 'a t = 'a Type.t * 'a f

let v t f = (t, f)
let f (x : 'a t) = snd x

let conflict fmt =
  ksprintf
    (fun msg ->
      [%log.debug "conflict: %s" msg];
      Lwt.return (Error (`Conflict msg)))
    fmt

let bind x f = x >>= function Error e -> Lwt.return (Error e) | Ok x -> f x
let map f x = x >|= function Error _ as x -> x | Ok x -> Ok (f x)

let map_promise f t () =
  t () >|= function
  | Error _ as x -> x
  | Ok None -> Ok None
  | Ok (Some a) -> Ok (Some (f a))

let bind_promise t f () =
  t () >>= function
  | Error e -> Lwt.return (Error e)
  | Ok None -> Lwt.return (Ok None)
  | Ok (Some a) -> f a ()

let ok x = Lwt.return (Ok x)

module Infix = struct
  let ( >>=* ) = bind
  let ( >|=* ) x f = map f x
  let ( >>=? ) = bind_promise
  let ( >|=? ) x f = map_promise f x
end

open Infix

let default (type a) (t : a Type.t) : a t =
  let pp = Type.pp t and equal = Type.(unstage (equal t)) in
  ( t,
    fun ~old t1 t2 ->
      let open Infix in
      [%log.debug "default %a | %a" pp t1 pp t2];
      old () >>=* function
      | None -> conflict "default: add/add and no common ancestor"
      | Some old ->
          [%log.debug "default old=%a" pp t1];
          if equal old t1 && equal t1 t2 then ok t1
          else if equal old t1 then ok t2
          else if equal old t2 then ok t1
          else conflict "default" )

let idempotent dt =
  let equal = Type.(unstage (equal dt)) in
  let default = default dt in
  let f ~old x y = if equal x y then ok x else f default ~old x y in
  v dt f

let seq = function
  | [] -> invalid_arg "nothing to merge"
  | (t, _) :: _ as ts ->
      ( t,
        fun ~old v1 v2 ->
          Lwt_list.fold_left_s
            (fun acc (_, merge) ->
              match acc with Ok x -> ok x | Error _ -> merge ~old v1 v2)
            (Error (`Conflict "nothing to merge"))
            ts )

let option (type a) ((a, t) : a t) : a option t =
  let pp_a = Type.pp a and equal = Type.(unstage (equal a)) in
  let dt = Type.option a in
  let pp = Type.pp dt in
  ( dt,
    fun ~old t1 t2 ->
      [%log.debug "some %a | %a" pp t1 pp t2];
      f (default Type.(option a)) ~old t1 t2 >>= function
      | Ok x -> ok x
      | Error _ -> (
          match (t1, t2) with
          | None, None -> ok None
          | Some v1, Some v2 ->
              let open Infix in
              let old () =
                old () >>=* function
                | None -> ok None
                | Some o ->
                    [%log.debug "option old=%a" pp o];
                    ok o
              in
              t ~old v1 v2 >|=* fun x -> Some x
          | Some x, None | None, Some x -> (
              let open Infix in
              old () >>=* function
              | None | Some None -> ok (Some x)
              | Some (Some o) ->
                  [%log.debug "option old=%a" pp_a o];
                  if equal x o then ok (Some x) else conflict "option: add/del")
          ) )

let pair (da, a) (db, b) =
  let dt = Type.pair da db in
  let pp = Type.pp dt in
  ( dt,
    fun ~old x y ->
      [%log.debug "pair %a | %a" pp x pp y];
      (snd (default dt)) ~old x y >>= function
      | Ok x -> ok x
      | Error _ ->
          let (a1, b1), (a2, b2) = (x, y) in
          let o1 = map_promise fst old in
          let o2 = map_promise snd old in
          a ~old:o1 a1 a2 >>=* fun a3 ->
          b ~old:o2 b1 b2 >|=* fun b3 -> (a3, b3) )

let triple (da, a) (db, b) (dc, c) =
  let dt = Type.triple da db dc in
  let pp = Type.pp dt in
  ( dt,
    fun ~old x y ->
      [%log.debug "triple %a | %a" pp x pp y];
      (snd (default dt)) ~old x y >>= function
      | Ok x -> ok x
      | Error _ ->
          let (a1, b1, c1), (a2, b2, c2) = (x, y) in
          let o1 = map_promise (fun (x, _, _) -> x) old in
          let o2 = map_promise (fun (_, x, _) -> x) old in
          let o3 = map_promise (fun (_, _, x) -> x) old in
          a ~old:o1 a1 a2 >>=* fun a3 ->
          b ~old:o2 b1 b2 >>=* fun b3 ->
          c ~old:o3 c1 c2 >|=* fun c3 -> (a3, b3, c3) )

exception C of string

let merge_elt merge_v old key vs =
  let v1, v2 =
    match vs with
    | `Left v -> (Some v, None)
    | `Right v -> (None, Some v)
    | `Both (v1, v2) -> (Some v1, Some v2)
  in
  let old () = old key in
  merge_v key ~old v1 v2 >>= function
  | Error (`Conflict msg) -> Lwt.fail (C msg)
  | Ok x -> Lwt.return x

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
            aux t1 t2
        | x ->
            if x < 0 then (
              f k1 (`Left v1);
              aux t1 l2)
            else (
              f k2 (`Right v2);
              aux l1 t2))
  in
  aux l1 l2

(* assume l1 and l2 are key-sorted *)
let alist_iter2_lwt compare_k f l1 l2 =
  let l3 = ref [] in
  alist_iter2 compare_k (fun left right -> l3 := f left right :: !l3) l1 l2;
  Lwt_list.iter_p Fun.id (List.rev !l3)

(* DO NOT assume l1 and l2 are key-sorted *)
let alist_merge_lwt compare_k f l1 l2 =
  let open Lwt in
  let l3 = ref [] in
  let sort l = List.sort (fun (x, _) (y, _) -> compare_k x y) l in
  let l1 = sort l1 in
  let l2 = sort l2 in
  let f key data =
    f key data >>= function
    | None -> return_unit
    | Some v ->
        l3 := (key, v) :: !l3;
        return_unit
  in
  alist_iter2_lwt compare_k f l1 l2 >>= fun () -> return !l3

let alist dx dy merge_v =
  let pair = Type.pair dx dy in
  let compare_pair = Type.unstage (Type.compare pair) in
  let compare_dx = Type.(unstage (compare dx)) in
  let dt = Type.list pair in
  ( dt,
    fun ~old x y ->
      let pp = Type.pp dt in
      [%log.debug "alist %a | %a" pp x pp y];
      let sort = List.sort compare_pair in
      let x = sort x in
      let y = sort y in
      let old k =
        let open Infix in
        old () >|=* function
        | None -> Some None (* no parent = parent with empty value *)
        | Some old ->
            let old = try Some (List.assoc k old) with Not_found -> None in
            Some old
      in
      let merge_v k = f (merge_v k) in
      Lwt.catch
        (fun () ->
          alist_merge_lwt compare_dx (merge_elt merge_v old) x y >>= ok)
        (function C msg -> conflict "%s" msg | e -> Lwt.fail e) )

module MultiSet (K : sig
  include Set.OrderedType

  val t : t Type.t
end) =
struct
  module M = Map.Make (K)

  let of_alist l = List.fold_left (fun map (k, v) -> M.add k v map) M.empty l
  let t = Type.map Type.(list (pair K.t int64)) of_alist M.bindings

  let merge ~old m1 m2 =
    let get k m = try M.find k m with Not_found -> 0L in
    let set k v m = match v with 0L -> M.remove k m | _ -> M.add k v m in
    let add k v m = set k (Int64.add v @@ get k m) m in
    let keys = ref M.empty in
    old () >|=* fun old ->
    let old =
      match old with
      | None -> M.empty (* no parent = parent with empty value *)
      | Some o -> o
    in
    M.iter (fun k v -> keys := add k (Int64.neg v) !keys) old;
    M.iter (fun k v -> keys := add k v !keys) m1;
    M.iter (fun k v -> keys := add k v !keys) m2;
    !keys

  let merge = (t, merge)
end

module Set (K : sig
  include Set.OrderedType

  val t : t Type.t
end) =
struct
  module S = Set.Make (K)

  let of_list l = List.fold_left (fun set elt -> S.add elt set) S.empty l
  let t = Type.(map @@ list K.t) of_list S.elements
  let pp = Type.pp t

  let merge ~old x y =
    [%log.debug "merge %a %a" pp x pp y];
    old () >|=* fun old ->
    let old = match old with None -> S.empty | Some o -> o in
    let ( ++ ) = S.union and ( -- ) = S.diff in
    let to_add = x -- old ++ (y -- old) in
    let to_del = old -- x ++ (old -- y) in
    old -- to_del ++ to_add

  let merge = (t, merge)
end

module Map (K : sig
  include Map.OrderedType

  val t : t Type.t
end) =
struct
  module M = Map.Make (K)

  let of_alist l = List.fold_left (fun map (k, v) -> M.add k v map) M.empty l
  let t x = Type.map Type.(list @@ pair K.t x) of_alist M.bindings
  let iter2 f t1 t2 = alist_iter2 K.compare f (M.bindings t1) (M.bindings t2)

  let iter2 f m1 m2 =
    let m3 = ref [] in
    iter2 (fun key data -> m3 := f key data :: !m3) m1 m2;
    Lwt_list.iter_p (fun b -> b >>= fun () -> Lwt.return_unit) (List.rev !m3)

  let merge_maps f m1 m2 =
    let l3 = ref [] in
    let f key data =
      f key data >|= function None -> () | Some v -> l3 := (key, v) :: !l3
    in
    iter2 f m1 m2 >>= fun () ->
    let m3 = of_alist !l3 in
    Lwt.return m3

  let merge dv (merge_v : K.t -> 'a option t) =
    let pp ppf m = Type.(pp (list (pair K.t dv))) ppf @@ M.bindings m in
    let merge_v k = f (merge_v k) in
    ( t dv,
      fun ~old m1 m2 ->
        [%log.debug "assoc %a | %a" pp m1 pp m2];
        Lwt.catch
          (fun () ->
            let old key =
              old () >>=* function
              | None -> ok None
              | Some old ->
                  [%log.debug "assoc old=%a" pp old];
                  let old =
                    try Some (M.find key old) with Not_found -> None
                  in
                  ok (Some old)
            in
            merge_maps (merge_elt merge_v old) m1 m2 >>= ok)
          (function C msg -> conflict "%s" msg | e -> Lwt.fail e) )
end

let like da t a_to_b b_to_a =
  let pp = Type.pp da in
  let merge ~old a1 a2 =
    [%log.debug "biject %a | %a" pp a1 pp a2];
    try
      let b1 = a_to_b a1 in
      let b2 = a_to_b a2 in
      let old = memo (map_promise a_to_b old) in
      (f t) ~old b1 b2 >|=* b_to_a
    with Not_found -> conflict "biject"
  in
  seq [ default da; (da, merge) ]

let like_lwt (type a b) da (t : b t) (a_to_b : a -> b Lwt.t)
    (b_to_a : b -> a Lwt.t) : a t =
  let pp = Type.pp da in
  let merge ~old a1 a2 =
    [%log.debug "biject' %a | %a" pp a1 pp a2];
    try
      let* b1 = a_to_b a1 in
      let* b2 = a_to_b a2 in
      let old =
        memo (fun () ->
            bind (old ()) @@ function
            | None -> ok None
            | Some a ->
                let+ b = a_to_b a in
                Ok (Some b))
      in
      bind ((f t) ~old b1 b2) @@ fun b3 -> b_to_a b3 >>= ok
    with Not_found -> conflict "biject'"
  in
  seq [ default da; (da, merge) ]

let unit = default Type.unit
let bool = default Type.bool
let char = default Type.char
let int32 = default Type.int32
let int64 = default Type.int64
let float = default Type.float
let string = default Type.string

type counter = int64

let counter =
  ( Type.int64,
    fun ~old x y ->
      old () >|=* fun old ->
      let old = match old with None -> 0L | Some o -> o in
      let ( + ) = Int64.add and ( - ) = Int64.sub in
      x + y - old )

let with_conflict rewrite (d, f) =
  let f ~old x y =
    f ~old x y >>= function
    | Error (`Conflict msg) -> conflict "%s" (rewrite msg)
    | Ok x -> ok x
  in
  (d, f)

let conflict_t =
  Type.(map string) (fun x -> `Conflict x) (function `Conflict x -> x)

type nonrec 'a result = ('a, conflict) result [@@deriving irmin]
