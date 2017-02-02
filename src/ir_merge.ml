(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Printf
open Result

let src = Logs.Src.create "irmin.merge" ~doc:"Irmin merging"
module Log = (val Logs.src_log src : Logs.LOG)

type conflict = [ `Conflict of string ]

type 'a promise = unit -> ('a option, conflict) result Lwt.t

let promise t: 'a promise = fun () -> Lwt.return (Ok (Some t))

let memo fn =
  let r = ref None in
  fun () ->
    match !r with
    | Some x -> x
    | None   ->
      fn () >>= fun x ->
      r := Some (Lwt.return x);
      Lwt.return x

type 'a f = old:'a promise -> 'a -> 'a -> ('a, conflict) result Lwt.t
type 'a t = 'a Ir_type.t * 'a f
let v t f = t, f
let f = snd

let conflict fmt =
  ksprintf (fun msg ->
      Log.debug (fun f -> f "conflict: %s" msg);
      Lwt.return (Error (`Conflict msg))
    ) fmt

let bind x f = x >>= function
  | Error _ as x -> Lwt.return x
  | Ok x         -> f x

let promise_map f t () =
  t () >|= function
  | Error _ as x -> x
  | Ok None      -> Ok None
  | Ok (Some a)  -> Ok (Some (f a))

let promise_bind t f () =
  t () >>= function
  | Error _ as x -> Lwt.return x
  | Ok None      -> Lwt.return @@ Ok None
  | Ok (Some a)  -> f a ()

let ok x = Lwt.return (Ok x)

module Infix = struct
  let (>>|) = bind
  let (>?|) = promise_bind
end

open Infix

let default t =
  let pp = Ir_type.dump t and (=) = Ir_type.equal t in
  t, fun ~old t1 t2 ->
    Log.debug (fun f -> f "default %a | %a" pp t1 pp t2);
    if t1 = t2 then ok t1
    else old () >>| function
      | None     -> conflict "default: add/add and no common ancestor"
      | Some old ->
        Log.debug (fun f -> f "default old=%a" pp t1);
        if old = t1 then ok t2
        else if old = t2 then ok t1
        else conflict "default"

let seq = function
  | []         -> invalid_arg "nothing to merge"
  | (t, _)::_ as ts ->
    t, fun ~old v1 v2 ->
      Lwt_list.fold_left_s (fun acc (_, merge) ->
          match acc with
          | Ok x    -> ok x
          | Error _ -> merge ~old v1 v2
        ) (Error (`Conflict "nothing to merge")) ts

let option (a, t) =
  let dt = Ir_type.(option a) in
  let pp = Ir_type.(dump dt) in
  dt, fun ~old t1 t2->
    Log.debug (fun f -> f "some %a | %a" pp t1 pp t2);
    f (default Ir_type.(option a)) ~old t1 t2 >>= function
    | Ok x    -> ok x
    | Error _ ->
      match t1, t2 with
      | None   , None    -> ok None
      | Some v1, Some v2 ->
        let old = fun () ->
          old () >>| function
          | None   -> ok None
          | Some o ->
            Log.debug (fun f -> f "option old=%a" pp o);
            ok o
        in
        t ~old v1 v2 >>| fun x ->
        ok (Some x)
      | Some x , None
      | None   , Some x ->
        old () >>| function
        | None
        | Some None     -> ok (Some x)
        | Some (Some o) ->
          let pp = Ir_type.dump a and (=) = Ir_type.equal a in
          Log.debug (fun f -> f "option old=%a" pp o);
          if x = o then ok (Some x) else conflict "option: add/del"

let omap f = function
  | None   -> ok None
  | Some x -> ok (Some (f x))

let pair (da, a) (db, b) =
  let dt = Ir_type.pair da db in
  let pp = Ir_type.dump dt in
  dt, fun ~old x y ->
    Log.debug (fun f -> f "pair %a | %a" pp x pp y);
    let (a1, b1), (a2, b2) = x, y in
    let ret m x = Log.debug (fun f -> f "pair obj=%a" Ir_type.(dump m) x); x in
    let o1 () = old () >>| omap (fun (o1, _) -> ret da o1) in
    let o2 () = old () >>| omap (fun (_, o2) -> ret db o2) in
    a ~old:o1 a1 a2 >>| fun a3 ->
    b ~old:o2 b1 b2 >>| fun b3 ->
    ok (a3, b3)

let triple (da, a) (db, b) (dc, c) =
  let dt = Ir_type.triple  da db dc in
  let pp = Ir_type.dump dt in
  dt, fun ~old x y ->
    Log.debug (fun f -> f "triple %a | %a" pp x pp y);
    let (a1, b1, c1), (a2, b2, c2) = x, y in
    let ret m x = Log.debug (fun f -> f "triple old=%a" Ir_type.(dump m) x); x in
    let o1 () = old () >>| omap (fun (o1, _, _) -> ret da o1) in
    let o2 () = old () >>| omap (fun (_, o2, _) -> ret db o2) in
    let o3 () = old () >>| omap (fun (_, _, o3) -> ret dc o3) in
    a ~old:o1 a1 a2 >>| fun a3 ->
    b ~old:o2 b1 b2 >>| fun b3 ->
    c ~old:o3 c1 c2 >>| fun c3 ->
    ok (a3, b3, c3)

exception C of string

let merge_elt merge_v old key vs =
  let v1, v2 = match vs with
    | `Left v        -> Some v , None
    | `Right v       -> None   , Some v
    | `Both (v1, v2) -> Some v1, Some v2
  in
  let old () = old key in
  merge_v key ~old v1 v2 >>= function
  | Error (`Conflict msg) -> Lwt.fail (C msg)
  | Ok x -> Lwt.return x

(* assume l1 and l2 are key-sorted *)
let alist_iter2 compare_k f l1 l2 =
  let rec aux l1 l2 = match l1, l2 with
    | [], t -> List.iter (fun (key, v) -> f key (`Right v)) t
    | t, [] -> List.iter (fun (key, v) -> f key (`Left v)) t
    | (k1,v1)::t1, (k2,v2)::t2 ->
      match compare_k k1 k2 with
      | 0 ->
        f k1 (`Both (v1, v2));
        aux t1 t2
      | x -> if x < 0 then (
          f k1 (`Left v1);
          aux t1 l2
        ) else (
          f k2 (`Right v2);
          aux l1 t2
        )
  in
  aux l1 l2

(* assume l1 and l2 are key-sorted *)
let alist_iter2_lwt compare_k f l1 l2 =
  let open Lwt in
  let l3 = ref [] in
  alist_iter2 compare_k (fun left right ->
      l3 := f left right :: !l3
    ) l1 l2;
  Lwt_list.iter_p
    (fun b -> b >>= fun () -> return_unit) (List.rev !l3)

(* DO NOT assume l1 and l2 are key-sorted *)
let alist_merge_lwt compare_k f l1 l2 =
  let open Lwt in
  let l3 = ref [] in
  let sort l = List.sort (fun (x,_) (y,_) -> compare_k x y) l in
  let l1 = sort l1 in
  let l2 = sort l2 in
  let f key data =
    f key data >>= function
    | None   -> return_unit
    | Some v -> l3 := (key, v) :: !l3; return_unit
  in
  alist_iter2_lwt compare_k f l1 l2 >>= fun () ->
  return !l3

let alist dx dy merge_v =
  let dt = Ir_type.(list (pair dx dy)) in
  dt, fun ~old x y ->
    let pair = Ir_type.pair dx dy in
    let pp = Ir_type.dump dt in
    Log.debug (fun l -> l "alist %a | %a" pp x pp y);
    let sort = List.sort @@ Ir_type.compare pair in
    let x = sort x in
    let y = sort y in
    let old k =
      old () >>| function
      | None     -> ok (Some None)
      | Some old ->
        let old = try Some (List.assoc k old) with Not_found -> None in
        ok (Some old)
    in
    let merge_v k = f (merge_v k) in
    Lwt.catch (fun () ->
        alist_merge_lwt Ir_type.(compare dx) (merge_elt merge_v old) x y >>= ok
      ) (function
        | C msg -> conflict "%s" msg
        | e     -> Lwt.fail e)

module MultiSet (K: sig
    include Set.OrderedType
    val t: t Ir_type.t
  end) = struct

  module M = Map.Make(K)
  let of_alist l = List.fold_left (fun map (k, v)  -> M.add k v map) M.empty l
  let t = Ir_type.like Ir_type.(list (pair K.t int)) of_alist M.bindings

  let merge ~old m1 m2 =
    let get k m = try M.find k m with Not_found -> 0 in
    let set k v m = match v with
      | 0 -> M.remove k m
      | _ -> M.add k v m
    in
    let add k v m = set k (v + get k m) m in
    let keys = ref M.empty in
    old () >>| fun old ->
    let old = match old with None -> M.empty | Some o -> o in
    M.iter (fun k v -> keys := add k (-v) !keys) old;
    M.iter (fun k v -> keys := add k v !keys) m1;
    M.iter (fun k v -> keys := add k v !keys) m2;
    ok !keys

  let merge = t, merge
end

module Set (K: sig
    include Set.OrderedType
    val t: t Ir_type.t
  end) =
struct

  module S = Set.Make(K)
  let of_list l = List.fold_left (fun set elt -> S.add elt set) S.empty l
  let t = Ir_type.(like @@ list K.t) of_list S.elements
  let pp = Ir_type.dump t

  let merge ~old x y =
    Log.debug (fun l -> l "merge %a %a" pp x pp y);
    old () >>| fun old ->
    let old = match old with None -> S.empty | Some o -> o in
    let (++) = S.union and (--) = S.diff in
    let to_add = (x -- old) ++ (y -- old) in
    let to_del = (old -- x) ++ (old -- y) in
    ok ((old -- to_del) ++ to_add)

  let merge = t, merge

end

module Map (K: sig
    include Map.OrderedType
    val t: t Ir_type.t
  end) = struct

  module M = Map.Make(K)
  let of_alist l = List.fold_left (fun map (k, v)  -> M.add k v map) M.empty l
  let t v = Ir_type.like Ir_type.(list @@ pair K.t v) of_alist M.bindings
  let iter2 f t1 t2 = alist_iter2 K.compare f (M.bindings t1) (M.bindings t2)

  let iter2 f m1 m2 =
    let m3 = ref [] in
    iter2 (fun key data ->
        m3 := f key data :: !m3
      ) m1 m2;
    Lwt_list.iter_p
      (fun b -> b >>= fun () -> Lwt.return_unit) (List.rev !m3)

  let merge_maps f m1 m2 =
    let l3 = ref [] in
    let f key data =
      f key data >|= function
      | None   -> ()
      | Some v -> l3 := (key, v) :: !l3
    in
    iter2 f m1 m2 >>= fun () ->
    let m3 = of_alist !l3 in
    Lwt.return m3

  let merge dv merge_v =
    let pp ppf m = Ir_type.(dump (list (pair K.t dv))) ppf @@ M.bindings m in
    let merge_v k = f (merge_v k) in
    t dv, fun ~old m1 m2 ->
      Log.debug (fun f -> f "assoc %a | %a" pp m1 pp m2);
      Lwt.catch (fun () ->
          let old key =
            old () >>| function
            | None     -> ok None
            | Some old ->
              Log.debug (fun f -> f "assoc old=%a" pp old);
              let old = try Some (M.find key old) with Not_found -> None in
              ok (Some old)
          in
          merge_maps (merge_elt merge_v old) m1 m2
          >>= ok)
        (function
          | C msg -> conflict "%s" msg
          | e     -> Lwt.fail e)

end

let like da t a_to_b b_to_a =
  let pp = Ir_type.dump da in
  let merge ~old a1 a2 =
    Log.debug (fun f -> f "biject %a | %a" pp a1 pp a2);
    try
      let b1  = a_to_b a1 in
      let b2  = a_to_b a2 in
      let old =
        memo (fun () ->
            old () >>| omap (fun a ->
            Log.debug (fun f -> f "biject old=%a" pp a);
            a_to_b a))
      in
      (f t) ~old b1 b2 >>| fun b3 ->
      ok (b_to_a b3)
    with Not_found ->
      conflict "biject"
  in
  seq [
    default da;
    da, merge;
  ]

let like_lwt da t a_to_b b_to_a =
  let pp = Ir_type.dump da in
  let merge ~old a1 a2 =
    Log.debug (fun f -> f "biject' %a | %a" pp a1 pp a2);
    try
      a_to_b a1  >>= fun b1 ->
      a_to_b a2  >>= fun b2 ->
      let old = memo (fun () ->
          old () >>| function
          | None   -> ok None
          | Some a ->
            Log.debug (fun f -> f "biject' old=%a" pp a);
            a_to_b a >>= fun b ->
            ok (Some b))
      in
      (f t) ~old b1 b2 >>| fun b3 ->
      b_to_a b3 >>=
      ok
    with Not_found ->
      conflict "biject'"
  in
  seq [
    default da;
    da, merge;
  ]

let unit = default Ir_type.unit
let bool = default Ir_type.bool
let char = default Ir_type.char
let int = default Ir_type.int
let int32 = default Ir_type.int32
let int64 = default Ir_type.int64
let float = default Ir_type.float
let string = default Ir_type.string

type counter = int

let counter =
  Ir_type.int,
  fun ~old x y ->
    old () >>| fun old ->
    let old = match old with None -> 0 | Some o -> o in
    ok (x + y - old)

let with_conflict rewrite (d, f) =
  let f ~old x y =
    f ~old x y >>= function
    | Error (`Conflict msg) -> conflict "%s" (rewrite msg)
    | Ok x -> ok x
  in
  d, f

let conflict_t =
  Ir_type.(like string) (fun x -> `Conflict x) (function `Conflict x -> x)

let result_t ok =
  let open Ir_type in
  variant "result" (fun ok error -> function
      | Ok x    -> ok x
      | Error x -> error x)
  |~ case1 "ok" ok (fun x -> Ok x)
  |~ case1 "error" conflict_t (fun x -> Error x)
  |> sealv
