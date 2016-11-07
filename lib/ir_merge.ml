(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Printf
open Ir_misc.OP

let src = Logs.Src.create "irmin.merge" ~doc:"Irmin merging"
module Log = (val Logs.src_log src : Logs.LOG)

type 'a result =
  [ `Ok of 'a
  | `Conflict of string ]

module Result = struct

  type 'a t = 'a result

  let equal equal_a x y = match x, y with
    | `Ok x, `Ok y -> equal_a x y
    | `Conflict x, `Conflict y -> x = y
    | _ -> false

  let compare _ = Pervasives.compare
  let hash _ = Hashtbl.hash

  let to_json a_to_json = function
    | `Ok a       -> `O [ "ok", a_to_json a ]
    | `Conflict s -> `O [ "conflict", Ezjsonm.encode_string s ]

  let of_json a_of_json = function
    | `O [ "ok", j ] -> `Ok (a_of_json j)
    | `O [ "conflict", j ] -> `Conflict (Ezjsonm.decode_string_exn j)
    | j -> Ezjsonm.parse_error j "Merge.Result"

  let write a_write t buf = match t with
    | `Ok a -> a_write a (Ir_misc.tag buf 0)
    | `Conflict s -> Tc.write (module Tc.String) s (Ir_misc.tag buf 1)

  let read a_read buf =
    match Ir_misc.untag buf with
    | 0 -> `Ok (a_read buf)
    | 1 -> `Conflict (Tc.read (module Tc.String) buf)
    | n -> Tc.Reader.error "Merge.Result (tag=%d)" n

  let size_of size_of_a t = 1 + match t with
    | `Ok a -> size_of_a a
    | `Conflict s -> Tc.size_of (module Tc.String) s

end

exception Conflict of string

let exn = function
  | `Ok x       -> return x
  | `Conflict x -> fail (Conflict x)

module R (A: Tc.S0) = Tc.App1(Result)(A)

type 'a promise = unit -> 'a option result Lwt.t

let promise t: 'a promise = fun () -> Lwt.return (`Ok (Some t))

let memo fn =
  let r = ref None in
  fun () ->
    match !r with
    | Some x -> x
    | None   ->
      fn () >>= fun x ->
      r := Some (Lwt.return x);
      Lwt.return x

type 'a t = old:'a promise -> 'a -> 'a -> 'a result Lwt.t

let conflict fmt =
  ksprintf (fun msg ->
      Log.debug (fun f -> f "conflict: %s" msg);
      return (`Conflict msg)
    ) fmt

let bind x f =
  x >>= function
  | `Conflict _ as x -> Lwt.return x
  | `Ok x            -> f x

let promise_map f t () =
  t () >>= function
  | `Conflict _ as x -> Lwt.return x
  | `Ok None         -> Lwt.return @@ `Ok None
  | `Ok (Some a)     -> Lwt.return @@ `Ok (Some (f a))

let promise_bind t f () =
  t () >>= function
  | `Conflict _ as x -> Lwt.return x
  | `Ok None         -> Lwt.return @@ `Ok None
  | `Ok (Some a)     -> f a ()

module OP = struct

  let ok x: 'a result Lwt.t = return (`Ok x)

  let conflict: ('a, unit, string, 'b result Lwt.t) format4 -> 'a = conflict

  let (>>|) = bind

  let (>?|) = promise_bind

end

open OP

let rec iter f = function
  | []   -> ok ()
  | h::t ->
    f h >>= function
    | `Conflict x -> conflict "%s" x
    | `Ok ()      -> iter f t

let default (type a) (module A: Tc.S0 with type t = a) =
  fun ~old t1 t2 ->
    Log.debug (fun f -> f "default %a | %a"
      (show (module A)) t1
      (show (module A)) t2);
    if A.equal t1 t2 then ok t1
    else old () >>| function
      | None     -> conflict "default: add/add and no common ancestor"
      | Some old ->
        Log.debug (fun f -> f "default old=%a" (show (module A)) t1);
        if A.equal old t1 then ok t2
        else if A.equal old t2 then ok t1
        else conflict "default"

let seq = function
  | []         -> fun ~old:_ _ _ -> conflict "nothing to merge"
  | _::_ as ts ->
    fun ~old v1 v2 ->
      Lwt_list.fold_left_s (fun acc merge ->
          match acc with
          | `Ok x       -> ok x
          | `Conflict _ -> merge ~old v1 v2
        ) (`Conflict "nothing to merge") ts

let option (type a) (module T: Tc.S0 with type t = a) (t: a t): a option t =
  let module S = Tc.Option(T) in
  fun ~old t1 t2->
    Log.debug (fun f -> f "some %a | %a"
      (show (module S)) t1
      (show (module S)) t2);
    default (module S) ~old t1 t2 >>= function
    | `Ok x       -> ok x
    | `Conflict _ ->
      match t1, t2 with
      | None   , None    -> ok None
      | Some v1, Some v2 ->
        let old: a promise = fun () ->
          old () >>| function
          | None   -> ok None
          | Some o ->
            Log.debug (fun f -> f "option old=%a" (show (module S)) o);
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
          Log.debug (fun f -> f "option old=%a" (show (module T)) o);
          if T.equal x o then ok (Some x) else conflict "option: add/del"

let omap f = function
  | None   -> ok None
  | Some x -> ok (Some (f x))

let pair
    (type a) (module A: Tc.S0 with type t = a)
    (type b) (module B: Tc.S0 with type t = b)
    a b =
  let module S = Tc.Pair(A)(B) in
  fun ~old x y ->
    Log.debug (fun f -> f "pair %a | %a"
      (show (module S)) x
      (show (module S)) y);
    let (a1, b1), (a2, b2) = x, y in
    let ret m x = Log.debug (fun f -> f "pair obj=%a" (show m) x); x in
    let o1 () = old () >>| omap (fun (o1, _) -> ret (module A) o1) in
    let o2 () = old () >>| omap (fun (_, o2) -> ret (module B) o2) in
    a ~old:o1 a1 a2 >>| fun a3 ->
    b ~old:o2 b1 b2 >>| fun b3 ->
    ok (a3, b3)

let triple
  (type a) (module A: Tc.S0 with type t = a)
  (type b) (module B: Tc.S0 with type t = b)
  (type c) (module C: Tc.S0 with type t = c)
  a b c =
  let module S = Tc.Triple(A)(B)(C) in
  fun ~old x y ->
    Log.debug (fun f -> f "triple %a | %a"
      (show (module S)) x
      (show (module S)) y);
    let (a1, b1, c1), (a2, b2, c2) = x, y in
    let ret m x = Log.debug (fun f -> f "triple old=%a" (show m) x); x in
    let o1 () = old () >>| omap (fun (o1, _, _) -> ret (module A) o1) in
    let o2 () = old () >>| omap (fun (_, o2, _) -> ret (module B) o2) in
    let o3 () = old () >>| omap (fun (_, _, o3) -> ret (module C) o3) in
    a ~old:o1 a1 a2 >>| fun a3 ->
    b ~old:o2 b1 b2 >>| fun b3 ->
    c ~old:o3 c1 c2 >>| fun c3 ->
    ok (a3, b3, c3)

exception C of string

let merge_elt (type k) (type a)
    (module K: Tc.S0 with type t = k)
    (module V: Tc.S0 with type t = a)
    merge_v old key vs
  =
  let v1, v2 = match vs with
    | `Left v  -> Some v, None
    | `Right v -> None, Some v
    | `Both (v1, v2) -> Some v1, Some v2
  in
  let old () = old key in
  merge_v key ~old v1 v2 >>= function
  | `Conflict msg -> fail (C msg)
  | `Ok x         -> return x

let alist
  (type a) (module A: Tc.S0 with type t = a)
  (type b) (module B: Tc.S0 with type t = b)
  merge_b ~old x y =
  let pp = Fmt.(Dump.list @@ Dump.pair (show (module A)) (show (module B))) in
  Log.debug (fun l -> l "alist %a | %a" pp x pp y);
  let module P = Tc.Pair(A)(B) in
  let sort = List.sort P.compare in
  let x = sort x in
  let y = sort y in
  let old k =
    old () >>| function
    | None     -> ok (Some None)
    | Some old ->
      let old = try Some (List.assoc k old) with Not_found -> None in
      ok (Some old)
  in
  Lwt.catch (fun () ->
      Ir_misc.alist_merge_lwt A.compare
        (merge_elt (module A) (module B) merge_b old) x y
      >>= ok)
    (function
      | C msg -> conflict "%s" msg
      | e     -> fail e)

module MSet (M: Map.S) = struct

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

end

module Map (M: Map.S) (S: Tc.S0 with type t = M.key) = struct

  module SM = Ir_misc.Map_ext(M)(S)

  let merge (type a) (module A: Tc.S0 with type t = a) t =
    let module X = Tc.App1(SM)(A) in
    fun ~old m1 m2 ->
      Log.debug (fun f -> f "assoc %a | %a"
        (show (module X)) m1
        (show (module X)) m2);
      Lwt.catch (fun () ->
          let old key =
            old () >>| function
            | None     -> ok None
            | Some old ->
              Log.debug (fun f -> f "assoc old=%a" (show (module X)) old);
              let old = try Some (SM.find key old) with Not_found -> None in
              ok (Some old)
          in
          SM.Lwt.merge (merge_elt (module S) (module A) t old) m1 m2
          >>= ok)
        (function
          | C msg -> conflict "%s" msg
          | e     -> fail e)
end

let biject
    (type a) (module A: Tc.S0 with type t = a)
    t a_to_b b_to_a
  =
  let merge ~old a1 a2 =
    Log.debug (fun f -> f "biject %a | %a"
      (show (module A)) a1
      (show (module A)) a2);
    try
      let b1  = a_to_b a1 in
      let b2  = a_to_b a2 in
      let old =
        memo (fun () ->
            old () >>| omap (fun a ->
            Log.debug (fun f -> f "biject old=%a" (show (module A)) a);
            a_to_b a))
      in
      t ~old b1 b2 >>| fun b3 ->
      ok (b_to_a b3)
    with Not_found ->
      conflict "biject"
  in
  seq [
    default (module A);
    merge;
  ]

let biject'
    (type a) (module A: Tc.S0 with type t = a)
    t a_to_b b_to_a
  =
  let merge ~old a1 a2 =
    Log.debug (fun f -> f "biject' %a | %a"
      (show (module A)) a1
      (show (module A)) a2);
    try
      a_to_b a1  >>= fun b1 ->
      a_to_b a2  >>= fun b2 ->
      let old = memo (fun () ->
          old () >>| function
          | None   -> ok None
          | Some a ->
            Log.debug (fun f -> f "biject' old=%a" (show (module A)) a);
            a_to_b a >>= fun b ->
            ok (Some b))
      in
      t ~old b1 b2 >>| fun b3 ->
      b_to_a b3 >>=
      ok
    with Not_found ->
      conflict "biject'"
  in
  seq [
    default (module A);
    merge;
  ]

let string ~old x y =
  default (module Tc.String) ~old x y

let set (type t) (module S: Set.S with type t = t) ~old x y =
  old () >>| fun old ->
  let old = match old with None -> S.empty | Some o -> o in
  let (++) = S.union and (--) = S.diff in
  let to_add = (x -- old) ++ (y -- old) in
  let to_del = (old -- x) ++ (old -- y) in
  ok ((old -- to_del) ++ to_add)

type counter = int

let counter ~old x y =
  old () >>| fun old ->
  let old = match old with None -> 0 | Some o -> o in
  ok (x + y - old)

let apply f x ~old a b =
  f x ~old a b

let with_conflict rewrite f ~old x y =
  f ~old x y >>= function
  | `Conflict msg -> conflict "%s" (rewrite msg)
  | `Ok x -> ok x
