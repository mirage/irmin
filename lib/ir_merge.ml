(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Log = Log.Make(struct let section = "MERGE" end)

module type S = Tc.I0

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

  let to_sexp a_to_sexp t =
    let open Sexplib.Type in
    match t with
    | `Ok a       -> List [ Atom "ok"; a_to_sexp a ]
    | `Conflict s -> List [ Atom "conflict"; Atom s ]

  let to_json a_to_json = function
    | `Ok a       -> `O [ "ok", a_to_json a ]
    | `Conflict s -> `O [ "conflict", Ezjsonm.encode_string s ]

  let of_json a_of_json = function
    | `O [ "ok", j ] -> `Ok (a_of_json j)
    | `O [ "conflict", j ] -> `Conflict (Ezjsonm.decode_string_exn j)
    | j -> Ezjsonm.parse_error j "Merge.Result"

  let write a_write t buf = match t with
    | `Ok a -> a_write a (Ir_misc.tag buf 0)
    | `Conflict s -> Tc.write (module Tc.S) s (Ir_misc.tag buf 1)

  let read a_read buf =
    match Ir_misc.untag buf with
    | 0 -> `Ok (a_read buf)
    | 1 -> `Conflict (Tc.read (module Tc.S) buf)
    | n -> Tc.Reader.error "Merge.Result (tag=%d)" n

  let size_of size_of_a t = 1 + match t with
    | `Ok a -> size_of_a a
    | `Conflict s -> Tc.size_of (module Tc.S) s

end

exception Conflict of string

let exn = function
  | `Ok x       -> return x
  | `Conflict x -> fail (Conflict x)

module R (A: S) = Tc.App1(Result)(A)

type ('a, 'o) t = 'o -> old:'a -> 'a -> 'a -> 'a result Lwt.t

type 'a elt = (module S with type t = 'a)

let conflict fmt =
  ksprintf (fun msg ->
      Log.debugf "conflict: %s" msg;
      return (`Conflict msg)
    ) fmt

let bind x f =
  x >>= function
  | `Conflict _ as x -> return x
  | `Ok x            -> f x

module OP = struct

  let ok x: 'a result Lwt.t = return (`Ok x)

  let conflict: ('a, unit, string, 'b result Lwt.t) format4 -> 'a = conflict

  let (>>|) = bind

end

open OP

let rec iter f = function
  | []   -> ok ()
  | h::t ->
    f h >>= function
    | `Conflict x -> conflict "%s" x
    | `Ok ()      -> iter f t

let default (type a) (module A: S with type t = a) =
  fun _origin ~old t1 t2 ->
    Log.debugf "default %a | %a | %a"
      force (show (module A) old)
      force (show (module A) t1)
      force (show (module A) t2);
    if A.equal t1 t2 then ok t1
    else if A.equal old t1 then ok t2
    else if A.equal old t2 then ok t1
    else conflict "default"

let seq = function
  | []         -> fun _ ~old:_ _ _ -> conflict "nothing to merge"
  | _::_ as ts ->
    fun origin ~old v1 v2 ->
      Lwt_list.fold_left_s (fun acc merge ->
          match acc with
          | `Ok x       -> ok x
          | `Conflict _ -> merge origin ~old v1 v2
        ) (`Conflict "nothing to merge") ts

let some (type a) (module T: S with type t = a) t =
  let module S = Tc.App1(Tc.O)(T) in
  fun origin ~old t1 t2 ->
    Log.debugf "some %a | %a | %a"
      force (show (module S) old)
      force (show (module S) t1)
      force (show (module S) t2);
    default (module S) origin ~old t1 t2 >>= function
    | `Ok x       -> ok x
    | `Conflict _ ->
      match old, t1, t2 with
      | Some o, Some v1, Some v2 ->
        t origin ~old:o v1 v2 >>| fun x ->
        ok (Some x)
      | _ -> conflict "some"

let pair
    (type a) (module A: S with type t = a)
    (type b) (module B: S with type t = b)
    a b =
  let module S = Tc.App2(Tc.P)(A)(B) in
  fun origin ~old x y ->
    Log.debugf "pair %a | %a | %a"
      force (show (module S) old)
      force (show (module S) x)
      force (show (module S) y);
    let (o1, o2), (a1, b1), (a2, b2) = old, x, y in
    a origin ~old:o1 a1 a2 >>| fun a3 ->
    b origin ~old:o2 b1 b2 >>| fun b3 ->
    ok (a3, b3)

exception C of string

let merge_elt (type a) (module V: S with type t = a) merge_v origin old key vs =
  match vs with
  | `Left v | `Right v ->
    begin
      match old key with
      | Some ov ->
        if V.equal v ov then
          (* the value has been removed in one branch *)
          return_none
        else
          (* the value has been both created and removed. *)
          fail (C "remove/add")
      | None ->
        (* the value has been created in one branch *)
        return (Some v)
    end
  | `Both (v1, v2) ->
    if V.equal v1 v2 then
      (* no modification. *)
      return (Some v1)
    else match old key with
      | Some ov -> begin
          merge_v origin ~old:ov v1 v2 >>= function
          | `Conflict msg -> fail (C msg)
          | `Ok x         -> return (Some x)
        end
      | None ->
        (* two different values have been added *)
        raise (C "add/add")

let alist
  (type a) (module A: S with type t = a)
  (type b) (module B: S with type t = b)
  merge_b origin ~old x y =
  let module P = Tc.App2(Tc.P)(A)(B) in
  let sort = List.sort P.compare in
  let x = sort x in
  let y = sort y in
  let old k = try Some (List.assoc k old) with Not_found -> None in
  Lwt.catch (fun () ->
      Ir_misc.alist_merge_lwt A.compare (merge_elt (module B) merge_b origin old) x y
      >>= ok)
    (function
      | C msg -> conflict "%s" msg
      | e     -> fail e)

module Map (S: S) = struct

  let merge (type a) (module A: S with type t = a) t =
    let module SM = Ir_misc.Map(S) in
    let module S = Tc.App1(SM)(A) in
    fun origin ~old m1 m2 ->
      Log.debugf "assoc %a | %a | %a"
        force (show (module S) old)
        force (show (module S) m1)
        force (show (module S) m2);
      default (module S) origin ~old m1 m2 >>= function
      | `Ok x       -> ok x
      | `Conflict _ ->
        Lwt.catch (fun () ->
            let old key = try Some (SM.find key old) with Not_found -> None in
            SM.Lwt.merge (merge_elt (module A) t origin old) m1 m2
            >>= ok)
          (function
            | C msg -> conflict "%s" msg
            | e     -> fail e)

end

let biject
    (type a) (module A: S with type t = a)
    (type b) (module B: S with type t = b)
    t a_to_b b_to_a =
  let default = default (module B) in
  let merge' origin ~old b1 b2 =
    Log.debugf "map %a | %a | %a"
      force (show (module B) old)
      force (show (module B) b1)
      force (show (module B) b2);
    try
      let a1  = b_to_a b1 in
      let a2  = b_to_a b2 in
      let old = b_to_a old in
      t origin ~old a1 a2 >>| fun a3 ->
      ok (a_to_b a3)
    with Not_found ->
      conflict "biject"
  in
  fun origin ~old b1 b2 ->
    default origin ~old b1 b2 >>= function
    | `Ok x       -> ok x
    | `Conflict _ -> merge' origin ~old b1 b2

let biject'
  (type a) (module A: S with type t = a)
  (type b) (module B: S with type t = b)
  t a_to_b b_to_a =
  let default = default (module B) in
  let merge' origin ~old b1 b2 =
    Log.debugf "map' %a | %a | %a"
      force (show (module B) old)
      force (show (module B) b1)
      force (show (module B) b2);
    try
      b_to_a b1  >>= fun a1 ->
      b_to_a b2  >>= fun a2 ->
      b_to_a old >>= fun old ->
      t origin ~old a1 a2 >>| fun a3 ->
      a_to_b a3 >>=
      ok
    with Not_found ->
      conflict "biject'"
  in
  fun origin ~old b1 b2 ->
    default origin ~old b1 b2 >>= function
    | `Ok x       -> ok x
    | `Conflict _ -> merge' origin ~old b1 b2

let string origin ~old x y =
  default (module Tc.S) origin ~old x y

let counter _ ~old x y =
  ok (x + y - old)

let apply f x origin ~old a b =
  f x origin ~old a b
