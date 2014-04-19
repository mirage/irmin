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
open Core_kernel.Std

module Log = Log.Make(struct let section = "MERGE" end)

exception Conflict

module type S = sig
  type t
  val to_string: t -> string
  val equal: t -> t -> bool
end

type 'a t = {
  equal: 'a -> 'a -> bool Lwt.t;
  merge: old:'a -> 'a -> 'a -> 'a Lwt.t;
  m: (module S with type t = 'a);
}

let create (type a) (module A: S with type t = a) merge =
  let equal a b = return (A.equal a b) in
  let merge ~old a b = return (merge ~old a b) in
  { m = (module A); equal; merge }

let create' (type a) (module A: S with type t = a) merge =
  let equal a b = return (A.equal a b) in
  { m = (module A); equal; merge }

let default (type a) (module A: S with type t = a) =
  let equal a b = return (A.equal a b) in
  let merge ~old t1 t2 =
    Log.debugf "default %s | %s | %s" (A.to_string old) (A.to_string t1) (A.to_string t2);
    if A.equal t1 t2 then return t1
    else if A.equal old t1 then return t2
    else if A.equal old t2 then return t1
    else (
      Log.debugf "conflict: default";
      fail Conflict
    )
  in
  { m = (module A); equal; merge }

let default' (type a) (module A: S with type t = a) equal =
  let default = default (module A) in
  let merge' ~old t1 t2 =
    Log.debugf "default' %s | %s | %s" (A.to_string old) (A.to_string t1) (A.to_string t2);
    equal t1 t2 >>= fun b1 ->
    if b1 then return t1
    else
      equal old t1 >>= fun b2 ->
      if b2 then return t2
      else
        equal old t2 >>= fun b3 ->
        if b3 then return t1
        else fail Conflict
  in
  let merge ~old t1 t2 =
    Lwt.catch
      (fun () -> default.merge ~old t1 t2)
      (function Conflict -> merge' ~old t1 t2
              | e        -> fail e)
  in
  { m = (module A); equal; merge }

let merge t = t.merge

let some (type a) t =
  let module T = (val t.m: S with type t = a) in
  let module S = struct
    type t = T.t option
    let to_string = function
      | None   -> "None"
      | Some v -> Printf.sprintf "(Some %s)" (T.to_string v)
    let equal x y = match x, y with
      | None, None     -> true
      | Some _, None
      | None  , Some _ -> false
      | Some x, Some y -> T.equal x y
  end in
  let equal v1 v2 = match v1, v2 with
    | None  , None   -> return true
    | Some _, None
    | None  , Some _ -> return false
    | Some a, Some b -> t.equal a b in
  let merge ~old t1 t2 =
    Log.debugf "some %s | %s | %s" (S.to_string old) (S.to_string t1) (S.to_string t2);
    Lwt.catch
      (fun () -> merge (default' (module S) equal) ~old t1 t2)
      (function
        | Conflict ->
          begin match old, t1, t2 with
            | Some o, Some v1, Some v2 ->
              t.merge ~old:o v1 v2 >>= fun t3 -> return (Some t3)
            | _                               ->
              Log.debugf "conflict: some";
              fail Conflict
          end
        | e -> fail e)
  in
  { m = (module S); equal; merge }

let pair (type a) (type b) a b =
  let module A = (val a.m: S with type t = a) in
  let module B = (val b.m: S with type t = b) in
  let module S = struct
    type t = a * b
    let to_string (x, y) =
      Printf.sprintf "(%s, %s)" (A.to_string x) (B.to_string y)
    let equal (x1, y1) (x2, y2) =
      A.equal x1 x2 && B.equal y1 y2
  end in
  let equal (a1, b1) (a2, b2) =
    a.equal a1 a2 >>= fun a3 ->
    if a3 then b.equal b1 b2
    else return false
  in
  let merge ~old x y =
    Log.debugf "pair %s | %s | %s" (S.to_string old) (S.to_string x) (S.to_string y);
    let (o1, o2), (a1, b1), (a2, b2) = old, x, y in
    a.merge ~old:o1 a1 a2 >>= fun a3 ->
    b.merge ~old:o2 b1 b2 >>= fun b3 ->
    return (a3, b3)
  in
  { m = (module S); equal; merge }

let map (type a) t =
  let module T = (val t.m: S with type t = a) in
  let module S = struct
    type t = a String.Map.t
    let to_string m =
      let l = Map.to_alist m in
      let l = List.map ~f:(fun (l,x) ->
          Printf.sprintf "%s: %s" l (T.to_string x)
        ) l in
      "[" ^ (String.concat ~sep:", " l) ^ "]"
    let equal m1 m2 =
      let equal = ref true in
      Map.iter2 ~f:(fun ~key ~data ->
          match data with
          | `Left _ | `Right _ -> equal := false
          | `Both (a, b)       -> equal := !equal && T.equal a b
        ) m1 m2;
      !equal
  end in
  let equal m1 m2 =
    let equal = ref true in
    IrminMisc.Map.iter2 ~f:(fun ~key ~data ->
        match data with
        | `Left _ | `Right _ -> equal := false; return_unit
        | `Both (a, b)       ->
          t.equal a b >>= fun r ->
          equal := !equal && r;
          return_unit
      ) m1 m2 >>= fun () ->
    return !equal
  in
  let merge ~old m1 m2 =
    Log.debugf "assoc %s | %s | %s" (S.to_string old) (S.to_string m1) (S.to_string m2);
    Lwt.catch
      (fun () -> merge (default' (module S) equal) ~old m1 m2)
      (function
        | Conflict ->
          IrminMisc.Map.merge ~f:(fun ~key -> function
              | `Left v | `Right v ->
                begin match Map.find old key with
                  | None       ->
                    (* the value has been created in one branch *)
                    return (Some v)
                  | Some ov    ->
                    t.equal v ov >>= fun b ->
                    (* the value has been removed in one branch *)
                    if b then return_none
                    else (
                      (* the value has been both created and removed. *)
                      Log.debugf "conflict: assoc 1";
                      fail Conflict
                    )
                end
              | `Both (v1, v2) ->
                t.equal v1 v2 >>= fun b ->
                (* no modification. *)
                if b then return (Some v1)
                else
                  match Map.find old key with
                  | None    ->
                    Log.debugf "conflict: assoc 2";
                    fail Conflict
                  | Some ov ->
                    t.merge ~old:ov v1 v2 >>= fun v -> return (Some v)
            ) m1 m2
          >>= fun m3 ->
          return m3
        | e -> fail e)
  in
  { m = (module S); equal; merge }

let biject (type b) (module B: S with type t = b) t a_to_b b_to_a =
  let default = default (module B) in
  let equal b1 b2 =
    if B.equal b1 b2 then return true
    else
      let a1 = b_to_a b1 in
      let a2 = b_to_a b2 in
      t.equal a1 a2 in
  let merge' ~old b1 b2 =
    Log.debugf "map %s | %s | %s" (B.to_string old) (B.to_string b1) (B.to_string b2);
    try
      let a1 = b_to_a b1 in
      let a2 = b_to_a b2 in
      let old = b_to_a old in
      merge t ~old a1 a2 >>= fun a3 ->
      return (a_to_b a3)
    with Not_found ->
      Log.debugf "conflict: map";
      fail Conflict
  in
  let merge ~old b1 b2 =
    Lwt.catch
      (fun () -> default.merge ~old b1 b2)
      (function Conflict -> merge' ~old b1 b2
              | e        -> fail e)
  in
  { m = (module B); equal; merge }

let biject' (type b) (module B: S with type t = b) t a_to_b b_to_a =
  let default = default (module B) in
  let equal b1 b2 =
    if B.equal b1 b2 then return true
    else
      b_to_a b1 >>= fun a1 ->
      b_to_a b2 >>= fun a2 ->
      t.equal a1 a2 in
  let merge' ~old b1 b2 =
    Log.debugf "map' %s | %s | %s" (B.to_string old) (B.to_string b1) (B.to_string b2);
    try
      b_to_a b1  >>= fun a1 ->
      b_to_a b2  >>= fun a2 ->
      b_to_a old >>= fun old ->
      merge t ~old a1 a2 >>= fun a3 ->
      a_to_b a3
    with Not_found ->
      Log.debugf "conflict: map'";
      fail Conflict
  in
  let merge ~old b1 b2 =
    Lwt.catch
      (fun () -> default.merge ~old b1 b2)
      (function Conflict -> merge' ~old b1 b2
              | e        -> fail e)
  in
  { m = (module B); equal; merge }

let apply (type b) f x =
  let module B = struct
    type t = b
    let to_string a =
      let module B = (val (f x).m: S with type t = b) in
      B.to_string a
    let equal a b =
      let module B = (val (f x).m: S with type t = b) in
      B.equal a b
  end in
  let equal a b = (f x).equal a b in
  let merge ~old a b = (f x).merge ~old a b in
  { m = (module B); equal; merge }

let string =
  default (module String)

let counter =
  let equal x y = return (Int.equal x y) in
  let merge ~old x y = return (x + y - old) in
  { m = (module Int); equal; merge }
