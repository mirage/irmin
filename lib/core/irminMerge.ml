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

type 'a t = {
  eq   : 'a -> 'a -> bool Lwt.t;
  merge: old:'a -> 'a -> 'a -> 'a Lwt.t;
  to_string: 'a -> string;
}

let default ~eq ~to_string =
  let merge ~old t1 t2 =
    Log.debugf "default %s | %s | %s" (to_string old) (to_string t1) (to_string t2);
    if eq t1 t2 then return t1
    else if eq old t1 then return t2
    else if eq old t2 then return t1
    else (
      Log.debugf "conflict: default";
      fail Conflict
    )
  in
  let eq a b =
    return (eq a b)
  in
  { eq; merge; to_string }

let custom ~eq ~merge ~to_string =
  { eq; merge; to_string }

let default' ~eq ~to_string =
  let merge ~old t1 t2 =
    Log.debugf "default' %s | %s | %s" (to_string old) (to_string t1) (to_string t2);
    eq t1 t2 >>= fun b1 ->
    if b1 then (
      Log.debugf "b1";
      return t1
    ) else
      eq old t1 >>= fun b2 ->
      if b2 then (
        Log.debugf "b2: %s %s" (to_string old) (to_string t1);
        return t2
      ) else
        eq old t2 >>= fun b3 ->
        if b3 then (
          Log.debugf "b3";
          return t1
        ) else (
          Log.debugf "conflict: default'";
          fail Conflict
        )
  in
  { eq; merge; to_string }

let merge t = t.merge

let some t =
  let to_string = function
    | None   -> "None"
    | Some v -> Printf.sprintf "(Some %s)" (t.to_string v) in
  let eq v1 v2 = match v1, v2 with
    | None  , None   -> return true
    | Some _, None
    | None  , Some _ -> return false
    | Some a, Some b -> t.eq a b in
  let merge ~old t1 t2 =
    Log.debugf "some %s | %s | %s" (to_string old) (to_string t1) (to_string t2);
    Lwt.catch
      (fun () -> merge (default' ~eq ~to_string) ~old t1 t2)
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
  { eq; merge; to_string }

let pair a b =
  let to_string (x, y) =
    Printf.sprintf "(%s, %s)" (a.to_string x) (b.to_string y) in
  let eq (a1, b1) (a2, b2) =
    a.eq a1 a2 >>= fun a3 ->
    b.eq b1 b2 >>= fun b3 ->
    return (a3 && b3)
  in
  let merge ~old x y =
    Log.debugf "pair %s | %s | %s" (to_string old) (to_string x) (to_string y);
    let (o1, o2), (a1, b1), (a2, b2) = old, x, y in
    a.merge ~old:o1 a1 a2 >>= fun a3 ->
    b.merge ~old:o2 b1 b2 >>= fun b3 ->
    return (a3, b3)
  in
  { eq; merge; to_string }

let assoc t =
  let to_string l =
    let l = List.map ~f:(fun (l,x) ->
        Printf.sprintf "%s: %s" l (t.to_string x)
      ) l in
    "[" ^ (String.concat ~sep:", " l) ^ "]" in
  let eq l1 l2 =
    let m1 = String.Map.of_alist_exn l1 in
    let m2 = String.Map.of_alist_exn l2 in
    let equal = ref true in
    IrminMisc.Map.iter2 ~f:(fun ~key ~data ->
        match data with
        | `Left _ | `Right _ -> equal := false; return_unit
        | `Both (a, b)       ->
          t.eq a b >>= fun r ->
          equal := !equal && r;
          return_unit
      ) m1 m2 >>= fun () ->
    Log.debugf "eq %s %s= %s" (to_string l1) (if !equal then "" else "!") (to_string l2);
    return !equal
  in
  let merge ~old l1 l2 =
    Log.debugf "assoc %s | %s | %s" (to_string old) (to_string l1) (to_string l2);
    Lwt.catch
      (fun () -> merge (default' ~eq ~to_string) ~old l1 l2)
      (function
        | Conflict ->
          let m1 = String.Map.of_alist_exn l1 in
          let m2 = String.Map.of_alist_exn l2 in
          let old = String.Map.of_alist_exn old in
          IrminMisc.Map.merge ~f:(fun ~key -> function
              | `Left v | `Right v ->
                begin match Map.find old key with
                  | None       ->
                    (* the value has been created in one branch *)
                    return (Some v)
                  | Some ov    ->
                    t.eq v ov >>= fun b ->
                    (* the value has been removed in one branch *)
                    if b then return_none
                    else (
                      (* the value has been both created and removed. *)
                      Log.debugf "conflict: assoc 1";
                      fail Conflict
                    )
                end
              | `Both (v1, v2) ->
                t.eq v1 v2 >>= fun b ->
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
          return (String.Map.to_alist m3)
        | e -> fail e)
  in
  { eq; merge; to_string }

let map t a_to_b b_to_a string_of_b =
  let to_string = string_of_b in
  let eq b1 b2 =
    if b1 = b2 then return true
    else
      let a1 = b_to_a b1 in
      let a2 = b_to_a b2 in
      t.eq a1 a2 in
  let merge ~old b1 b2 =
    Log.debugf "map %s | %s | %s" (to_string old) (to_string b1) (to_string b2);
    if b1 = b2 then return b1
    else
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
  { eq; merge; to_string }

let map' t a_to_b b_to_a string_of_b =
  let to_string = string_of_b in
  let eq b1 b2 =
    if b1 = b2 then return true
    else
      b_to_a b1 >>= fun a1 ->
      b_to_a b2 >>= fun a2 ->
      t.eq a1 a2 in
  let merge ~old b1 b2 =
    Log.debugf "map' %s | %s | %s" (to_string old) (to_string b1) (to_string b2);
    try
      if b1 = b2 then return b1
      else
        b_to_a b1  >>= fun a1 ->
        b_to_a b2  >>= fun a2 ->
        b_to_a old >>= fun old ->
        merge t ~old a1 a2 >>= fun a3 ->
        a_to_b a3
    with Not_found ->
      Log.debugf "conflict: map'";
      fail Conflict
  in
  { eq; merge; to_string }

let string =
  default ~eq:String.equal ~to_string:(fun x -> x)

let apply f x =
  let to_string y = (f x).to_string y in
  let eq a b = (f x).eq a b in
  let merge ~old a b = (f x).merge ~old a b in
  { eq; merge; to_string }
