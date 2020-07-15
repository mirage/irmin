(*
 * Copyright (c) 2016-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Type_core

let ( >>= ) x f = match x with Some x -> f x | None -> None

let ( >|= ) x f = match x with Some x -> Some (f x) | None -> None

let int n =
  let rec aux len n =
    if n >= 0 && n < 128 then len else aux (len + 1) (n lsr 7)
  in
  aux 1 n

let len n = function
  | `Int -> int n
  | `Int8 -> 1
  | `Int16 -> 2
  | `Int32 -> 4
  | `Int64 -> 8
  | `Fixed _ -> 0

let unit () = 0

let char (_ : char) = 1

let int32 (_ : int32) = 4

let int64 (_ : int64) = 8

let bool (_ : bool) = 1

let float (_ : float) = 8 (* NOTE: we consider 'double' here *)

let boxed_string n s =
  let s = String.length s in
  len s n + s

let unboxed_string = function
  | `Fixed len -> fun _ -> len (* fixed-size strings are never boxed *)
  | _ -> String.length

let string ~boxed = if boxed then boxed_string else unboxed_string

let boxed_bytes n s =
  let s = Bytes.length s in
  len s n + s

let unboxed_bytes = function
  | `Fixed len -> fun _ -> len (* fixed-size bytes are never boxed *)
  | _ -> Bytes.length

let bytes ~boxed = if boxed then boxed_bytes else unboxed_bytes

let list l n x =
  let init = len (List.length x) n in
  List.fold_left
    (fun acc x ->
      acc >>= fun acc ->
      l x >|= fun l -> acc + l)
    (Some init) x

let array l n x =
  let init = len (Array.length x) n in
  Array.fold_left
    (fun acc x ->
      acc >>= fun acc ->
      l x >|= fun l -> acc + l)
    (Some init) x

let pair a b (x, y) =
  a x >>= fun a ->
  b y >|= fun b -> a + b

let triple a b c (x, y, z) =
  a x >>= fun a ->
  b y >>= fun b ->
  c z >|= fun c -> a + b + c

let option o = function
  | None -> Some (char '\000')
  | Some x -> o x >|= fun o -> char '\000' + o

let rec t : type a. a t -> a size_of = function
  | Self s -> t s.self_fix
  | Custom c -> c.size_of
  | Map b -> map ~boxed:true b
  | Prim t -> prim ~boxed:true t
  | Boxed b -> t b
  | List l -> list (t l.v) l.len
  | Array a -> array (t a.v) a.len
  | Tuple t -> tuple t
  | Option x -> option (t x)
  | Record r -> record r
  | Variant v -> variant v
  | Var v -> raise (Unbound_type_variable v)

and unboxed : type a. a t -> a size_of = function
  | Self s -> unboxed s.self_fix
  | Custom c -> c.unboxed_size_of
  | Map b -> map ~boxed:false b
  | Prim t -> prim ~boxed:false t
  | Boxed b -> t b
  | List l -> list (t l.v) l.len
  | Array a -> array (t a.v) a.len
  | Tuple t -> tuple t
  | Option x -> option (t x)
  | Record r -> record r
  | Variant v -> variant v
  | Var v -> raise (Unbound_type_variable v)

and tuple : type a. a tuple -> a size_of =
 fun ty ->
  match ty with
  | Pair (x, y) -> pair (t x) (t y)
  | Triple (x, y, z) -> triple (t x) (t y) (t z)

and map : type a b. boxed:bool -> (a, b) map -> b size_of =
 fun ~boxed { x; g; _ } ->
  let size_of = if boxed then t x else unboxed x in
  fun u -> size_of (g u)

and prim : type a. boxed:bool -> a prim -> a size_of =
 fun ~boxed p ->
  match p with
  | Unit -> fun x -> Some (unit x)
  | Bool -> fun x -> Some (bool x)
  | Char -> fun x -> Some (char x)
  | Int -> fun x -> Some (int x)
  | Int32 -> fun x -> Some (int32 x)
  | Int64 -> fun x -> Some (int64 x)
  | Float -> fun x -> Some (float x)
  | String n ->
      let size_of = string ~boxed n in
      fun x -> Some (size_of x)
  | Bytes n ->
      let size_of = bytes ~boxed n in
      fun x -> Some (size_of x)

and record : type a. a record -> a size_of =
 fun r ->
  let fields = fields r in
  fun x ->
    List.fold_left
      (fun acc (Field f) ->
        acc >>= fun acc ->
        field f x >|= fun f -> acc + f)
      (Some 0) fields

and field : type a b. (a, b) field -> a size_of =
 fun f ->
  let size_of = t f.ftype in
  fun x -> size_of (f.fget x)

and variant : type a. a variant -> a size_of =
 fun v x ->
  match v.vget x with
  | CV0 v -> Some (int v.ctag0)
  | CV1 (x, vx) -> t x.ctype1 vx >|= fun v -> int x.ctag1 + v
