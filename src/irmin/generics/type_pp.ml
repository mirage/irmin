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

let t t =
  let rec aux : type a. a t -> a pp =
   fun t ppf x ->
    match t with
    | Self s -> aux s.self ppf x
    | Custom c -> c.pp ppf x
    | Map m -> map m ppf x
    | Prim p -> prim p ppf x
    | _ -> Type_json.pp t ppf x
  and map : type a b. (a, b) map -> b pp = fun l ppf x -> aux l.x ppf (l.g x)
  and prim : type a. a prim -> a pp =
   fun t ppf x ->
    match t with
    | Unit -> ()
    | Bool -> Fmt.bool ppf x
    | Char -> Fmt.char ppf x
    | Int -> Fmt.int ppf x
    | Int32 -> Fmt.int32 ppf x
    | Int64 -> Fmt.int64 ppf x
    | Float -> Fmt.float ppf x
    | String _ -> Fmt.string ppf x
    | Bytes _ -> Fmt.string ppf (Bytes.unsafe_to_string x)
  in
  aux t

let rec ty : type a. a t Fmt.t =
 fun ppf -> function
  | Self s -> Fmt.pf ppf "@[Self (%a@)]" ty s.self
  | Custom c -> Fmt.pf ppf "@[Custom (%a)@]" custom c
  | Map m -> Fmt.pf ppf "@[Map (%a)]" ty m.x
  | Prim p -> Fmt.pf ppf "@[Prim %a@]" prim p
  | List l -> Fmt.pf ppf "@[List%a (%a)@]" len l.len ty l.v
  | Array a -> Fmt.pf ppf "@[Array%a (%a)@]" len a.len ty a.v
  | Tuple (Pair (a, b)) -> Fmt.pf ppf "@[Pair (%a, %a)@]" ty a ty b
  | Tuple (Triple (a, b, c)) ->
      Fmt.pf ppf "@[Triple (%a, %a, %a)@]" ty a ty b ty c
  | Option t -> Fmt.pf ppf "@[Option (%a)@]" ty t
  | Record _ -> Fmt.pf ppf "@[Record@]"
  | Variant _ -> Fmt.pf ppf "@[Variant@]"

and custom : type a. a custom Fmt.t =
 fun ppf c ->
  match c.cwit with `Type t -> ty ppf t | `Witness _ -> Fmt.string ppf "-"

and prim : type a. a prim Fmt.t =
 fun ppf -> function
  | Unit -> Fmt.string ppf "Unit"
  | Bool -> Fmt.string ppf "Bool"
  | Char -> Fmt.string ppf "Char"
  | Int -> Fmt.string ppf "Int"
  | Int32 -> Fmt.string ppf "Int32"
  | Int64 -> Fmt.string ppf "Int64"
  | Float -> Fmt.string ppf "Float"
  | String n -> Fmt.pf ppf "String%a" len n
  | Bytes n -> Fmt.pf ppf "Bytes%a" len n

and len : len Fmt.t =
 fun ppf -> function
  | `Int8 -> Fmt.string ppf ":8"
  | `Int64 -> Fmt.string ppf ":64"
  | `Int16 -> Fmt.string ppf ":16"
  | `Fixed n -> Fmt.pf ppf ":<%d>" n
  | `Int -> ()
  | `Int32 -> Fmt.pf ppf ":32"

let to_string ty = Fmt.to_to_string (t ty)

let of_string t =
  let map_result f = function Ok x -> Ok (f x) | Error _ as e -> e in
  let v f x = try Ok (f x) with Invalid_argument e -> Error (`Msg e) in
  let rec aux : type a a. a t -> a of_string =
   fun t x ->
    match t with
    | Self s -> aux s.self x
    | Custom c -> c.of_string x
    | Map m -> aux m.x x |> map_result m.f
    | Prim p -> prim p x
    | _ -> Type_json.of_string t x
  and prim : type a. a prim -> a of_string =
   fun t x ->
    match t with
    | Unit -> Ok ()
    | Bool -> v bool_of_string x
    | Char -> v (fun x -> x.[1]) x
    | Int -> v int_of_string x
    | Int32 -> v Int32.of_string x
    | Int64 -> v Int64.of_string x
    | Float -> v float_of_string x
    | String _ -> Ok x
    | Bytes _ -> Ok (Bytes.unsafe_of_string x)
  in
  aux t
