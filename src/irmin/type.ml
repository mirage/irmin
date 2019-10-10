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

type (_, _) eq = Refl : ('a, 'a) eq

module Witness : sig
  type 'a t

  val make : unit -> 'a t

  val eq : 'a t -> 'b t -> ('a, 'b) eq option
end = struct
  type _ equality = ..

  module type Inst = sig
    type t

    type _ equality += Eq : t equality
  end

  type 'a t = (module Inst with type t = 'a)

  let make : type a. unit -> a t =
   fun () ->
    let module Inst = struct
      type t = a

      type _ equality += Eq : t equality
    end in
    (module Inst)

  let eq : type a b. a t -> b t -> (a, b) eq option =
   fun (module A) (module B) -> match A.Eq with B.Eq -> Some Refl | _ -> None
end

module Json = struct
  type decoder = { mutable lexemes : Jsonm.lexeme list; d : Jsonm.decoder }

  let decoder ?encoding src = { lexemes = []; d = Jsonm.decoder ?encoding src }

  let decoder_of_lexemes lexemes = { lexemes; d = Jsonm.decoder (`String "") }

  let rewind e l = e.lexemes <- l :: e.lexemes

  let decode e =
    match e.lexemes with
    | h :: t ->
        e.lexemes <- t;
        `Lexeme h
    | [] -> Jsonm.decode e.d
end

type len = [ `Int | `Int8 | `Int16 | `Int32 | `Int64 | `Fixed of int ]

type 'a pp = 'a Fmt.t

type 'a of_string = string -> ('a, [ `Msg of string ]) result

type 'a to_string = 'a -> string

type 'a encode_json = Jsonm.encoder -> 'a -> unit

type 'a decode_json = Json.decoder -> ('a, [ `Msg of string ]) result

type 'a bin_seq = 'a -> (string -> unit) -> unit

type 'a encode_bin = ?headers:bool -> 'a bin_seq

type 'a decode_bin = ?headers:bool -> string -> int -> int * 'a

type 'a size_of = ?headers:bool -> 'a -> int option

type 'a compare = 'a -> 'a -> int

type 'a equal = 'a -> 'a -> bool

type 'a short_hash = ?seed:int -> 'a -> int

type 'a t =
  | Self : 'a self -> 'a t
  | Custom : 'a custom -> 'a t
  | Map : ('a, 'b) map -> 'b t
  | Prim : 'a prim -> 'a t
  | List : 'a len_v -> 'a list t
  | Array : 'a len_v -> 'a array t
  | Tuple : 'a tuple -> 'a t
  | Option : 'a t -> 'a option t
  | Record : 'a record -> 'a t
  | Variant : 'a variant -> 'a t

and 'a len_v = { len : len; v : 'a t }

and 'a custom = {
  cwit : [ `Type of 'a t | `Witness of 'a Witness.t ];
  pp : 'a pp;
  of_string : 'a of_string;
  encode_json : 'a encode_json;
  decode_json : 'a decode_json;
  encode_bin : 'a encode_bin;
  decode_bin : 'a decode_bin;
  short_hash : 'a short_hash;
  pre_hash : 'a bin_seq;
  size_of : 'a size_of;
  compare : 'a compare;
  equal : 'a equal;
}

and ('a, 'b) map = {
  x : 'a t;
  f : 'a -> 'b;
  g : 'b -> 'a;
  mwit : 'b Witness.t;
}

and 'a self = { mutable self : 'a t }

and 'a prim =
  | Unit : unit prim
  | Bool : bool prim
  | Char : char prim
  | Int : int prim
  | Int32 : int32 prim
  | Int64 : int64 prim
  | Float : float prim
  | String : len -> string prim
  | Bytes : len -> bytes prim

and 'a tuple =
  | Pair : 'a t * 'b t -> ('a * 'b) tuple
  | Triple : 'a t * 'b t * 'c t -> ('a * 'b * 'c) tuple

and 'a record = {
  rwit : 'a Witness.t;
  rname : string;
  rfields : 'a fields_and_constr;
}

and 'a fields_and_constr =
  | Fields : ('a, 'b) fields * 'b -> 'a fields_and_constr

and ('a, 'b) fields =
  | F0 : ('a, 'a) fields
  | F1 : ('a, 'b) field * ('a, 'c) fields -> ('a, 'b -> 'c) fields

and ('a, 'b) field = { fname : string; ftype : 'b t; fget : 'a -> 'b }

and 'a variant = {
  vwit : 'a Witness.t;
  vname : string;
  vcases : 'a a_case array;
  vget : 'a -> 'a case_v;
}

and 'a a_case = C0 : 'a case0 -> 'a a_case | C1 : ('a, 'b) case1 -> 'a a_case

and 'a case_v =
  | CV0 : 'a case0 -> 'a case_v
  | CV1 : ('a, 'b) case1 * 'b -> 'a case_v

and 'a case0 = { ctag0 : int; cname0 : string; c0 : 'a }

and ('a, 'b) case1 = {
  ctag1 : int;
  cname1 : string;
  ctype1 : 'b t;
  c1 : 'b -> 'a;
}

type _ a_field = Field : ('a, 'b) field -> 'a a_field

let rec pp_ty : type a. a t Fmt.t =
 fun ppf -> function
  | Self s -> Fmt.pf ppf "@[Self (%a@)]" pp_ty s.self
  | Custom c -> Fmt.pf ppf "@[Custom (%a)@]" pp_custom c
  | Map m -> Fmt.pf ppf "@[Map (%a)]" pp_ty m.x
  | Prim p -> Fmt.pf ppf "@[Prim %a@]" pp_prim p
  | List l -> Fmt.pf ppf "@[List%a (%a)@]" pp_len l.len pp_ty l.v
  | Array a -> Fmt.pf ppf "@[Array%a (%a)@]" pp_len a.len pp_ty a.v
  | Tuple (Pair (a, b)) -> Fmt.pf ppf "@[Pair (%a, %a)@]" pp_ty a pp_ty b
  | Tuple (Triple (a, b, c)) ->
      Fmt.pf ppf "@[Triple (%a, %a, %a)@]" pp_ty a pp_ty b pp_ty c
  | Option t -> Fmt.pf ppf "@[Option (%a)@]" pp_ty t
  | Record _ -> Fmt.pf ppf "@[Record@]"
  | Variant _ -> Fmt.pf ppf "@[Variant@]"

and pp_custom : type a. a custom Fmt.t =
 fun ppf c ->
  match c.cwit with `Type t -> pp_ty ppf t | `Witness _ -> Fmt.string ppf "-"

and pp_prim : type a. a prim Fmt.t =
 fun ppf -> function
  | Unit -> Fmt.string ppf "Unit"
  | Bool -> Fmt.string ppf "Bool"
  | Char -> Fmt.string ppf "Char"
  | Int -> Fmt.string ppf "Int"
  | Int32 -> Fmt.string ppf "Int32"
  | Int64 -> Fmt.string ppf "Int64"
  | Float -> Fmt.string ppf "Float"
  | String n -> Fmt.pf ppf "String%a" pp_len n
  | Bytes n -> Fmt.pf ppf "Bytes%a" pp_len n

and pp_len : len Fmt.t =
 fun ppf -> function
  | `Int8 -> Fmt.string ppf ":8"
  | `Int64 -> Fmt.string ppf ":64"
  | `Int16 -> Fmt.string ppf ":16"
  | `Fixed n -> Fmt.pf ppf ":<%d>" n
  | `Int -> ()
  | `Int32 -> Fmt.pf ppf ":32"

module Refl = struct
  let prim : type a b. a prim -> b prim -> (a, b) eq option =
   fun a b ->
    match (a, b) with
    | Unit, Unit -> Some Refl
    | Bool, Bool -> Some Refl
    | Char, Char -> Some Refl
    | Int, Int -> Some Refl
    | Int32, Int32 -> Some Refl
    | Int64, Int64 -> Some Refl
    | Float, Float -> Some Refl
    | String _, String _ -> Some Refl
    | Bytes _, Bytes _ -> Some Refl
    | _ -> None

  let rec t : type a b. a t -> b t -> (a, b) eq option =
   fun a b ->
    match (a, b) with
    | Self a, _ -> t a.self b
    | _, Self b -> t a b.self
    | Map a, Map b -> Witness.eq a.mwit b.mwit
    | Custom a, Custom b -> custom a b
    | Prim a, Prim b -> prim a b
    | Array a, Array b -> (
        match t a.v b.v with Some Refl -> Some Refl | None -> None )
    | List a, List b -> (
        match t a.v b.v with Some Refl -> Some Refl | None -> None )
    | Tuple a, Tuple b -> tuple a b
    | Option a, Option b -> (
        match t a b with Some Refl -> Some Refl | None -> None )
    | Record a, Record b -> Witness.eq a.rwit b.rwit
    | Variant a, Variant b -> Witness.eq a.vwit b.vwit
    | _ -> None

  and custom : type a b. a custom -> b custom -> (a, b) eq option =
   fun a b ->
    match (a.cwit, b.cwit) with
    | `Witness a, `Witness b -> Witness.eq a b
    | `Type a, `Type b -> t a b
    | _ -> None

  and tuple : type a b. a tuple -> b tuple -> (a, b) eq option =
   fun a b ->
    match (a, b) with
    | Pair (a0, a1), Pair (b0, b1) -> (
        match (t a0 b0, t a1 b1) with
        | Some Refl, Some Refl -> Some Refl
        | _ -> None )
    | Triple (a0, a1, a2), Triple (b0, b1, b2) -> (
        match (t a0 b0, t a1 b1, t a2 b2) with
        | Some Refl, Some Refl, Some Refl -> Some Refl
        | _ -> None )
    | _ -> None
end

let unit = Prim Unit

let bool = Prim Bool

let char = Prim Char

let int = Prim Int

let int32 = Prim Int32

let int64 = Prim Int64

let float = Prim Float

let string = Prim (String `Int)

let bytes = Prim (Bytes `Int)

let string_of n = Prim (String n)

let bytes_of n = Prim (Bytes n)

let list ?(len = `Int) v = List { v; len }

let array ?(len = `Int) v = Array { v; len }

let pair a b = Tuple (Pair (a, b))

let triple a b c = Tuple (Triple (a, b, c))

let option a = Option a

let v ~cli ~json ~bin ~equal ~compare ~short_hash ~pre_hash =
  let pp, of_string = cli in
  let encode_json, decode_json = json in
  let encode_bin, decode_bin, size_of = bin in
  Custom
    {
      cwit = `Witness (Witness.make ());
      pp;
      of_string;
      pre_hash;
      encode_json;
      decode_json;
      encode_bin;
      decode_bin;
      size_of;
      compare;
      equal;
      short_hash;
    }

(* fix points *)

let mu : type a. (a t -> a t) -> a t =
 fun f ->
  let rec fake_x = { self = Self fake_x } in
  let real_x = f (Self fake_x) in
  fake_x.self <- real_x;
  real_x

let mu2 : type a b. (a t -> b t -> a t * b t) -> a t * b t =
 fun f ->
  let rec fake_x = { self = Self fake_x } in
  let rec fake_y = { self = Self fake_y } in
  let real_x, real_y = f (Self fake_x) (Self fake_y) in
  fake_x.self <- real_x;
  fake_y.self <- real_y;
  (real_x, real_y)

(* records *)

type ('a, 'b, 'c) open_record =
  ('a, 'c) fields -> string * 'b * ('a, 'b) fields

let field fname ftype fget = { fname; ftype; fget }

let record : string -> 'b -> ('a, 'b, 'b) open_record = fun n c fs -> (n, c, fs)

let app :
    type a b c d.
    (a, b, c -> d) open_record -> (a, c) field -> (a, b, d) open_record =
 fun r f fs ->
  let n, c, fs = r (F1 (f, fs)) in
  (n, c, fs)

let sealr : type a b. (a, b, a) open_record -> a t =
 fun r ->
  let rname, c, fs = r F0 in
  let rwit = Witness.make () in
  Record { rwit; rname; rfields = Fields (fs, c) }

let ( |+ ) = app

(* variants *)

type 'a case_p = 'a case_v

type ('a, 'b) case = int -> 'a a_case * 'b

let case0 cname0 c0 ctag0 =
  let c = { ctag0; cname0; c0 } in
  (C0 c, CV0 c)

let case1 cname1 ctype1 c1 ctag1 =
  let c = { ctag1; cname1; ctype1; c1 } in
  (C1 c, fun v -> CV1 (c, v))

type ('a, 'b, 'c) open_variant = 'a a_case list -> string * 'c * 'a a_case list

let variant n c vs = (n, c, vs)

let app v c cs =
  let n, fc, cs = v cs in
  let c, f = c (List.length cs) in
  (n, fc f, c :: cs)

let sealv v =
  let vname, vget, vcases = v [] in
  let vwit = Witness.make () in
  let vcases = Array.of_list (List.rev vcases) in
  Variant { vwit; vname; vcases; vget }

let ( |~ ) = app

let enum vname l =
  let vwit = Witness.make () in
  let _, vcases, mk =
    List.fold_left
      (fun (ctag0, cases, mk) (n, v) ->
        let c = { ctag0; cname0 = n; c0 = v } in
        (ctag0 + 1, C0 c :: cases, (v, CV0 c) :: mk))
      (0, [], []) l
  in
  let vcases = Array.of_list (List.rev vcases) in
  Variant { vwit; vname; vcases; vget = (fun x -> List.assq x mk) }

let rec fields_aux : type a b. (a, b) fields -> a a_field list = function
  | F0 -> []
  | F1 (h, t) -> Field h :: fields_aux t

let fields r = match r.rfields with Fields (f, _) -> fields_aux f

let result a b =
  variant "result" (fun ok error ->
    function Ok x -> ok x | Error x -> error x)
  |~ case1 "ok" a (fun a -> Ok a)
  |~ case1 "error" b (fun b -> Error b)
  |> sealv

module Equal = struct
  let unit _ _ = true

  let bool (x : bool) (y : bool) = x = y

  let char (x : char) (y : char) = x = y

  let int (x : int) (y : int) = x = y

  let int32 (x : int32) (y : int32) = x = y

  let int64 (x : int64) (y : int64) = x = y

  let string x y = x == y || String.equal x y

  let bytes x y = x == y || Bytes.equal x y

  (* NOTE: equality is ill-defined on float *)
  let float (x : float) (y : float) = x = y

  let list e x y =
    x == y || (List.length x = List.length y && List.for_all2 e x y)

  let array e x y =
    x == y
    || Array.length x = Array.length y
       &&
       let rec aux = function
         | -1 -> true
         | i -> e x.(i) y.(i) && aux (i - 1)
       in
       aux (Array.length x - 1)

  let pair ex ey ((x1, y1) as a) ((x2, y2) as b) =
    a == b || (ex x1 x2 && ey y1 y2)

  let triple ex ey ez ((x1, y1, z1) as a) ((x2, y2, z2) as b) =
    a == b || (ex x1 x2 && ey y1 y2 && ez z1 z2)

  let option e x y =
    x == y
    ||
    match (x, y) with
    | None, None -> true
    | Some x, Some y -> e x y
    | _ -> false

  let rec t : type a. a t -> a equal =
   fun ty a b ->
    match ty with
    | Self s -> t s.self a b
    | Custom c -> c.equal a b
    | Map m -> map m a b
    | Prim p -> prim p a b
    | List l -> list (t l.v) a b
    | Array x -> array (t x.v) a b
    | Tuple t -> tuple t a b
    | Option x -> option (t x) a b
    | Record r -> record r a b
    | Variant v -> variant v a b

  and tuple : type a. a tuple -> a equal = function
    | Pair (a, b) -> pair (t a) (t b)
    | Triple (a, b, c) -> triple (t a) (t b) (t c)

  and map : type a b. (a, b) map -> b equal =
   fun { x; g; _ } u v -> t x (g u) (g v)

  and prim : type a. a prim -> a equal = function
    | Unit -> unit
    | Bool -> bool
    | Char -> char
    | Int -> int
    | Int32 -> int32
    | Int64 -> int64
    | Float -> float
    | String _ -> string
    | Bytes _ -> bytes

  and record : type a. a record -> a equal =
   fun r x y -> List.for_all (function Field f -> field f x y) (fields r)

  and field : type a b. (a, b) field -> a equal =
   fun f x y -> t f.ftype (f.fget x) (f.fget y)

  and variant : type a. a variant -> a equal =
   fun v x y -> case_v (v.vget x) (v.vget y)

  and case_v : type a. a case_v equal =
   fun x y ->
    match (x, y) with
    | CV0 x, CV0 y -> int x.ctag0 y.ctag0
    | CV1 (x, vx), CV1 (y, vy) ->
        int x.ctag1 y.ctag1 && eq (x.ctype1, vx) (y.ctype1, vy)
    | _ -> false

  and eq : type a b. a t * a -> b t * b -> bool =
   fun (tx, x) (ty, y) ->
    match Refl.t tx ty with Some Refl -> t tx x y | None -> assert false

  (* this should never happen *)
end

let equal = Equal.t

module Compare = struct
  let unit (_ : unit) (_ : unit) = 0 [@@inline always]

  let bool (x : bool) (y : bool) = compare x y [@@inline always]

  let char x y = Char.compare x y [@@inline always]

  let int (x : int) (y : int) = compare x y [@@inline always]

  let int32 x y = Int32.compare x y [@@inline always]

  let int64 x y = Int64.compare x y [@@inline always]

  let float (x : float) (y : float) = compare x y [@@inline always]

  let string x y = if x == y then 0 else String.compare x y [@@inline always]

  let bytes x y = if x == y then 0 else Bytes.compare x y [@@inline always]

  let list c x y =
    if x == y then 0
    else
      let rec aux x y =
        match (x, y) with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | xx :: x, yy :: y -> ( match c xx yy with 0 -> aux x y | i -> i )
      in
      aux x y

  let array c x y =
    if x == y then 0
    else
      let lenx = Array.length x in
      let leny = Array.length y in
      if lenx > leny then 1
      else if lenx < leny then -1
      else
        let rec aux i =
          match c x.(i) y.(i) with
          | 0 when i + 1 = lenx -> 0
          | 0 -> aux (i + 1)
          | i -> i
        in
        aux 0

  let pair cx cy ((x1, y1) as a) ((x2, y2) as b) =
    if a == b then 0 else match cx x1 x2 with 0 -> cy y1 y2 | i -> i

  let triple cx cy cz ((x1, y1, z1) as a) ((x2, y2, z2) as b) =
    if a == b then 0
    else match cx x1 x2 with 0 -> pair cy cz (y1, z1) (y2, z2) | i -> i

  let option c x y =
    if x == y then 0
    else
      match (x, y) with
      | None, None -> 0
      | Some _, None -> 1
      | None, Some _ -> -1
      | Some x, Some y -> c x y

  let prim : type a. a prim -> a compare =
   fun ty a b ->
    match ty with
    | Unit -> (unit [@inlined]) a b
    | Bool -> (bool [@inlined]) a b
    | Char -> (char [@inlined]) a b
    | Int -> (int [@inlined]) a b
    | Int32 -> (int32 [@inlined]) a b
    | Int64 -> (int64 [@inlined]) a b
    | Float -> (float [@inlined]) a b
    | String _ -> (string [@inlined]) a b
    | Bytes _ -> (bytes [@inlined]) a b
   [@@inline always]

  let rec t : type a. a t -> a compare =
   fun ty a b ->
    match ty with
    | Self s -> t s.self a b
    | Custom c -> c.compare a b
    | Map m -> map m a b
    | Prim p -> (prim [@inlined]) p a b
    | List l -> list (t l.v) a b
    | Array x -> array (t x.v) a b
    | Tuple t -> tuple t a b
    | Option x -> option (t x) a b
    | Record r -> record r a b
    | Variant v -> variant v a b

  and tuple : type a. a tuple -> a compare = function
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. (a, b) map -> b compare =
   fun { x; g; _ } u v -> t x (g u) (g v)

  and record : type a. a record -> a compare =
   fun r x y ->
    let rec aux = function
      | [] -> 0
      | Field f :: t -> ( match field f x y with 0 -> aux t | i -> i )
    in
    aux (fields r)

  and field : type a b. (a, b) field -> a compare =
   fun f x y -> t f.ftype (f.fget x) (f.fget y)

  and variant : type a. a variant -> a compare =
   fun v x y -> case_v (v.vget x) (v.vget y)

  and case_v : type a. a case_v compare =
   fun x y ->
    match (x, y) with
    | CV0 x, CV0 y -> int x.ctag0 y.ctag0
    | CV0 x, CV1 (y, _) -> int x.ctag0 y.ctag1
    | CV1 (x, _), CV0 y -> int x.ctag1 y.ctag0
    | CV1 (x, vx), CV1 (y, vy) -> (
        match int x.ctag1 y.ctag1 with
        | 0 -> compare (x.ctype1, vx) (y.ctype1, vy)
        | i -> i )

  and compare : type a b. a t * a -> b t * b -> int =
   fun (tx, x) (ty, y) ->
    match Refl.t tx ty with Some Refl -> t tx x y | None -> assert false

  (* this should never happen *)
end

let compare t x y = Compare.t t x y

exception Not_utf8

let is_valid_utf8 str =
  try
    Uutf.String.fold_utf_8
      (fun _ _ -> function `Malformed _ -> raise Not_utf8 | _ -> ())
      () str;
    true
  with Not_utf8 -> false

module Encode_json = struct
  let lexeme e l = ignore (Jsonm.encode e (`Lexeme l))

  let unit e () = lexeme e `Null

  let base64 e s =
    let x = Base64.encode_exn s in
    lexeme e `Os;
    lexeme e (`Name "base64");
    lexeme e (`String x);
    lexeme e `Oe

  let string e s = if is_valid_utf8 s then lexeme e (`String s) else base64 e s

  let bytes e b =
    let s = Bytes.unsafe_to_string b in
    string e s

  let char e c =
    let s = String.make 1 c in
    string e s

  let float e f = lexeme e (`Float f)

  let int e i = float e (float_of_int i)

  let int32 e i = float e (Int32.to_float i)

  let int64 e i = float e (Int64.to_float i)

  let bool e = function false -> float e 0. | _ -> float e 1.

  let list l e x =
    lexeme e `As;
    List.iter (l e) x;
    lexeme e `Ae

  let array l e x =
    lexeme e `As;
    Array.iter (l e) x;
    lexeme e `Ae

  let pair a b e (x, y) =
    lexeme e `As;
    a e x;
    b e y;
    lexeme e `Ae

  let triple a b c e (x, y, z) =
    lexeme e `As;
    a e x;
    b e y;
    c e z;
    lexeme e `Ae

  let option o e = function None -> lexeme e `Null | Some x -> o e x

  let rec t : type a. a t -> a encode_json =
   fun ty e ->
    match ty with
    | Self s -> t s.self e
    | Custom c -> c.encode_json e
    | Map b -> map b e
    | Prim t -> prim t e
    | List l -> list (t l.v) e
    | Array a -> array (t a.v) e
    | Tuple t -> tuple t e
    | Option x -> option (t x) e
    | Record r -> record r e
    | Variant v -> variant v e

  and tuple : type a. a tuple -> a encode_json = function
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. (a, b) map -> b encode_json =
   fun { x; g; _ } e u -> t x e (g u)

  and prim : type a. a prim -> a encode_json = function
    | Unit -> unit
    | Bool -> bool
    | Char -> char
    | Int -> int
    | Int32 -> int32
    | Int64 -> int64
    | Float -> float
    | String _ -> string
    | Bytes _ -> bytes

  and record : type a. a record -> a encode_json =
   fun r e x ->
    let fields = fields r in
    lexeme e `Os;
    List.iter
      (fun (Field f) ->
        match (f.ftype, f.fget x) with
        | Option _, None -> ()
        | Option o, Some x ->
            lexeme e (`Name f.fname);
            t o e x
        | List _, [] -> ()
        | tx, x ->
            lexeme e (`Name f.fname);
            t tx e x)
      fields;
    lexeme e `Oe

  and variant : type a. a variant -> a encode_json =
   fun v e x -> case_v e (v.vget x)

  and case_v : type a. a case_v encode_json =
   fun e c ->
    match c with
    | CV0 c -> string e c.cname0
    | CV1 (c, v) ->
        lexeme e `Os;
        lexeme e (`Name c.cname1);
        t c.ctype1 e v;
        lexeme e `Oe
end

let encode_json = Encode_json.t

let pp_json ?minify t ppf x =
  let buf = Buffer.create 42 in
  let e = Jsonm.encoder ?minify (`Buffer buf) in
  encode_json t e x;
  ignore (Jsonm.encode e `End);
  Fmt.string ppf (Buffer.contents buf)

let pp t =
  let rec aux : type a. a t -> a pp =
   fun t ppf x ->
    match t with
    | Self s -> aux s.self ppf x
    | Custom c -> c.pp ppf x
    | Map m -> map m ppf x
    | Prim p -> prim p ppf x
    | _ -> pp_json t ppf x
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

let to_json_string ?minify t x = Fmt.to_to_string (pp_json ?minify t) x

module Decode_json = struct
  let lexeme e =
    match Json.decode e with
    | `Lexeme e -> Ok e
    | `Error e -> Error (`Msg (Fmt.to_to_string Jsonm.pp_error e))
    | `End | `Await -> assert false

  let ( >>= ) l f = match l with Error _ as e -> e | Ok l -> f l

  let ( >|= ) l f = match l with Ok l -> Ok (f l) | Error _ as e -> e

  let join = function Error _ as e -> e | Ok x -> x

  let error e got expected =
    let _, (l, c) = Jsonm.decoded_range e.Json.d in
    Error
      (`Msg
        (Fmt.strf
           "line %d, character %d:\n\
            Found lexeme %a, but lexeme %s was expected"
           l c Jsonm.pp_lexeme got expected))

  let expect_lexeme e expected =
    lexeme e >>= fun got ->
    if expected = got then Ok ()
    else error e got (Fmt.to_to_string Jsonm.pp_lexeme expected)

  (* read all lexemes until the end of the next well-formed value *)
  let value e =
    let lexemes = ref [] in
    let objs = ref 0 in
    let arrs = ref 0 in
    let rec aux () =
      lexeme e >>= fun l ->
      lexemes := l :: !lexemes;
      let () =
        match l with
        | `Os -> incr objs
        | `As -> incr arrs
        | `Oe -> decr objs
        | `Ae -> decr arrs
        | `Name _ | `Null | `Bool _ | `String _ | `Float _ -> ()
      in
      if !objs > 0 || !arrs > 0 then aux () else Ok ()
    in
    aux () >|= fun () -> List.rev !lexemes

  let unit e = expect_lexeme e `Null

  let get_base64_value e =
    match lexeme e with
    | Ok (`Name "base64") -> (
        match lexeme e with
        | Ok (`String b) -> (
            match expect_lexeme e `Oe with
            | Ok () -> Ok (Base64.decode_exn b)
            | Error e -> Error e )
        | Ok l -> error e l "Bad base64 encoded character"
        | Error e -> Error e )
    | Ok l -> error e l "Invalid base64 object"
    | Error e -> Error e

  let string e =
    lexeme e >>= function
    | `String s -> Ok s
    | `Os -> get_base64_value e
    | l -> error e l "`String"

  let bytes e =
    lexeme e >>= function
    | `String s -> Ok (Bytes.unsafe_of_string s)
    | `Os -> (
        match get_base64_value e with
        | Ok s -> Ok (Bytes.unsafe_of_string s)
        | Error e -> Error e )
    | l -> error e l "`String"

  let float e =
    lexeme e >>= function `Float f -> Ok f | l -> error e l "`Float"

  let char e =
    lexeme e >>= function
    | `String s when String.length s = 1 -> Ok s.[0]
    | `Os -> (
        match get_base64_value e with Ok s -> Ok s.[0] | Error x -> Error x )
    | l -> error e l "`String[0]"

  let int32 e = float e >|= Int32.of_float

  let int64 e = float e >|= Int64.of_float

  let int e = float e >|= int_of_float

  let bool e = int e >|= function 0 -> false | _ -> true

  let list l e =
    expect_lexeme e `As >>= fun () ->
    let rec aux acc =
      lexeme e >>= function
      | `Ae -> Ok (List.rev acc)
      | lex ->
          Json.rewind e lex;
          l e >>= fun v -> aux (v :: acc)
    in
    aux []

  let array l e = list l e >|= Array.of_list

  let pair a b e =
    expect_lexeme e `As >>= fun () ->
    a e >>= fun x ->
    b e >>= fun y ->
    expect_lexeme e `Ae >|= fun () -> (x, y)

  let triple a b c e =
    expect_lexeme e `As >>= fun () ->
    a e >>= fun x ->
    b e >>= fun y ->
    c e >>= fun z ->
    expect_lexeme e `Ae >|= fun () -> (x, y, z)

  let option o e =
    lexeme e >>= function
    | `Null -> Ok None
    | lex ->
        Json.rewind e lex;
        o e >|= fun v -> Some v

  let rec t : type a. a t -> a decode_json =
   fun ty d ->
    match ty with
    | Self s -> t s.self d
    | Custom c -> c.decode_json d
    | Map b -> map b d
    | Prim t -> prim t d
    | List l -> list (t l.v) d
    | Array a -> array (t a.v) d
    | Tuple t -> tuple t d
    | Option x -> option (t x) d
    | Record r -> record r d
    | Variant v -> variant v d

  and tuple : type a. a tuple -> a decode_json = function
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. (a, b) map -> b decode_json =
   fun { x; f; _ } e -> t x e >|= f

  and prim : type a. a prim -> a decode_json = function
    | Unit -> unit
    | Bool -> bool
    | Char -> char
    | Int -> int
    | Int32 -> int32
    | Int64 -> int64
    | Float -> float
    | String _ -> string
    | Bytes _ -> bytes

  and record : type a. a record -> a decode_json =
   fun r e ->
    expect_lexeme e `Os >>= fun () ->
    let rec soup acc =
      lexeme e >>= function
      | `Name n -> value e >>= fun s -> soup ((n, s) :: acc)
      | `Oe -> Ok acc
      | l -> error e l "`Record-contents"
    in
    soup [] >>= fun soup ->
    let rec aux :
        type a b. (a, b) fields -> b -> (a, [ `Msg of string ]) result =
     fun f c ->
      match f with
      | F0 -> Ok c
      | F1 (h, f) -> (
          let v =
            try
              let s = List.assoc h.fname soup in
              let e = Json.decoder_of_lexemes s in
              t h.ftype e
            with Not_found -> (
              match h.ftype with
              | Option _ -> Ok None
              | List _ -> Ok []
              | _ ->
                  Error
                    (`Msg (Fmt.strf "missing value for %s.%s" r.rname h.fname)) )
          in
          match v with Ok v -> aux f (c v) | Error _ as e -> e )
    in
    let (Fields (f, c)) = r.rfields in
    aux f c

  and variant : type a. a variant -> a decode_json =
   fun v e ->
    lexeme e >>= function
    | `String s -> case0 s v e
    | `Os -> case1 v e
    | l -> error e l "(`String | `Os)"

  and case0 : type a. string -> a variant -> a decode_json =
   fun s v _e ->
    let rec aux i =
      match v.vcases.(i) with
      | C0 c when String.compare c.cname0 s = 0 -> Ok c.c0
      | _ ->
          if i < Array.length v.vcases then aux (i + 1)
          else Error (`Msg "variant")
    in
    aux 0

  and case1 : type a. a variant -> a decode_json =
   fun v e ->
    lexeme e >>= function
    | `Name s ->
        let rec aux i =
          match v.vcases.(i) with
          | C1 c when String.compare c.cname1 s = 0 -> t c.ctype1 e >|= c.c1
          | _ ->
              if i < Array.length v.vcases then aux (i + 1)
              else Error (`Msg "variant")
        in
        aux 0 >>= fun c ->
        expect_lexeme e `Oe >|= fun () -> c
    | l -> error e l "`Name"
end

let decode_json x d = Decode_json.(t x @@ { Json.d; lexemes = [] })

let decode_json_lexemes x ls = Decode_json.(t x @@ Json.decoder_of_lexemes ls)

let of_json_string x s = Decode_json.(t x @@ Json.decoder (`String s))

module Size_of = struct
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

  let string ?(headers = true) n s =
    let s = String.length s in
    if not headers then s else len s n + s

  let bytes ?(headers = true) n s =
    let s = Bytes.length s in
    if not headers then s else len s n + s

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

  let rec t : type a. a t -> a size_of =
   fun ty ?headers e ->
    match ty with
    | Self s -> t ?headers s.self e
    | Custom c -> c.size_of ?headers e
    | Map b -> map ?headers b e
    | Prim t -> prim ?headers t e
    | List l -> list (t l.v) l.len e
    | Array a -> array (t a.v) a.len e
    | Tuple t -> tuple ?headers t e
    | Option x -> option (t x) e
    | Record r -> record ?headers r e
    | Variant v -> variant ?headers v e

  and tuple : type a. a tuple -> a size_of =
   fun ty ?headers:_ ->
    match ty with
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. (a, b) map -> b size_of =
   fun { x; g; _ } ?headers u -> t ?headers x (g u)

  and prim : type a. a prim -> a size_of =
   fun p ?headers x ->
    match p with
    | Unit -> Some (unit x)
    | Bool -> Some (bool x)
    | Char -> Some (char x)
    | Int -> Some (int x)
    | Int32 -> Some (int32 x)
    | Int64 -> Some (int64 x)
    | Float -> Some (float x)
    | String n -> Some (string ?headers n x)
    | Bytes n -> Some (bytes ?headers n x)

  and record : type a. a record -> a size_of =
   fun r ?headers:_ x ->
    let fields = fields r in
    List.fold_left
      (fun acc (Field f) ->
        acc >>= fun acc ->
        field f x >|= fun f -> acc + f)
      (Some 0) fields

  and field : type a b. (a, b) field -> a size_of =
   fun f ?headers:_ x -> t f.ftype (f.fget x)

  and variant : type a. a variant -> a size_of =
   fun v ?headers:_ x ->
    match v.vget x with
    | CV0 v -> Some (int v.ctag0)
    | CV1 (x, vx) -> t x.ctype1 vx >|= fun v -> int x.ctag1 + v
end

let size_of = Size_of.t

module B = struct
  external get_16 : string -> int -> int = "%caml_string_get16"

  external get_32 : string -> int -> int32 = "%caml_string_get32"

  external get_64 : string -> int -> int64 = "%caml_string_get64"

  external set_16 : Bytes.t -> int -> int -> unit = "%caml_string_set16u"

  external set_32 : Bytes.t -> int -> int32 -> unit = "%caml_string_set32u"

  external set_64 : Bytes.t -> int -> int64 -> unit = "%caml_string_set64u"

  external swap16 : int -> int = "%bswap16"

  external swap32 : int32 -> int32 = "%bswap_int32"

  external swap64 : int64 -> int64 = "%bswap_int64"

  let get_uint16 s off =
    if not Sys.big_endian then swap16 (get_16 s off) else get_16 s off

  let get_uint32 s off =
    if not Sys.big_endian then swap32 (get_32 s off) else get_32 s off

  let get_uint64 s off =
    if not Sys.big_endian then swap64 (get_64 s off) else get_64 s off

  let set_uint16 s off v =
    if not Sys.big_endian then set_16 s off (swap16 v) else set_16 s off v

  let set_uint32 s off v =
    if not Sys.big_endian then set_32 s off (swap32 v) else set_32 s off v

  let set_uint64 s off v =
    if not Sys.big_endian then set_64 s off (swap64 v) else set_64 s off v
end

module Encode_bin = struct
  let unit () _k = ()

  let add_bytes b k = k (Bytes.to_string b)

  let add_string s k = k s

  let char c = add_bytes (Bytes.make 1 c)

  let int8 i = char (Char.chr i)

  let int16 i =
    let b = Bytes.create 2 in
    B.set_uint16 b 0 i;
    add_bytes b

  let int32 i =
    let b = Bytes.create 4 in
    B.set_uint32 b 0 i;
    add_bytes b

  let int64 i =
    let b = Bytes.create 8 in
    B.set_uint64 b 0 i;
    add_bytes b

  let float f = int64 (Int64.bits_of_float f)

  let bool b = char (if b then '\255' else '\000')

  let int i k =
    let rec aux n =
      if n >= 0 && n < 128 then int8 n k
      else
        let out = 128 + (n land 127) in
        int8 out k;
        aux (n lsr 7)
    in
    aux i

  let len n i =
    match n with
    | `Int -> int i
    | `Int8 -> int8 i
    | `Int16 -> int16 i
    | `Int32 -> int32 (Int32.of_int i)
    | `Int64 -> int64 (Int64.of_int i)
    | `Fixed _ -> unit ()

  let string ?(headers = true) n s k =
    if not headers then add_string s k
    else
      let i = String.length s in
      len n i k;
      add_string s k

  let bytes ?(headers = true) n s k =
    if not headers then add_bytes s k
    else
      let i = Bytes.length s in
      len n i k;
      add_bytes s k

  let list l n x k =
    len n (List.length x) k;
    List.iter (fun e -> l e k) x

  let array l n x k =
    len n (Array.length x) k;
    Array.iter (fun e -> l e k) x

  let pair a b (x, y) k =
    a x k;
    b y k

  let triple a b c (x, y, z) k =
    a x k;
    b y k;
    c z k

  let option o v k =
    match v with
    | None -> char '\000' k
    | Some x ->
        char '\255' k;
        o x k

  let rec t : type a. a t -> a encode_bin =
   fun ty ?headers e k ->
    match ty with
    | Self s -> t ?headers s.self e k
    | Custom c -> c.encode_bin ?headers e k
    | Map b -> map ?headers b e k
    | Prim t -> prim ?headers t e k
    | List l -> list (t l.v) l.len e k
    | Array a -> array (t a.v) a.len e k
    | Tuple t -> tuple ?headers t e k
    | Option x -> option (t x) e k
    | Record r -> record ?headers r e k
    | Variant v -> variant ?headers v e k

  and tuple : type a. a tuple -> a encode_bin =
   fun ty ?headers:_ ->
    match ty with
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. (a, b) map -> b encode_bin =
   fun { x; g; _ } ?headers u k -> t ?headers x (g u) k

  and prim : type a. a prim -> a encode_bin =
   fun ty ?headers ->
    match ty with
    | Unit -> unit
    | Bool -> bool
    | Char -> char
    | Int -> int
    | Int32 -> int32
    | Int64 -> int64
    | Float -> float
    | String n -> string ?headers n
    | Bytes n -> bytes ?headers n

  and record : type a. a record -> a encode_bin =
   fun r ?headers:_ x k ->
    let fields = fields r in
    List.iter (fun (Field f) -> t f.ftype (f.fget x) k) fields

  and variant : type a. a variant -> a encode_bin =
   fun v ?headers:_ x k -> case_v (v.vget x) k

  and case_v : type a. a case_v encode_bin =
   fun ?headers:_ c k ->
    match c with
    | CV0 c -> int c.ctag0 k
    | CV1 (c, v) ->
        int c.ctag1 k;
        t c.ctype1 v k
end

let encode_bin = Encode_bin.t

let to_bin size_of encode_bin x =
  let seq = encode_bin ?headers:(Some false) x in
  let len =
    match size_of ?headers:(Some false) x with None -> 1024 | Some n -> n
  in
  let buf = Buffer.create len in
  seq (Buffer.add_string buf);
  Buffer.contents buf

let to_bin_string t x =
  let rec aux : type a. a t -> a -> string =
   fun t x ->
    match t with
    | Self s -> aux s.self x
    | Map m -> aux m.x (m.g x)
    | Prim (String _) -> x
    | Prim (Bytes _) -> Bytes.to_string x
    | Custom c -> to_bin c.size_of c.encode_bin x
    | _ -> to_bin (size_of t) (encode_bin t) x
  in
  aux t x

let pre_hash t x =
  let rec aux : type a. a t -> a bin_seq =
   fun t v k ->
    match t with
    | Self s -> aux s.self v k
    | Map m -> aux m.x (m.g v) k
    | Custom c -> c.pre_hash v k
    | _ -> encode_bin ?headers:(Some false) t v k
  in
  aux t x

module Decode_bin = struct
  let ( >|= ) (ofs, x) f = (ofs, f x)

  let ( >>= ) (ofs, x) f = f (ofs, x)

  let ok ofs x = (ofs, x)

  type 'a res = int * 'a

  let unit _ ofs = ok ofs ()

  let char buf ofs = ok (ofs + 1) buf.[ofs]

  let int8 buf ofs = char buf ofs >|= Char.code

  let int16 buf ofs = ok (ofs + 2) (B.get_uint16 buf ofs)

  let int32 buf ofs = ok (ofs + 4) (B.get_uint32 buf ofs)

  let int64 buf ofs = ok (ofs + 8) (B.get_uint64 buf ofs)

  let bool buf ofs = char buf ofs >|= function '\000' -> false | _ -> true

  let float buf ofs = int64 buf ofs >|= Int64.float_of_bits

  let int buf ofs =
    let rec aux n p ofs =
      int8 buf ofs >>= fun (ofs, i) ->
      let n = n + ((i land 127) lsl (p * 7)) in
      if i >= 0 && i < 128 then (ofs, n) else aux n (p + 1) ofs
    in
    aux 0 0 ofs

  let len buf ofs = function
    | `Int -> int buf ofs
    | `Int8 -> int8 buf ofs
    | `Int16 -> int16 buf ofs
    | `Int32 -> int32 buf ofs >|= Int32.to_int
    | `Int64 -> int64 buf ofs >|= Int64.to_int
    | `Fixed n -> ok ofs n

  let fixed_size = function `Fixed n -> n | _ -> -1

  let string ?(headers = true) n buf ofs =
    let f = fixed_size n in
    if (not headers) && f = String.length buf then ok f buf
    else
      len buf ofs n >>= fun (ofs, len) ->
      let str = Bytes.create len in
      String.blit buf ofs str 0 len;
      ok (ofs + len) (Bytes.unsafe_to_string str)

  let bytes ?(headers = true) n buf ofs =
    let f = fixed_size n in
    if (not headers) && f = String.length buf then ok f (Bytes.of_string buf)
    else
      len buf ofs n >>= fun (ofs, len) ->
      let str = Bytes.create len in
      String.blit buf ofs str 0 len;
      ok (ofs + len) str

  let list l n buf ofs =
    len buf ofs n >>= fun (ofs, len) ->
    let rec aux acc ofs = function
      | 0 -> ok ofs (List.rev acc)
      | n -> l buf ofs >>= fun (ofs, x) -> aux (x :: acc) ofs (n - 1)
    in
    aux [] ofs len

  let array l len buf ofs = list l len buf ofs >|= Array.of_list

  let pair a b buf ofs =
    a buf ofs >>= fun (ofs, a) ->
    b buf ofs >|= fun b -> (a, b)

  let triple a b c buf ofs =
    a buf ofs >>= fun (ofs, a) ->
    b buf ofs >>= fun (ofs, b) ->
    c buf ofs >|= fun c -> (a, b, c)

  let option : type a. a decode_bin -> a option decode_bin =
   fun o ?headers:_ buf ofs ->
    char buf ofs >>= function
    | ofs, '\000' -> ok ofs None
    | ofs, _ -> o buf ofs >|= fun x -> Some x

  let rec t : type a. a t -> a decode_bin =
   fun ty ?headers buf ofs ->
    match ty with
    | Self s -> t ?headers s.self buf ofs
    | Custom c -> c.decode_bin ?headers buf ofs
    | Map b -> map ?headers b buf ofs
    | Prim t -> prim ?headers t buf ofs
    | List l -> list (t l.v) l.len buf ofs
    | Array a -> array (t a.v) a.len buf ofs
    | Tuple t -> tuple ?headers t buf ofs
    | Option x -> option ?headers (t x) buf ofs
    | Record r -> record ?headers r buf ofs
    | Variant v -> variant ?headers v buf ofs

  and tuple : type a. a tuple -> a decode_bin =
   fun ty ?headers:_ ->
    match ty with
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. (a, b) map -> b decode_bin =
   fun { x; f; _ } ?headers buf ofs -> t ?headers x buf ofs >|= f

  and prim : type a. a prim -> a decode_bin =
   fun ty ?headers ->
    match ty with
    | Unit -> unit
    | Bool -> bool
    | Char -> char
    | Int -> int
    | Int32 -> int32
    | Int64 -> int64
    | Float -> float
    | String n -> string ?headers n
    | Bytes n -> bytes ?headers n

  and record : type a. a record -> a decode_bin =
   fun r ?headers:_ buf ofs ->
    match r.rfields with
    | Fields (fs, c) ->
        let rec aux : type b. int -> b -> (a, b) fields -> a res =
         fun ofs f -> function
          | F0 -> ok ofs f
          | F1 (h, t) -> field h buf ofs >>= fun (ofs, x) -> aux ofs (f x) t
        in
        aux ofs c fs

  and field : type a b. (a, b) field -> b decode_bin = fun f -> t f.ftype

  and variant : type a. a variant -> a decode_bin =
   fun v ?headers:_ buf ofs ->
    int buf ofs >>= fun (ofs, i) -> case v.vcases.(i) buf ofs

  and case : type a. a a_case -> a decode_bin =
   fun c ?headers:_ buf ofs ->
    match c with C0 c -> ok ofs c.c0 | C1 c -> t c.ctype1 buf ofs >|= c.c1
end

let map_result f = function Ok x -> Ok (f x) | Error _ as e -> e

let decode_bin = Decode_bin.t

let of_bin decode_bin x =
  let last, v = decode_bin ?headers:(Some false) x 0 in
  assert (last = String.length x);
  Ok v

let of_bin_string t x =
  let rec aux : type a. a t -> string -> (a, [ `Msg of string ]) result =
   fun t x ->
    match t with
    | Self s -> aux s.self x
    | Map l -> aux l.x x |> map_result l.f
    | Prim (String _) -> Ok x
    | Prim (Bytes _) -> Ok (Bytes.of_string x)
    | Custom c -> of_bin c.decode_bin x
    | _ -> of_bin (decode_bin t) x
  in
  try aux t x with Invalid_argument e -> Error (`Msg e)

let to_string t = Fmt.to_to_string (pp t)

let of_string t =
  let v f x = try Ok (f x) with Invalid_argument e -> Error (`Msg e) in
  let rec aux : type a a. a t -> a of_string =
   fun t x ->
    match t with
    | Self s -> aux s.self x
    | Custom c -> c.of_string x
    | Map m -> aux m.x x |> map_result m.f
    | Prim p -> prim p x
    | _ -> of_json_string t x
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

type 'a ty = 'a t

let short_hash t ?seed x =
  match t with
  | Custom c -> c.short_hash ?seed x
  | _ ->
      let seed = match seed with None -> 0 | Some t -> t in
      let h = ref seed in
      pre_hash t x (fun s -> h := Hashtbl.seeded_hash !h s);
      !h

let like ?cli ?json ?bin ?equal ?compare ?short_hash:h ?pre_hash:p t =
  let encode_json, decode_json =
    match json with
    | Some (x, y) -> (x, y)
    | None -> (
        let rec is_prim : type a. a t -> bool = function
          | Self s -> is_prim s.self
          | Map m -> is_prim m.x
          | Prim _ -> true
          | _ -> false
        in
        match (t, cli) with
        | ty, Some (pp, of_string) when is_prim ty ->
            let ty = string in
            ( (fun ppf u -> Encode_json.t ty ppf (Fmt.to_to_string pp u)),
              fun buf -> Decode_json.(t ty buf >|= of_string |> join) )
        | _ -> (Encode_json.t t, Decode_json.t t) )
  in
  let pp, of_string =
    match cli with Some (x, y) -> (x, y) | None -> (pp t, of_string t)
  in
  let encode_bin, decode_bin, size_of =
    match bin with
    | Some (x, y, z) -> (x, y, z)
    | None -> (encode_bin t, decode_bin t, size_of t)
  in
  let equal =
    match equal with
    | Some x -> x
    | None -> (
        match compare with Some f -> fun x y -> f x y = 0 | None -> Equal.t t )
  in
  let compare = match compare with Some x -> x | None -> Compare.t t in
  let short_hash ?seed =
    match h with Some x -> x | None -> short_hash ?seed t
  in
  let pre_hash =
    match p with Some x -> x | None -> encode_bin ?headers:(Some false)
  in
  Custom
    {
      cwit = `Type t;
      pp;
      of_string;
      encode_json;
      decode_json;
      encode_bin;
      decode_bin;
      size_of;
      compare;
      equal;
      short_hash;
      pre_hash;
    }

let map ?cli ?json ?bin ?equal ?compare ?short_hash ?pre_hash x f g =
  match (cli, json, bin, equal, compare, short_hash, pre_hash) with
  | None, None, None, None, None, None, None ->
      Map { x; f; g; mwit = Witness.make () }
  | _ ->
      let x = Map { x; f; g; mwit = Witness.make () } in
      like ?cli ?json ?bin ?equal ?compare ?short_hash ?pre_hash x

module type S = sig
  type t

  val t : t ty
end
