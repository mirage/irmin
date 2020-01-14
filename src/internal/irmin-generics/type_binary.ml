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

module Encode = struct
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

module Decode = struct
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

let encode_bin = Encode.t

let decode_bin = Decode.t

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
    | _ -> to_bin (Type_size.t t) (Encode.t t) x
  in
  aux t x

let map_result f = function Ok x -> Ok (f x) | Error _ as e -> e

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
    | _ -> of_bin (Decode.t t) x
  in
  try aux t x with Invalid_argument e -> Error (`Msg e)
