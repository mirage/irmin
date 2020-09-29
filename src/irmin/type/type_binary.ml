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
open Staging
open Utils

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
    | `Unboxed -> unit ()

  let unboxed_string _ = add_string

  let boxed_string n =
    let len = len n in
    fun s k ->
      let i = String.length s in
      len i k;
      add_string s k

  let string boxed = if boxed then boxed_string else unboxed_string

  let unboxed_bytes _ = add_bytes

  let boxed_bytes n =
    let len = len n in
    fun s k ->
      let i = Bytes.length s in
      len i k;
      add_bytes s k

  let bytes boxed = if boxed then boxed_bytes else unboxed_bytes

  let list l n =
    let l = unstage l in
    stage (fun x k ->
        len n (List.length x) k;
        List.iter (fun e -> l e k) x)

  let array l n =
    let l = unstage l in
    stage (fun x k ->
        len n (Array.length x) k;
        Array.iter (fun e -> l e k) x)

  let pair a b =
    let a = unstage a and b = unstage b in
    stage (fun (x, y) k ->
        a x k;
        b y k)

  let triple a b c =
    let a = unstage a and b = unstage b and c = unstage c in
    stage (fun (x, y, z) k ->
        a x k;
        b y k;
        c z k)

  let option o =
    let o = unstage o in
    stage (fun v k ->
        match v with
        | None -> char '\000' k
        | Some x ->
            char '\255' k;
            o x k)

  let rec t : type a. a t -> a encode_bin = function
    | Self s ->
        fix_staged (fun encode_bin ->
            t (s.self_unroll (partial ~encode_bin ())))
    | Custom c -> c.encode_bin
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

  and unboxed : type a. a t -> a encode_bin = function
    | Self s ->
        fix_staged (fun unboxed_encode_bin ->
            unboxed (s.self_unroll (partial ~unboxed_encode_bin ())))
    | Custom c -> c.unboxed_encode_bin
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

  and tuple : type a. a tuple -> a encode_bin = function
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. boxed:bool -> (a, b) map -> b encode_bin =
   fun ~boxed { x; g; _ } ->
    let encode_bin = unstage (if boxed then t x else unboxed x) in
    stage (fun u k -> encode_bin (g u) k)

  and prim : type a. boxed:bool -> a prim -> a encode_bin =
   fun ~boxed -> function
    | Unit -> stage unit
    | Bool -> stage bool
    | Char -> stage char
    | Int -> stage int
    | Int32 -> stage int32
    | Int64 -> stage int64
    | Float -> stage float
    | String n -> stage (string boxed n)
    | Bytes n -> stage (bytes boxed n)

  and record : type a. a record -> a encode_bin =
   fun r ->
    let field_encoders : (a -> (string -> unit) -> unit) list =
      fields r
      |> List.map @@ fun (Field f) ->
         let field_encode = unstage (t f.ftype) in
         fun x -> field_encode (f.fget x)
    in
    stage (fun x k -> List.iter (fun f -> f x k) field_encoders)

  and variant : type a. a variant -> a encode_bin =
    let c0 { ctag0; _ } = stage (int ctag0) in
    let c1 c =
      let encode_arg = unstage (t c.ctype1) in
      stage (fun v k ->
          int c.ctag1 k;
          encode_arg v k)
    in
    fun v -> fold_variant { c0; c1 } v
end

module Decode = struct
  type 'a res = int * 'a

  let unit _ ofs = (ofs, ())

  let char buf ofs = (ofs + 1, buf.[ofs])

  let int8 buf ofs =
    let ofs, c = char buf ofs in
    (ofs, Char.code c)

  let int16 buf ofs = (ofs + 2, B.get_uint16 buf ofs)

  let int32 buf ofs = (ofs + 4, B.get_uint32 buf ofs)

  let int64 buf ofs = (ofs + 8, B.get_uint64 buf ofs)

  let bool buf ofs =
    let ofs, c = char buf ofs in
    match c with '\000' -> (ofs, false) | _ -> (ofs, true)

  let float buf ofs =
    let ofs, f = int64 buf ofs in
    (ofs, Int64.float_of_bits f)

  let int buf ofs =
    let rec aux n p ofs =
      let ofs, i = int8 buf ofs in
      let n = n + ((i land 127) lsl (p * 7)) in
      if i >= 0 && i < 128 then (ofs, n) else aux n (p + 1) ofs
    in
    aux 0 0 ofs

  let len buf ofs = function
    | `Int -> int buf ofs
    | `Int8 -> int8 buf ofs
    | `Int16 -> int16 buf ofs
    | `Int32 ->
        let ofs, i = int32 buf ofs in
        (ofs, Int32.to_int i)
    | `Int64 ->
        let ofs, i = int64 buf ofs in
        (ofs, Int64.to_int i)
    | `Fixed n -> (ofs, n)
    | `Unboxed -> (ofs, String.length buf - ofs)

  let mk_unboxed of_string of_bytes _ buf ofs =
    let len = String.length buf - ofs in
    if ofs = 0 then (len, of_string buf)
    else
      let str = Bytes.create len in
      String.blit buf ofs str 0 len;
      (ofs + len, of_bytes str)

  let mk_boxed of_string of_bytes n =
    let sub len buf ofs =
      if ofs = 0 && len = String.length buf then (len, of_string buf)
      else
        let str = Bytes.create len in
        String.blit buf ofs str 0 len;
        (ofs + len, of_bytes str)
    in
    match n with
    | `Fixed n -> sub n (* fixed-size strings are never boxed *)
    | n ->
        fun buf ofs ->
          let ofs, len = len buf ofs n in
          sub len buf ofs

  let mk of_string of_bytes =
    let f_boxed = mk_boxed of_string of_bytes in
    let f_unboxed = mk_unboxed of_string of_bytes in
    fun boxed -> if boxed then f_boxed else f_unboxed

  let string = mk (fun x -> x) Bytes.unsafe_to_string

  let bytes = mk Bytes.of_string (fun x -> x)

  let list l n =
    let l = unstage l in
    stage (fun buf ofs ->
        let ofs, len = len buf ofs n in
        let rec aux acc ofs = function
          | 0 -> (ofs, List.rev acc)
          | n ->
              let ofs, x = l buf ofs in
              aux (x :: acc) ofs (n - 1)
        in
        aux [] ofs len)

  let array l len =
    let decode_list = unstage (list l len) in
    stage (fun buf ofs ->
        let ofs, l = decode_list buf ofs in
        (ofs, Array.of_list l))

  let pair a b =
    let a = unstage a and b = unstage b in
    stage (fun buf ofs ->
        let ofs, a = a buf ofs in
        let ofs, b = b buf ofs in
        (ofs, (a, b)))

  let triple a b c =
    let a = unstage a and b = unstage b and c = unstage c in
    stage (fun buf ofs ->
        let ofs, a = a buf ofs in
        let ofs, b = b buf ofs in
        let ofs, c = c buf ofs in
        (ofs, (a, b, c)))

  let option : type a. a decode_bin -> a option decode_bin =
   fun o ->
    let o = unstage o in
    stage (fun buf ofs ->
        let ofs, c = char buf ofs in
        match c with
        | '\000' -> (ofs, None)
        | _ ->
            let ofs, x = o buf ofs in
            (ofs, Some x))

  module Record_decoder = Fields_folder (struct
    type ('a, 'b) t = string -> int -> 'b -> 'a res [@@deriving branded]
  end)

  let rec t : type a. a t -> a decode_bin = function
    | Self s ->
        fix_staged (fun decode_bin ->
            t (s.self_unroll (partial ~decode_bin ())))
    | Custom c -> c.decode_bin
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

  and unboxed : type a. a t -> a decode_bin = function
    | Self s ->
        fix_staged (fun unboxed_decode_bin ->
            t (s.self_unroll (partial ~unboxed_decode_bin ())))
    | Custom c -> c.unboxed_decode_bin
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

  and tuple : type a. a tuple -> a decode_bin = function
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. boxed:bool -> (a, b) map -> b decode_bin =
   fun ~boxed { x; f; _ } ->
    let decode_bin = unstage (if boxed then t x else unboxed x) in
    stage (fun buf ofs ->
        let ofs, x = decode_bin buf ofs in
        (ofs, f x))

  and prim : type a. boxed:bool -> a prim -> a decode_bin =
   fun ~boxed -> function
    | Unit -> stage unit
    | Bool -> stage bool
    | Char -> stage char
    | Int -> stage int
    | Int32 -> stage int32
    | Int64 -> stage int64
    | Float -> stage float
    | String n -> stage (string boxed n)
    | Bytes n -> stage (bytes boxed n)

  and record : type a. a record -> a decode_bin =
   fun { rfields = Fields (fs, constr); _ } ->
    let nil _buf ofs f = (ofs, f) in
    let cons { ftype; _ } decode_remaining =
      let f_decode = unstage (t ftype) in
      fun buf ofs constr ->
        let ofs, x = f_decode buf ofs in
        let constr = constr x in
        decode_remaining buf ofs constr
    in
    let f = Record_decoder.fold { nil; cons } fs in
    stage (fun buf ofs -> f buf ofs constr)

  and variant : type a. a variant -> a decode_bin =
   fun v ->
    let decoders : a decode_bin array =
      v.vcases
      |> Array.map @@ function
         | C0 c -> stage (fun _ ofs -> (ofs, c.c0))
         | C1 c ->
             let decode_arg = unstage (t c.ctype1) in
             stage (fun buf ofs ->
                 let ofs, x = decode_arg buf ofs in
                 (ofs, c.c1 x))
    in
    stage (fun buf ofs ->
        let ofs, i = int buf ofs in
        unstage decoders.(i) buf ofs)
end

let encode_bin = Encode.t

let decode_bin = Decode.t

type 'a to_bin_string = 'a to_string staged

type 'a of_bin_string = 'a of_string staged

module Unboxed = struct
  let encode_bin = Encode.unboxed

  let decode_bin = Decode.unboxed
end

let to_bin size_of encode_bin =
  let size_of = unstage size_of in
  let encode_bin = unstage encode_bin in
  stage (fun x ->
      let seq = encode_bin x in
      let len = match size_of x with None -> 1024 | Some n -> n in
      let buf = Buffer.create len in
      seq (Buffer.add_string buf);
      Buffer.contents buf)

let to_bin_string =
  let rec aux : type a. a t -> a to_bin_string =
   fun t ->
    match t with
    | Self s -> aux s.self_fix
    | Map m ->
        let mapped = unstage (aux m.x) in
        stage (fun x -> mapped (m.g x))
    | Prim (String _) -> stage (fun x -> x)
    | Prim (Bytes _) -> stage Bytes.to_string
    | Custom c -> to_bin c.unboxed_size_of c.unboxed_encode_bin
    | _ -> to_bin (Type_size.unboxed t) (Encode.unboxed t)
  in
  aux

let map_result f = function Ok x -> Ok (f x) | Error _ as e -> e

let of_bin decode_bin x =
  let last, v = decode_bin x 0 in
  assert (last = String.length x);
  Ok v

let of_bin_string t =
  let rec aux : type a. a t -> a of_bin_string =
   fun t ->
    match t with
    | Self s -> aux s.self_fix
    | Map l ->
        let mapped = unstage (aux l.x) in
        stage (fun x -> mapped x |> map_result l.f)
    | Prim (String _) -> stage (fun x -> Ok x)
    | Prim (Bytes _) -> stage (fun x -> Ok (Bytes.of_string x))
    | Custom c -> stage (of_bin (unstage c.unboxed_decode_bin))
    | _ -> stage (of_bin (unstage (Decode.unboxed t)))
  in
  let f = unstage (aux t) in
  stage (fun x -> try f x with Invalid_argument e -> Error (`Msg e))
