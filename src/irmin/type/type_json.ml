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

module Encode = struct
  let lexeme e l = ignore (Jsonm.encode e (`Lexeme l))

  let unit e () =
    lexeme e `Os;
    lexeme e `Oe

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

  let boxed_option o e = function
    | None -> lexeme e `Null
    | Some x ->
        lexeme e `Os;
        lexeme e (`Name "some");
        o e x;
        lexeme e `Oe

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
    | Option x -> boxed_option (t x) e
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

module Decode = struct
  let lexeme e =
    match Json.decode e with
    | `Lexeme e -> Ok e
    | `Error e -> Error (`Msg (Fmt.to_to_string Jsonm.pp_error e))
    | `End | `Await -> assert false

  let ( >>= ) l f = match l with Error _ as e -> e | Ok l -> f l

  let ( >|= ) l f = match l with Ok l -> Ok (f l) | Error _ as e -> e

  let error e got expected =
    let _, (l, c) = Jsonm.decoded_range e.Json.d in
    Error
      (`Msg
        (Fmt.strf
           "line %d, character %d:\nFound lexeme %a, but lexeme %s was expected"
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

  let unit e = expect_lexeme e `Os >>= fun () -> expect_lexeme e `Oe

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

  let unboxed_option o e = o e >|= fun v -> Some v

  let boxed_option o e =
    lexeme e >>= function
    | `Null -> Ok None
    | `Os ->
        expect_lexeme e (`Name "some") >>= fun () ->
        o e >>= fun v ->
        expect_lexeme e `Oe >|= fun () -> Some v
    | l -> error e l "`Option-contents"

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
    | Option x -> boxed_option (t x) d
    | Record r -> record r d
    | Variant v -> variant v d

  (* Some types need to be decoded differently when wrapped inside records,
     since e.g. `k: None` is omitted and `k: Some v` is unboxed into `k: v`. *)
  and inside_record_t : type a. a t -> a decode_json =
   fun ty d -> match ty with Option x -> unboxed_option (t x) d | _ -> t ty d

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
    let rec aux : type a b. (a, b) fields -> b -> (a, [ `Msg of string ]) result
        =
     fun f c ->
      match f with
      | F0 -> Ok c
      | F1 (h, f) -> (
          let v =
            try
              let s = List.assoc h.fname soup in
              let e = Json.decoder_of_lexemes s in
              inside_record_t h.ftype e
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

let encode = Encode.t

let decode = Decode.t

let decode_jsonm x d = Decode.(t x @@ { Json.d; lexemes = [] })

let pp ?minify t ppf x =
  let buf = Buffer.create 42 in
  let e = Jsonm.encoder ?minify (`Buffer buf) in
  encode t e x;
  ignore (Jsonm.encode e `End);
  Fmt.string ppf (Buffer.contents buf)

let to_string ?minify t x = Fmt.to_to_string (pp ?minify t) x

let of_string x s = Decode.(t x @@ Json.decoder (`String s))

let decode_lexemes x ls = Decode.(t x @@ Json.decoder_of_lexemes ls)
