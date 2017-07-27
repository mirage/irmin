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

open Result

type 'a t
val unit: unit t
val bool: bool t
val char: char t
val int32: int32 t
val int64: int64 t
val float: float t
val string: string t
val cstruct: Cstruct.t t
val list: 'a t -> 'a list t
val array: 'a t -> 'a array t
val option: 'a t -> 'a option t
val pair: 'a t -> 'b t -> ('a * 'b) t
val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val result: 'a t -> 'b t -> ('a, 'b) result t

type ('a, 'b) field
type ('a, 'b, 'c) open_record

val field: string -> 'a t -> ('b -> 'a) -> ('b, 'a) field
val sealr: ('a, 'b, 'a) open_record -> 'a t

val (|+):
  ('a, 'b, 'c -> 'd) open_record -> ('a, 'c) field -> ('a, 'b, 'd) open_record

val record: string -> 'b -> ('a, 'b, 'b) open_record


type ('a, 'b) case
type 'a case_p
type ('a, 'b, 'c) open_variant

val case0: string -> 'a -> ('a, 'a case_p) case
val case1: string -> 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_p) case
val sealv: ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
val variant: string -> 'b -> ('a, 'b, 'b) open_variant

val (|~):
  ('a, 'b, 'c -> 'd) open_variant -> ('a, 'c) case -> ('a, 'b, 'd) open_variant

val enum: string -> (string * 'a) list -> 'a t

val mu: ('a t -> 'a t) -> 'a t
val mu2: ('a t -> 'b t -> 'a t * 'b t) -> 'a t * 'b t
val like: 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t

val dump: 'a t -> 'a Fmt.t
val equal: 'a t -> 'a -> 'a -> bool
val compare: 'a t -> 'a -> 'a -> int

val pp_json: ?minify:bool -> 'a t -> 'a Fmt.t
val encode_json: 'a t -> Jsonm.encoder -> 'a -> unit
val decode_json: 'a t -> Jsonm.decoder -> ('a, [`Msg of string]) result
val decode_json_lexemes:
  'a t -> Jsonm.lexeme list -> ('a, [`Msg of string]) result

val encode_cstruct: 'a t -> 'a -> Cstruct.t
val decode_cstruct: 'a t -> Cstruct.t -> ('a, [`Msg of string]) result
