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

type len = [ `Int | `Int8 | `Int16 | `Int32 | `Int64 | `Fixed of int ]

type 'a t

val unit : unit t

val bool : bool t

val char : char t

val int : int t

val int32 : int32 t

val int64 : int64 t

val float : float t

val string : string t

val bytes : bytes t

val list : ?len:len -> 'a t -> 'a list t

val array : ?len:len -> 'a t -> 'a array t

val option : 'a t -> 'a option t

val pair : 'a t -> 'b t -> ('a * 'b) t

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val result : 'a t -> 'b t -> ('a, 'b) result t

val string_of : len -> string t

val bytes_of : len -> bytes t

type ('a, 'b) field

type ('a, 'b, 'c) open_record

val field : string -> 'a t -> ('b -> 'a) -> ('b, 'a) field

val sealr : ('a, 'b, 'a) open_record -> 'a t

val ( |+ ) :
  ('a, 'b, 'c -> 'd) open_record -> ('a, 'c) field -> ('a, 'b, 'd) open_record

val record : string -> 'b -> ('a, 'b, 'b) open_record

type ('a, 'b) case

type 'a case_p

type ('a, 'b, 'c) open_variant

val case0 : string -> 'a -> ('a, 'a case_p) case

val case1 : string -> 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_p) case

val sealv : ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t

val variant : string -> 'b -> ('a, 'b, 'b) open_variant

val ( |~ ) :
  ('a, 'b, 'c -> 'd) open_variant -> ('a, 'c) case -> ('a, 'b, 'd) open_variant

val enum : string -> (string * 'a) list -> 'a t

val mu : ('a t -> 'a t) -> 'a t

val mu2 : ('a t -> 'b t -> 'a t * 'b t) -> 'a t * 'b t

(* generics *)

val equal : 'a t -> 'a -> 'a -> bool

val compare : 'a t -> 'a -> 'a -> int

val short_hash : 'a t -> ?seed:int -> 'a -> int

(* CLI *)

type 'a pp = 'a Fmt.t

type 'a to_string = 'a -> string

type 'a of_string = string -> ('a, [ `Msg of string ]) result

val pp : 'a t -> 'a Fmt.t

val of_string : 'a t -> 'a of_string

(* JSON (wire) *)

module Json : sig
  type decoder

  val decoder : ?encoding:[< Jsonm.encoding ] -> [< Jsonm.src ] -> decoder

  val decode :
    decoder ->
    [> `Await | `End | `Error of Jsonm.error | `Lexeme of Jsonm.lexeme ]

  val rewind : decoder -> Jsonm.lexeme -> unit
end

type 'a encode_json = Jsonm.encoder -> 'a -> unit

type 'a decode_json = Json.decoder -> ('a, [ `Msg of string ]) result

(* Raw (disk) *)

type 'a bin_seq = 'a -> (string -> unit) -> unit

type 'a encode_bin = ?headers:bool -> 'a bin_seq

type 'a decode_bin = ?headers:bool -> string -> int -> int * 'a

type 'a size_of = ?headers:bool -> 'a -> int option

val size_of : 'a t -> 'a size_of

(* like *)

val v :
  cli:'a pp * 'a of_string ->
  json:'a encode_json * 'a decode_json ->
  bin:'a encode_bin * 'a decode_bin * 'a size_of ->
  equal:('a -> 'a -> bool) ->
  compare:('a -> 'a -> int) ->
  short_hash:(?seed:int -> 'a -> int) ->
  pre_hash:'a bin_seq ->
  'a t

val like :
  ?cli:'a pp * 'a of_string ->
  ?json:'a encode_json * 'a decode_json ->
  ?bin:'a encode_bin * 'a decode_bin * 'a size_of ->
  ?equal:('a -> 'a -> bool) ->
  ?compare:('a -> 'a -> int) ->
  ?short_hash:('a -> int) ->
  ?pre_hash:'a bin_seq ->
  'a t ->
  'a t

val map :
  ?cli:'b pp * 'b of_string ->
  ?json:'b encode_json * 'b decode_json ->
  ?bin:'b encode_bin * 'b decode_bin * 'b size_of ->
  ?equal:('b -> 'b -> bool) ->
  ?compare:('b -> 'b -> int) ->
  ?short_hash:('b -> int) ->
  ?pre_hash:'b bin_seq ->
  'a t ->
  ('a -> 'b) ->
  ('b -> 'a) ->
  'b t

(* convenient functions. *)

val to_string : 'a t -> 'a -> string

val pp_json : ?minify:bool -> 'a t -> 'a Fmt.t

val pre_hash : 'a t -> 'a bin_seq

val encode_json : 'a t -> Jsonm.encoder -> 'a -> unit

val decode_json : 'a t -> Jsonm.decoder -> ('a, [ `Msg of string ]) result

val decode_json_lexemes :
  'a t -> Jsonm.lexeme list -> ('a, [ `Msg of string ]) result

val to_json_string : ?minify:bool -> 'a t -> 'a to_string

val of_json_string : 'a t -> 'a of_string

val encode_bin : 'a t -> 'a encode_bin

val to_bin_string : 'a t -> 'a to_string

val decode_bin : 'a t -> 'a decode_bin

val of_bin_string : 'a t -> 'a of_string

type 'a ty = 'a t

val pp_ty : 'a t Fmt.t

module type S = sig
  type t

  val t : t ty
end
