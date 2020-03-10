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

(** Yet-an-other type combinator library

    [Type] provides type combinators to define runtime representation for OCaml
    types and {{!generics} generic operations} to manipulate values with a
    runtime type representation.

    The type combinators supports all the usual {{!primitives} type primitives}
    but also compact definitions of {{!records} records} and {{!variants}
    variants}. It also allows the definition of run-time representations of
    {{!recursive} recursive types}. *)

(** {1 Type Combinators} *)

type 'a t
(** The type for runtime representation of values of type ['a]. *)

type len = [ `Int | `Int8 | `Int16 | `Int32 | `Int64 | `Fixed of int ]
(** The type of integer used to store buffers, list or array lengths. *)

exception Not_utf8
(** Raised when a field or case name is not valid UTF-8. *)

(** {1:primitives Primitives} *)

val unit : unit t
(** [unit] is a representation of the unit type. *)

val bool : bool t
(** [bool] is a representation of the boolean type. *)

val char : char t
(** [char] is a representation of the character type. *)

val int : int t
(** [int] is a representation of integers. Binary serialization uses a
    varying-width representation. *)

val int32 : int32 t
(** [int32] is a representation of the 32-bit integer type. *)

val int64 : int64 t
(** [int64] is a representation of the 64-bit integer type. *)

val float : float t
(** [float] is a representation of the [float] type. *)

val string : string t
(** [string] is a representation of the [string] type. *)

val bytes : bytes t
(** [bytes] is a representation of the [bytes] type. *)

val string_of : len -> string t
(** Like {!string} but with a given fixed size. *)

val bytes_of : len -> bytes t
(** Like {!bytes} but with a given fixed size. *)

val list : ?len:len -> 'a t -> 'a list t
(** [list t] is a representation of lists of values of type [t]. *)

val array : ?len:len -> 'a t -> 'a array t
(** [array t] is a representation of arrays of values of type [t]. *)

val option : 'a t -> 'a option t
(** [option t] is a representation of values of type [t option]. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair x y] is a representation of values of type [x * y]. *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple x y z] is a representation of values of type [x * y * z]. *)

val result : 'a t -> 'b t -> ('a, 'b) result t
(** [result a b] is a representation of values of type [(a, b) result]. *)

(** {1:records Records} *)

type ('a, 'b, 'c) open_record
(** The type for representing open records of type ['a] with a constructor of
    type ['b]. ['c] represents the remaining fields to be described using the
    {!(|+)} operator. An open record initially satisfies ['c = 'b] and can be
    {{!sealr} sealed} once ['c = 'a]. *)

val record : string -> 'b -> ('a, 'b, 'b) open_record
(** [record n f] is an incomplete representation of the record called [n] of
    type ['a] with constructor [f]. To complete the representation, add fields
    with {!(|+)} and then seal the record with {!sealr}. *)

type ('a, 'b) field
(** The type for fields holding values of type ['b] and belonging to a record of
    type ['a]. *)

val field : string -> 'a t -> ('b -> 'a) -> ('b, 'a) field
(** [field n t g] is the representation of the field [n] of type [t] with getter
    [g]. {b Raises.} [Not_utf8] if [n] is not valid UTF-8.

    The name [n] must not be used by any other [field] in the record.

    For instance:

    {[
      type manuscript = { title : string option }

      let manuscript = field "title" (option string) (fun t -> t.title)
    ]} *)

val ( |+ ) :
  ('a, 'b, 'c -> 'd) open_record -> ('a, 'c) field -> ('a, 'b, 'd) open_record
(** [r |+ f] is the open record [r] augmented with the field [f]. *)

val sealr : ('a, 'b, 'a) open_record -> 'a t
(** [sealr r] seals the open record [r]. {b Raises.} [Invalid_argument] if two
    or more fields share the same name. *)

(** Putting all together:

    {[
      type menu = { restaurant : string; items : (string * int32) list }

      let t =
        record "t" (fun restaurant items -> { restaurant; items })
        |+ field "restaurant" string (fun t -> t.restaurant)
        |+ field "items" (list (pair string int32)) (fun t -> t.items)
        |> sealr
    ]} *)

(** {1:variants Variants} *)

type ('a, 'b, 'c) open_variant
(** The type for representing open variants of type ['a] with pattern matching
    of type ['b]. ['c] represents the remaining constructors to be described
    using the {!(|~)} operator. An open variant initially satisfies [c' = 'b]
    and can be {{!sealv} sealed} once ['c = 'a]. *)

val variant : string -> 'b -> ('a, 'b, 'b) open_variant
(** [variant n p] is an incomplete representation of the variant type called [n]
    of type ['a] using [p] to deconstruct values. To complete the
    representation, add cases with {!(|~)} and then seal the variant with
    {!sealv}. *)

type ('a, 'b) case
(** The type for representing variant cases of type ['a] with patterns of type
    ['b]. *)

type 'a case_p
(** The type for representing patterns for a variant of type ['a]. *)

val case0 : string -> 'a -> ('a, 'a case_p) case
(** [case0 n v] is a representation of a variant constructor [v] with no
    arguments and name [n]. {b Raises.} [Not_utf8] if [n] is not valid UTF-8.

    The name [n] must not by used by any other [case0] in the record.

    For instance:

    {[
      type t = Foo

      let foo = case0 "Foo" Foo
    ]} *)

val case1 : string -> 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_p) case
(** [case1 n t c] is a representation of a variant constructor [c] with an
    argument of type [t] and name [n]. {b Raises.} [Not_utf8] if [n] is not
    valid UTF-8.

    The name [n] must not by used by any other [case1] in the record.

    For instance:

    {[
      type t = Foo of string

      let foo = case1 "Foo" string (fun s -> Foo s)
    ]} *)

val ( |~ ) :
  ('a, 'b, 'c -> 'd) open_variant -> ('a, 'c) case -> ('a, 'b, 'd) open_variant
(** [v |~ c] is the open variant [v] augmented with the case [c]. *)

val sealv : ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
(** [sealv v] seals the open variant [v]. {b Raises.} [Invalid_argument] if two
    or more cases of same arity share the same name. *)

(** Putting all together:

    {[
      type t = Foo | Bar of string

      let t =
        variant "t" (fun foo bar -> function Foo -> foo | Bar s -> bar s)
        |~ case0 "Foo" Foo
        |~ case1 "Bar" string (fun x -> Bar x)
        |> sealv
    ]} *)

val enum : string -> (string * 'a) list -> 'a t
(** [enum n cs] is a representation of the variant type called [n] with
    singleton cases [cs]. e.g.

    {[
      type t = Foo | Bar | Toto

      let t = enum "t" [ ("Foo", Foo); ("Bar", Bar); ("Toto", Toto) ]
    ]}

    {b Raises.} [Invalid_argument] if two or more cases share the same name. *)

(** {1:recursive Recursive definitions}

    [Type] allows a limited description of recursive records and variants.

    {b TODO}: describe the limitations, e.g. only regular recursion and no use
    of the generics inside the [mu*] functions and the usual caveats with
    recursive values (such as infinite loops on most of the generics which don't
    check sharing). *)

val mu : ('a t -> 'a t) -> 'a t
(** [mu f] is the representation [r] such that [r = mu r].

    For instance:

    {[
      type x = { x : x option }

      let x =
        mu (fun x ->
            record "x" (fun x -> { x }) |+ field "x" x (fun x -> x.x) |> sealr)
    ]} *)

val mu2 : ('a t -> 'b t -> 'a t * 'b t) -> 'a t * 'b t
(** [mu2 f] is the representations [r] and [s] such that [r, s = mu2 r s].

    For instance:

    {[
      type r = { foo : int; bar : string list; z : z option }

      and z = { x : int; r : r list }

      (* Build the representation of [r] knowing [z]'s. *)
      let mkr z =
        record "r" (fun foo bar z -> { foo; bar; z })
        |+ field "foo" int (fun t -> t.foo)
        |+ field "bar" (list string) (fun t -> t.bar)
        |+ field "z" (option z) (fun t -> t.z)
        |> sealr

      (* And the representation of [z] knowing [r]'s. *)
      let mkz r =
        record "z" (fun x r -> { x; r })
        |+ field "x" int (fun t -> t.x)
        |+ field "r" (list r) (fun t -> t.r)
        |> sealr

      (* Tie the loop. *)
      let r, z = mu2 (fun r z -> (mkr z, mkz y))
    ]} *)

(** {1:generics Generic Operations}

    Given a value ['a t], it is possible to define generic operations on value
    of type ['a] such as pretty-printing, parsing and unparsing. *)

val equal : 'a t -> 'a -> 'a -> bool
(** [equal t] is the equality function between values of type [t]. *)

val compare : 'a t -> 'a -> 'a -> int
(** [compare t] compares values of type [t]. *)

val short_hash : 'a t -> ?seed:int -> 'a -> int
(** [hash t x] is a short hash of [x] of type [t]. *)

type 'a pp = 'a Fmt.t
(** The type for pretty-printers. *)

type 'a of_string = string -> ('a, [ `Msg of string ]) result
(** The type for parsers. *)

val pp : 'a t -> 'a pp
(** [pp t] is the pretty-printer for values of type [t]. *)

val pp_ty : 'a t pp
(** The pretty printer for generics of type {!t}. *)

val to_string : 'a t -> 'a -> string
(** [to_string t] is [Fmt.to_to_string (pp t)]. *)

val of_string : 'a t -> 'a of_string
(** [of_string t] parses values of type [t]. *)

(** {2 JSON converters} *)

module Json : sig
  (** Overlay on top of Jsonm to work with rewindable streams. *)

  type decoder
  (** The type for JSON decoder. *)

  val decoder : ?encoding:[< Jsonm.encoding ] -> [< Jsonm.src ] -> decoder
  (** Same as {!Jsonm.decoder}. *)

  val decode :
    decoder ->
    [> `Await | `End | `Error of Jsonm.error | `Lexeme of Jsonm.lexeme ]
  (** Same as {!Jsonm.decode}. *)

  val rewind : decoder -> Jsonm.lexeme -> unit
  (** [rewind d l] rewinds [l] on top of the current state of [d]. This allows
      to put back lexemes already seen. *)
end

type 'a encode_json = Jsonm.encoder -> 'a -> unit
(** The type for JSON encoders. *)

type 'a decode_json = Json.decoder -> ('a, [ `Msg of string ]) result
(** The type for JSON decoders. *)

val pp_json : ?minify:bool -> 'a t -> 'a Fmt.t
(** Similar to {!dump} but pretty-prints the JSON representation instead of the
    OCaml one. See {!encode_json} for details about the encoding.

    For instance:

    {[
      type t = { foo : int option; bar : string list }

      let t =
        record "r" (fun foo bar -> { foo; bar })
        |+ field "foo" (option int) (fun t -> t.foo)
        |+ field "bar" (list string) (fun t -> t.bar)
        |> sealr

      let s = Fmt.strf "%a\n" (pp t) { foo = None; bar = [ "foo" ] }

      (* s is "{ foo = None; bar = [\"foo\"]; }" *)

      let j = Fmt.strf "%a\n" (pp_json t) { foo = None; bar = [ "foo" ] }

      (* j is "{ \"bar\":[\"foo\"] }" *)
    ]}

    {b NOTE:} this will automatically convert JSON fragments to valid JSON
    objects by adding an enclosing array if necessary. *)

val encode_json : 'a t -> Jsonm.encoder -> 'a -> unit
(** [encode_json t e] encodes [t] into the
    {{:http://erratique.ch/software/jsonm} jsonm} encoder [e]. The encoding is a
    relatively straightforward translation of the OCaml structure into JSON. The
    main highlights are:

    - The unit value [()] is translated into the empty object [{}].
    - OCaml ints are translated into JSON floats.
    - OCaml strings are translated into JSON strings. You must then ensure that
      the OCaml strings contains only valid UTF-8 characters.
    - OCaml options are translated differently depending on context: record
      fields with a value of [None] are removed from the JSON object; record
      fields with a value of [Some x] are automatically unboxed into x; and
      outside of records, [None] is translated into [null] and [Some x] into
      [{"some": x'}] with [x'] the JSON encoding of [x].
    - Variant cases built using {!case0} are represented as strings.
    - Variant cases built using {!case1} are represented as a record with one
      field; the field name is the name of the variant.

    {b NOTE:} this can be used to encode JSON fragments. It's the responsibility
    of the caller to ensure that the encoded JSON fragment fits properly into a
    well-formed JSON object. *)

val decode_json : 'a t -> Jsonm.decoder -> ('a, [ `Msg of string ]) result
(** [decode_json t e] decodes values of type [t] from the
    {{:http://erratique.ch/software/jsonm} jsonm} decoder [e]. *)

val decode_json_lexemes :
  'a t -> Jsonm.lexeme list -> ('a, [ `Msg of string ]) result
(** [decode_json_lexemes] is similar to {!decode_json} but uses an already
    decoded list of JSON lexemes instead of a decoder. *)

val to_json_string : ?minify:bool -> 'a t -> 'a -> string
(** [to_json_string] is {!encode_json} with a string encoder. *)

val of_json_string : 'a t -> string -> ('a, [ `Msg of string ]) result
(** [of_json_string] is {!decode_json} with a string decoder .*)

(** {2 Binary Converters} *)

type 'a bin_seq = 'a -> (string -> unit) -> unit

type 'a encode_bin = ?headers:bool -> 'a bin_seq
(** The type for binary encoders. If [headers] is not set, do not output extra
    length headers for buffers. *)

type 'a decode_bin = ?headers:bool -> string -> int -> int * 'a
(** The type for binary decoders. IF [headers] is not set, do not read extra
    length header for buffers and consider the whole buffer instead. *)

type 'a size_of = ?headers:bool -> 'a -> int option
(** The type for size function related to binary encoder/decoders. *)

val pre_hash : 'a t -> 'a bin_seq
(** [pre_hash t x] is the string representation of [x], of type [t], which will
    be used to compute the digest of the value. By default it's
    [to_bin_string t x] but it can be overriden by {!v}, {!like} and {!map}
    operators. *)

val encode_bin : 'a t -> 'a encode_bin
(** [encode_bin t] is the binary encoder for values of type [t]. *)

val decode_bin : 'a t -> 'a decode_bin
(** [decode_bin t] is the binary decoder for values of type [t]. *)

val to_bin_string : 'a t -> 'a -> string
(** [to_bin_string t x] use {!encode_bin} to convert [x], of type [t], to a
    string.

    {b NOTE:} When [t] is {!Type.string} or {!Type.bytes}, the original buffer
    [x] is not prefixed by its size as {!encode_bin} would do. If [t] is
    {!Type.string}, the result is [x] (without copy). *)

val of_bin_string : 'a t -> string -> ('a, [ `Msg of string ]) result
(** [of_bin_string t s] is [v] such that [s = to_bin_string t v].

    {b NOTE:} When [t] is {!Type.string}, the result is [s] (without copy). *)

val size_of : 'a t -> 'a size_of
(** [size_of t x] is either the size of [encode_bin t x] or the binary encoding
    of [x], if the backend is not able to pre-compute serialisation lengths. *)

(** {1 Customs converters} *)

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
  ?cli:'a pp * 'a of_string ->
  ?json:'a encode_json * 'a decode_json ->
  ?bin:'a encode_bin * 'a decode_bin * 'a size_of ->
  ?equal:('a -> 'a -> bool) ->
  ?compare:('a -> 'a -> int) ->
  ?short_hash:('a -> int) ->
  ?pre_hash:'a bin_seq ->
  'b t ->
  ('b -> 'a) ->
  ('a -> 'b) ->
  'a t

type 'a ty = 'a t

module type S = sig
  type t

  val t : t ty
end
