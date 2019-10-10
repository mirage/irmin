(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Irmin public API.

    [Irmin] is a library to design and use persistent stores with
    built-in snapshot, branching and reverting mechanisms. Irmin uses
    concepts similar to {{:http://git-scm.com/}Git} but it exposes
    them as a high level library instead of a complex command-line
    frontend. It features a {e bidirectional} Git backend, where an
    application can read and persist its state using the Git format,
    fully-compatible with the usual Git tools and workflows.

    Irmin is designed to use a large variety of backends. It is
    written in pure OCaml and does not depend on external C stubs; it
    is thus very portable and aims to run everywhere, from Linux to
    browser and MirageOS unikernels.

    Consult the {!basics} and {!examples} of use for a quick
    start. See also the {{!Irmin_unix}documentation} for the unix
    backends.

    {e Release %%VERSION%% - %%HOMEPAGE%% }
*)

val version : string
(** The version of the library. *)

(** {1 Preliminaries} *)

(** Dynamic types for Irmin values. *)
module Type : sig
  (** Yet-an-other type combinator library

      [Type] provides type combinators to define runtime
      representation for OCaml types and {{!generics}generic
      operations} to manipulate values with a runtime type
      representation.

      The type combinators supports all the usual {{!primitives}type
      primitives} but also compact definitions of {{!records}records}
      and {{!variants}variants}. It also allows the definition of
      run-time representations of {{!recursive}recursive types}. *)

  (** {1 Type Combinators} *)

  type 'a t
  (** The type for runtime representation of values of type ['a]. *)

  type len = [ `Int | `Int8 | `Int16 | `Int32 | `Int64 | `Fixed of int ]
  (** The type of integer used to store buffers, list or array
     lengths. *)

  (** {1:primitives Primitives} *)

  val unit : unit t
  (** [unit] is a representation of the unit type. *)

  val bool : bool t
  (** [bool] is a representation of the boolean type. *)

  val char : char t
  (** [char] is a representation of the character type. *)

  val int : int t
  (** [int] is a representation of integers. Binary serialization uses
      a varying-width representation. *)

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
  (** [triple x y z] is a representation of values of type
      [x * y * z]. *)

  val result : 'a t -> 'b t -> ('a, 'b) result t
  (** [result a b] is a representation of values of type
      [(a, b) result]. *)

  (** {1:records Records} *)

  type ('a, 'b, 'c) open_record
  (** The type for representing open records of type ['a] with a constructor
      of type ['b]. ['c] represents the remaining fields to be described using
      the {!(|+)} operator. An open record initially satisfies ['c = 'b] and
      can be {{!sealr}sealed} once ['c = 'a]. *)

  val record : string -> 'b -> ('a, 'b, 'b) open_record
  (** [record n f] is an incomplete representation of the record called [n] of
      type ['a] with constructor [f]. To complete the representation, add fields
      with {!(|+)} and then seal the record with {!sealr}. *)

  type ('a, 'b) field
  (** The type for fields holding values of type ['b] and belonging to a
      record of type ['a]. *)

  val field : string -> 'a t -> ('b -> 'a) -> ('b, 'a) field
  (** [field n t g] is the representation of the field [n] of type [t]
      with getter [g].

      For instance:

      {[
        type manuscript = { title : string option }

        let manuscript = field "title" (option string) (fun t -> t.title)]}
  *)

  val ( |+ ) :
    ('a, 'b, 'c -> 'd) open_record ->
    ('a, 'c) field ->
    ('a, 'b, 'd) open_record
  (** [r |+ f] is the open record [r] augmented with the field [f]. *)

  val sealr : ('a, 'b, 'a) open_record -> 'a t
  (** [sealr r] seals the open record [r]. *)

  (** Putting all together:

      {[
        type menu = { restaurant: string; items: (string * int32) list; }

        let t =
          record "t" (fun restaurant items -> {restaurant; items})
          |+ field "restaurant" string (fun t -> t.restaurant)
          |+ field "items" (list (pair string int32)) (fun t -> t.items)
          |> sealr]}
  *)

  (** {1:variants Variants} *)

  type ('a, 'b, 'c) open_variant
  (** The type for representing open variants of type ['a] with pattern
      matching of type ['b]. ['c] represents the remaining constructors to
      be described using the {!(|~)} operator. An open variant initially
      satisfies [c' = 'b] and can be {{!sealv}sealed} once ['c = 'a]. *)

  val variant : string -> 'b -> ('a, 'b, 'b) open_variant
  (** [variant n p] is an incomplete representation of the variant type
      called [n] of type ['a] using [p] to deconstruct values. To complete
      the representation, add cases with {!(|~)} and then seal the variant
      with {!sealv}. *)

  type ('a, 'b) case
  (** The type for representing variant cases of type ['a] with
      patterns of type ['b]. *)

  type 'a case_p
  (** The type for representing patterns for a variant of type ['a]. *)

  val case0 : string -> 'a -> ('a, 'a case_p) case
  (** [case0 n v] is a representation of a variant constructor [v] with no
      arguments and name [n]. e.g.

      {[
        type t = Foo

        let foo = case0 "Foo" Foo]}
  *)

  val case1 : string -> 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_p) case
  (** [case1 n t c] is a representation of a variant constructor [c] with an
      argument of type [t] and name [n]. e.g.

      {[
        type t = Foo of string

        let foo = case1 "Foo" string (fun s -> Foo s)]}
  *)

  val ( |~ ) :
    ('a, 'b, 'c -> 'd) open_variant ->
    ('a, 'c) case ->
    ('a, 'b, 'd) open_variant
  (** [v |~ c] is the open variant [v] augmented with the case [c]. *)

  val sealv : ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
  (** [sealv v] seals the open variant [v]. *)

  (** Putting all together:
      {[
        type t = Foo | Bar of string

        let t =
          variant "t" (fun foo bar -> function
              | Foo   -> foo
              | Bar s -> bar s)
          |~ case0 "Foo" Foo
          |~ case1 "Bar" string (fun x -> Bar x)
          |> sealv]}
  *)

  val enum : string -> (string * 'a) list -> 'a t
  (** [enum n cs] is a representation of the variant type called [n]
      with singleton cases [cs]. e.g.

      {[
        type t = Foo | Bar | Toto

        let t = enum "t" ["Foo", Foo; "Bar", Bar; "Toto", Toto]]}
  *)

  (** {1:recursive Recursive definitions}

      [Type] allows a limited description of recursive records and
      variants.

      {b TODO}: describe the limitations, e.g. only regular recursion
      and no use of the generics inside the [mu*] functions and the
      usual caveats with recursive values (such as infinite loops on
      most of the generics which don't check sharing).

  *)

  val mu : ('a t -> 'a t) -> 'a t
  (** [mu f] is the representation [r] such that [r = mu r].

      For instance:

      {[
        type x = { x: x option }

        let x = mu (fun x ->
            record "x" (fun x -> { x })
            |+ field "x" x (fun x -> x.x)
            |> sealr)]}
  *)

  val mu2 : ('a t -> 'b t -> 'a t * 'b t) -> 'a t * 'b t
  (** [mu2 f] is the representations [r] and [s] such that
      [r, s = mu2 r s].

      For instance:

      {[
        type r = { foo: int; bar: string list; z: z option }
        and z = { x: int; r: r list }

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
        let r, z = mu2 (fun r z -> mkr z, mkz y)]}
  *)

  (** {1:generics Generic Operations}

      Given a value ['a t], it is possible to define generic operations
      on value of type ['a] such as pretty-printing, parsing and
      unparsing.
  *)

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
    (** [rewind d l] rewinds [l] on top of the current state of
        [d]. This allows to put back lexemes already seen. *)
  end

  type 'a encode_json = Jsonm.encoder -> 'a -> unit
  (** The type for JSON encoders. *)

  type 'a decode_json = Json.decoder -> ('a, [ `Msg of string ]) result
  (** The type for JSON decoders. *)

  val pp_json : ?minify:bool -> 'a t -> 'a Fmt.t
  (** Similar to {!dump} but pretty-prints the JSON representation instead
      of the OCaml one. See {!encode_json} for details about the encoding.

      For instance:

      {[
        type t = { foo: int option; bar: string list };;

        let t =
          record "r" (fun foo bar -> { foo; bar })
          |+ field "foo" (option int) (fun t -> t.foo)
          |+ field "bar" (list string) (fun t -> t.bar)
          |> sealr

        let s = Fmt.strf "%a\n" (pp t) { foo = None; bar = ["foo"] }
        (* s is "{ foo = None; bar = [\"foo\"]; }" *)

        let j = Fmt.strf "%a\n" (pp_json t) { foo = None; bar = ["foo"] }
        (* j is "{ \"bar\":[\"foo\"] }" *)]}

      {b NOTE:} this will automatically convert JSON fragments to valid
      JSON objects by adding an enclosing array if necessary. *)

  val encode_json : 'a t -> Jsonm.encoder -> 'a -> unit
  (** [encode_json t e] encodes [t] into the
      {{:http://erratique.ch/software/jsonm}jsonm} encoder [e]. The
      encoding is a relatively straightforward translation of the OCaml
      structure into JSON. The main highlights are:

      {ul
      {- OCaml [ints] are translated into JSON floats.}
      {- OCaml strings are translated into JSON strings. You must then
         ensure that the OCaml strings contains only valid UTF-8
         characters.}
      {- OCaml record fields of type ['a option] are automatically
         unboxed in their JSON representation. If the value if [None],
         the field is removed from the JSON object.}
      {- variant cases built using {!case0} are represented as strings.}
      {- variant cases built using {!case1} are represented as a record
         with one field; the field name is the name of the variant.}
      }

      {b NOTE:} this can be used to encode JSON fragments. It's the
      responsibility of the caller to ensure that the encoded JSON
      fragment fits properly into a well-formed JSON object. *)

  val decode_json : 'a t -> Jsonm.decoder -> ('a, [ `Msg of string ]) result
  (** [decode_json t e] decodes values of type [t] from the
      {{:http://erratique.ch/software/jsonm}jsonm} decoder [e]. *)

  val decode_json_lexemes :
    'a t -> Jsonm.lexeme list -> ('a, [ `Msg of string ]) result
  (** [decode_json_lexemes] is similar to {!decode_json} but uses an
      already decoded list of JSON lexemes instead of a decoder. *)

  val to_json_string : ?minify:bool -> 'a t -> 'a -> string
  (** [to_json_string] is {!encode_json} with a string encoder. *)

  val of_json_string : 'a t -> string -> ('a, [ `Msg of string ]) result
  (** [of_json_string] is {!decode_json} with a string decoder .*)

  (** {2 Binary Converters} *)

  type 'a bin_seq = 'a -> (string -> unit) -> unit

  type 'a encode_bin = ?headers:bool -> 'a bin_seq
  (** The type for binary encoders. If [headers] is not set, do not
     output extra length headers for buffers. *)

  type 'a decode_bin = ?headers:bool -> string -> int -> int * 'a
  (** The type for binary decoders. IF [headers] is not set, do not
     read extra length header for buffers and consider the whole
     buffer instead. *)

  type 'a size_of = ?headers:bool -> 'a -> int option
  (** The type for size function related to binary encoder/decoders. *)

  val pre_hash : 'a t -> 'a bin_seq
  (** [pre_hash t x] is the string representation of [x], of type
      [t], which will be used to compute the digest of the value. By
      default it's [to_bin_string t x] but it can be overriden by {!v},
      {!like} and {!map} operators. *)

  val encode_bin : 'a t -> 'a encode_bin
  (** [encode_bin t] is the binary encoder for values of type [t]. *)

  val decode_bin : 'a t -> 'a decode_bin
  (** [decode_bin t] is the binary decoder for values of type [t]. *)

  val to_bin_string : 'a t -> 'a -> string
  (** [to_bin_string t x] use {!encode_bin} to convert [x], of type
     [t], to a string.

      {b NOTE:} When [t] is {!Type.string} or {!Type.bytes}, the
     original buffer [x] is not prefixed by its size as {!encode_bin}
     would do. If [t] is {!Type.string}, the result is [x] (without
     copy). *)

  val of_bin_string : 'a t -> string -> ('a, [ `Msg of string ]) result
  (** [of_bin_string t s] is [v] such that [s = to_bin_string t v].

      {b NOTE:} When [t] is {!Type.string}, the result is [s] (without
     copy). *)

  val size_of : 'a t -> 'a size_of
  (** [size_of t x] is either the size of [encode_bin t x] or the
     binary encoding of [x], if the backend is not able to pre-compute
     serialisation lengths. *)

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
end

(** Commit info are used to keep track of the origin of write
    operations in the stores. [Info] models the metadata associated
    with commit objects in Git. *)
module Info : sig
  (** {1 Commit Info} *)

  type t
  (** The type for commit info. *)

  val v : date:int64 -> author:string -> string -> t
  (** Create a new commit info. *)

  val date : t -> int64
  (** [date t] is [t]'s commit date.

      The date provided by the user when calling the {{!Info.v}create}
      function. Rounding [Unix.gettimeofday ()] (when available) is a
      good value for such date. On more esoteric platforms, any
      monotonic counter is a fine value as well. On the Git backend,
      the date is translated into the commit {e Date} field and is
      expected to be the number of POSIX seconds (thus not counting
      leap seconds) since the Epoch. *)

  val author : t -> string
  (** [author t] is [t]'s commit author.

      The author identifies the entity (human, unikernel, process,
      thread, etc) performing an operation. For the Git backend, this
      will be directly translated into the {e Author} field. *)

  val message : t -> string
  (** [message t] is [t]'s commit message. *)

  val empty : t
  (** The empty commit info. *)

  (** {1 Info Functions} *)

  type f = unit -> t
  (** Alias for functions which can build commit info. *)

  val none : f
  (** The empty info function. [none ()] is [empty] *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)
end

(** [Merge] provides functions to build custom 3-way merge operators
    for various user-defined contents. *)
module Merge : sig
  type conflict = [ `Conflict of string ]
  (** The type for merge errors. *)

  val ok : 'a -> ('a, conflict) result Lwt.t
  (** Return [Ok x]. *)

  val conflict : ('a, unit, string, ('b, conflict) result Lwt.t) format4 -> 'a
  (** Return [Error (Conflict str)]. *)

  val bind :
    ('a, 'b) result Lwt.t ->
    ('a -> ('c, 'b) result Lwt.t) ->
    ('c, 'b) result Lwt.t
  (** [bind r f] is the merge result which behaves as of the
      application of the function [f] to the return value of [r]. If
      [r] fails, [bind r f] also fails, with the same conflict. *)

  val map : ('a -> 'c) -> ('a, 'b) result Lwt.t -> ('c, 'b) result Lwt.t
  (** [map f m] maps the result of a merge. This is the same as
      [bind m (fun x -> ok (f x))]. *)

  (** {1 Merge Combinators} *)

  type 'a promise = unit -> ('a option, conflict) result Lwt.t
  (** An ['a] promise is a function which, when called, will
      eventually return a value type of ['a]. A promise is an
      optional, lazy and non-blocking value. *)

  val promise : 'a -> 'a promise
  (** [promise a] is the promise containing [a]. *)

  val map_promise : ('a -> 'b) -> 'a promise -> 'b promise
  (** [map_promise f a] is the promise containing [f] applied to what
      is promised by [a]. *)

  val bind_promise : 'a promise -> ('a -> 'b promise) -> 'b promise
  (** [bind_promise a f] is the promise returned by [f] applied to
      what is promised by [a]. *)

  type 'a f = old:'a promise -> 'a -> 'a -> ('a, conflict) result Lwt.t
  (** Signature of a merge function. [old] is the value of the
      least-common ancestor.

      {v
              /----> t1 ----\
      ----> old              |--> result
              \----> t2 ----/
      v}
  *)

  type 'a t
  (** The type for merge combinators. *)

  val v : 'a Type.t -> 'a f -> 'a t
  (** [v dt f] create a merge combinator. *)

  val f : 'a t -> 'a f
  (** [f m] is [m]'s merge function. *)

  val seq : 'a t list -> 'a t
  (** Call the merge functions in sequence. Stop as soon as one is {e
      not} returning a conflict. *)

  val like : 'a Type.t -> 'b t -> ('a -> 'b) -> ('b -> 'a) -> 'a t
  (** Use the merge function defined in another domain. If the
      converting functions raise any exception the merge is a
      conflict. *)

  val like_lwt :
    'a Type.t -> 'b t -> ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> 'a t
  (** Same as {{!Merge.biject}biject} but with blocking domain
      converting functions. *)

  (** {1 Basic Merges} *)

  val default : 'a Type.t -> 'a t
  (** [default t] is the default merge function for values of type
      [t]. This is a simple merge function which supports changes in
      one branch at a time:

      {ul
        {- if [t1=old] then the result of the merge is [OK t2];}
        {- if [t2=old] then return [OK t1];}
        {- otherwise the result is [Conflict].}
      }
  *)

  val idempotent : 'a Type.t -> 'a t
  (** [idempotent t] is the default merge function for values of type
      [t] using idempotent operations. It follows the same rules as
      the {!default} merge function but also adds:

      {ul
        {- if [t1=t2] then the result of the merge is [OK t1].}
      }
  *)

  val unit : unit t
  (** [unit] is the default merge function for unit values. *)

  val bool : bool t
  (** [bool] is the default merge function for booleans. *)

  val char : char t
  (** [char] is the default merge function for characters. *)

  val int32 : int32 t
  (** [int32] is the default merge function for 32-bits integers. *)

  val int64 : int64 t
  (** [int64] the default merge function for 64-bit integers. *)

  val float : float t
  (** [float] is the default merge function for floating point
      numbers. *)

  val string : string t
  (** The default string merge function. Do not do anything clever, just
      compare the strings using the [default] merge function. *)

  val option : 'a t -> 'a option t
  (** Lift a merge function to optional values of the same type. If all
      the provided values are inhabited, then call the provided merge
      function, otherwise use the same behavior as {!default}. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** Lift merge functions to pairs of elements. *)

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** Lift merge functions to triples of elements. *)

  (** {1 Counters and Multisets} *)

  type counter = int64
  (** The type for counter values. It is expected that the only valid
      operations on counters are {e increment} and {e decrement}. The
      following merge functions ensure that the counter semantics are
      preserved: {e i.e.} it ensures that the number of increments and
      decrements is preserved. *)

  val counter : counter t
  (** The merge function for mergeable counters. *)

  (** Multi-sets. *)
  module MultiSet (K : sig
    include Set.OrderedType

    val t : t Type.t
  end) : sig
    val merge : counter Map.Make(K).t t
  end

  (** {1 Maps and Association Lists} *)

  (** We consider the only valid operations for maps and
      association lists to be:

      {ul
      {- Adding a new bindings to the map.}
      {- Removing a binding from the map.}
      {- Replacing an existing binding with a different value.}
      {- {e Trying to add an already existing binding is a no-op}.}
      }

      We thus assume that no operation on maps is modifying the {e
      key} names. So the following merge functions ensures that {e
      (i)} new bindings are preserved {e (ii)} removed bindings stay
      removed and {e (iii)} modified bindings are merged using the
      merge function of values.

      {b Note:} We only consider sets of bindings, instead of
      multisets. Application developers should take care of concurrent
      addition and removal of similar bindings themselves, by using the
      appropriate {{!Merge.MSet}multi-sets}. *)

  (** Lift merge functions to sets. *)
  module Set (E : sig
    include Set.OrderedType

    val t : t Type.t
  end) : sig
    val merge : Set.Make(E).t t
  end

  val alist : 'a Type.t -> 'b Type.t -> ('a -> 'b option t) -> ('a * 'b) list t
  (** Lift the merge functions to association lists. *)

  (** Lift the merge functions to maps. *)

  module Map (K : sig
    include Map.OrderedType

    val t : t Type.t
  end) : sig
    val merge : 'a Type.t -> (K.t -> 'a option t) -> 'a Map.Make(K).t t
  end

  (** Infix operators for manipulating merge results and {!promise}s.

      [open Irmin.Merge.Infix] at the top of your file to use them. *)
  module Infix : sig
    (** {1 Merge Result Combinators} *)

    val ( >>=* ) :
      ('a, conflict) result Lwt.t ->
      ('a -> ('b, conflict) result Lwt.t) ->
      ('b, conflict) result Lwt.t
    (** [>>=*] is {!bind}. *)

    val ( >|=* ) :
      ('a, conflict) result Lwt.t -> ('a -> 'b) -> ('b, conflict) result Lwt.t
    (** [>|=*] is {!map}. *)

    (** {1 Promise Combinators}

        This is useful to manipulate lca results. *)

    val ( >>=? ) : 'a promise -> ('a -> 'b promise) -> 'b promise
    (** [>>=?] is {!bind_promise}. *)

    val ( >|=? ) : 'a promise -> ('a -> 'b) -> 'b promise
    (** [>|=?] is {!map_promise}. *)
  end
  (** {1 Value Types} *)

  val conflict_t : conflict Type.t
  (** [conflict_t] is the value type for {!conflict}. *)

  val result_t : 'a Type.t -> ('a, conflict) result Type.t
  (** [result_t] is the value type for merge results. *)
end

(** Differences between values. *)
module Diff : sig
  type 'a t = [ `Updated of 'a * 'a | `Removed of 'a | `Added of 'a ]
  (** The type for representing differences betwen values. *)

  (** {1 Value Types} *)

  val t : 'a Type.t -> 'a t Type.t
  (** [t typ] is the value type for differences between values of type [typ]. *)
end

type 'a diff = 'a Diff.t
(** The type for representing differences betwen values. *)

(** {1 Low-level Stores} *)

(** An Irmin store is automatically built from a number of lower-level
    stores, each implementing fewer operations, such as
    {{!CONTENT_ADDRESSABLE_STORE}content-addressable}
    and {{!ATOMIC_WRITE_STORE}atomic-write} stores. These low-level stores
    are provided
    by various backends. *)

(** Content-addressable backend store. *)
module type CONTENT_ADDRESSABLE_STORE = sig
  (** {1 Content-addressable stores}

      Content-addressable stores are store where it is possible to read
      and add new values. Keys are derived from the values raw contents
      and hence are deterministic. *)

  type 'a t
  (** The type for content-addressable backend stores. The ['a]
      phantom type carries information about the store mutability. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for raw values. *)

  val mem : [> `Read ] t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : [> `Read ] t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and
      [None] is [k] is not present in [t]. *)

  val add : [> `Write ] t -> value -> key Lwt.t
  (** Write the contents of a value to the store. It's the
      responsibility of the content-addressable store to generate a
      consistent key. *)

  val unsafe_add : [> `Write ] t -> key -> value -> unit Lwt.t
  (** Same as {!add} but allows to specify the key directly. The
      backend might choose to discared that key and/or can be corrupt
      if the key scheme is not consistent. *)
end

(** Append-only backend store. *)
module type APPEND_ONLY_STORE = sig
  (** {1 Append-only stores}

      Append-onlye stores are store where it is possible to read
      and add new values. *)

  type 'a t
  (** The type for append-only backend stores. The ['a]
     phantom type carries information about the store mutability. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for raw values. *)

  val mem : [> `Read ] t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : [> `Read ] t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and
      [None] is [k] is not present in [t]. *)

  val add : [> `Write ] t -> key -> value -> unit Lwt.t
  (** Write the contents of a value to the store. *)
end

(** Atomic-write stores. *)
module type ATOMIC_WRITE_STORE = sig
  (** {1 Atomic write stores}

      Atomic-write stores are stores where it is possible to read,
      update and remove elements, with atomically guarantees. *)

  type t
  (** The type for atomic-write backend stores.  *)

  type key
  (** The type for keys. *)

  type value
  (** The type for raw values. *)

  val mem : t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and
      [None] is [k] is not present in [t]. *)

  val set : t -> key -> value -> unit Lwt.t
  (** [set t k v] replaces the contents of [k] by [v] in [t]. If [k]
      is not already defined in [t], create a fresh binding.  Raise
      [Invalid_argument] if [k] is the {{!Path.empty}empty path}. *)

  val test_and_set :
    t -> key -> test:value option -> set:value option -> bool Lwt.t
  (** [test_and_set t key ~test ~set] sets [key] to [set] only if
      the current value of [key] is [test] and in that case returns
      [true]. If the current value of [key] is different, it returns
      [false]. [None] means that the value does not have to exist or
      is removed.

      {b Note:} The operation is guaranteed to be atomic. *)

  val remove : t -> key -> unit Lwt.t
  (** [remove t k] remove the key [k] in [t]. *)

  val list : t -> key list Lwt.t
  (** [list t] it the list of keys in [t]. *)

  type watch
  (** The type of watch handlers. *)

  val watch :
    t ->
    ?init:(key * value) list ->
    (key -> value diff -> unit Lwt.t) ->
    watch Lwt.t
  (** [watch t ?init f] adds [f] to the list of [t]'s watch handlers
      and returns the watch handler to be used with {!unwatch}. [init]
      is the optional initial values. It is more efficient to use
      {!watch_key} to watch only a single given key.*)

  val watch_key :
    t -> key -> ?init:value -> (value diff -> unit Lwt.t) -> watch Lwt.t
  (** [watch_key t k ?init f] adds [f] to the list of [t]'s watch
      handlers for the key [k] and returns the watch handler to be
      used with {!unwatch}. [init] is the optional initial value of
      the key. *)

  val unwatch : t -> watch -> unit Lwt.t
  (** [unwatch t w] removes [w] from [t]'s watch handlers. *)
end

(** {1 User-Defined Contents} *)

(** Store paths.

    An Irmin {{!Irmin.S}store} binds {{!Path.S.t}paths} to
    user-defined {{!Contents.S}contents}. Paths are composed by basic
    elements, that we call {{!Path.S.step}steps}. The following [Path]
    module provides functions to manipulate steps and paths. *)
module Path : sig
  (** {1 Path} *)

  (** Signature for path implementations.*)
  module type S = sig
    (** {1 Path} *)

    type t
    (** The type for path values. *)

    type step
    (** Type type for path's steps. *)

    val empty : t
    (** The empty path. *)

    val v : step list -> t
    (** Create a path from a list of steps. *)

    val is_empty : t -> bool
    (** Check if the path is empty. *)

    val cons : step -> t -> t
    (** Prepend a step to the path. *)

    val rcons : t -> step -> t
    (** Append a step to the path. *)

    val decons : t -> (step * t) option
    (** Deconstruct the first element of the path. Return [None] if
        the path is empty. *)

    val rdecons : t -> (t * step) option
    (** Deconstruct the last element of the path. Return [None] if the
        path is empty. *)

    val map : t -> (step -> 'a) -> 'a list
    (** [map t f] maps [f] over all steps of [t]. *)

    (** {1 Value Types} *)

    val t : t Type.t
    (** [t] is the value type for {!t}. *)

    val step_t : step Type.t
    (** [step_t] is the value type for {!step}. *)
  end

  (** An implementation of paths as string lists. *)
  module String_list : S with type step = string and type t = string list
end

(** Hashing functions.

    [Hash] provides user-defined hash functions to digest serialized
    contents. Some {{!backend}backends} might be parameterized by such
    hash functions, others might work with a fixed one (for instance,
    the Git format uses only {{!Hash.SHA1}SHA1}).

    A {{!Hash.SHA1}SHA1} implementation is available to pass to the
    backends. *)
module Hash : sig
  (** {1 Contents Hashing} *)

  (** Signature for hash values. *)
  module type S = sig
    (** Signature for digest hashes, inspired by Digestif. *)

    type t
    (** The type for digest hashes. *)

    val hash : ((string -> unit) -> unit) -> t
    (** Compute a deterministic store key from a sequence of strings. *)

    val short_hash : t -> int
    (** [short_hash h] is a small hash of [h], to be used for instance as
       the `hash` function of an OCaml [Hashtbl]. *)

    val hash_size : int
    (** [hash_size] is the size of hash results, in bytes. *)

    (** {1 Value Types} *)

    val t : t Type.t
    (** [t] is the value type for {!t}. *)
  end

  (** Signature for typed hashes, where [hash] directly takes a value
      as argument and incremental hashing is not possible. *)
  module type TYPED = sig
    type t

    type value

    val hash : value -> t
    (** Compute a deterministic store key from a string. *)

    val short_hash : t -> int
    (** [short_hash h] is a small hash of [h], to be used for instance as
       the `hash` function of an OCaml [Hashtbl]. *)

    val hash_size : int
    (** [hash_size] is the size of hash results, in bytes. *)

    (** {1 Value Types} *)

    val t : t Type.t
    (** [t] is the value type for {!t}. *)
  end

  module Make (H : Digestif.S) : S with type t = H.t
  (** Digestif hashes. *)

  module SHA1 : S

  module RMD160 : S

  module SHA224 : S

  module SHA256 : S

  module SHA384 : S

  module SHA512 : S

  module BLAKE2B : S

  module BLAKE2S : S

  module V1 (H : S) : S with type t = H.t
  (** v1 serialisation *)

  (** Typed hashes. *)

  module Typed (K : S) (E : Type.S) :
    TYPED with type t = K.t and type value = E.t
end

(** [Metadata] defines metadata that is attached to contents but stored in
    nodes. The Git backend uses this to indicate the type of file (normal,
    executable or symlink). *)
module Metadata : sig
  module type S = sig
    type t
    (** The type for metadata. *)

    val t : t Type.t
    (** [t] is the value type for {!t}. *)

    val merge : t Merge.t
    (** [merge] is the merge function for metadata. *)

    val default : t
    (** The default metadata to attach, for APIs that don't
        care about metadata. *)
  end

  module None : S with type t = unit
  (** A metadata definition for systems that don't use metadata. *)
end

(** [Contents] specifies how user-defined contents need to be {e
    serializable} and {e mergeable}.

    The user needs to provide:

    {ul
    {- a type [t] to be used as store contents.}
    {- a value type for [t] (built using the {{!Irmin.Type}Irmin.Type} combinators).}
    {- a 3-way [merge] function, to handle conflicts between multiple
    versions of the same contents.}
    }

    Default implementations for {{!Contents.String}idempotent string}
    and {{!Contents.Json}JSON} contents are provided. *)
module Contents : sig
  module type S = sig
    (** {1 Signature for store contents} *)

    type t
    (** The type for user-defined contents. *)

    val t : t Type.t
    (** [t] is the value type for {!t}. *)

    val merge : t option Merge.t
    (** Merge function. Evaluates to [`Conflict msg] if the values
        cannot be merged properly. The arguments of the merge function
        can take [None] to mean that the key does not exists for
        either the least-common ancestor or one of the two merging
        points. The merge function returns [None] when the key's value
        should be deleted. *)
  end

  module String : S with type t = string
  (** Contents of type [string], with the {{!Irmin.Merge.default}default}
      3-way merge strategy: assume that update operations are idempotent and
      conflict iff values are modified concurrently. *)

  type json =
    [ `Null
    | `Bool of bool
    | `String of string
    | `Float of float
    | `O of (string * json) list
    | `A of json list ]

  module Json : S with type t = (string * json) list
  (** [Json] contents are associations from strings to [json] values
     stored as JSON encoded strings.  If the same JSON key has been
     modified concurrently with different values then the [merge]
     function conflicts. *)

  module Json_value : S with type t = json
  (** [Json_value] allows any kind of json value to be stored, not only objects. *)

  module V1 : sig
    module String : S with type t = string
    (** Same as {!String} but use v1 serialisation format. *)
  end

  (** Contents store. *)
  module type STORE = sig
    include CONTENT_ADDRESSABLE_STORE

    val merge : [ `Read | `Write ] t -> key option Merge.t
    (** [merge t] lifts the merge functions defined on contents values
        to contents key. The merge function will: {e (i)} read the
        values associated with the given keys, {e (ii)} use the merge
        function defined on values and {e (iii)} write the resulting
        values into the store to get the resulting key. See
        {!Contents.S.merge}.

        If any of these operations fail, return [`Conflict]. *)

    (** [Key] provides base functions for user-defined contents keys. *)
    module Key : Hash.TYPED with type t = key and type value = value

    module Val : S with type t = value
    (** [Val] provides base functions for user-defined contents values. *)
  end

  (** [Store] creates a contents store. *)
  module Store (S : sig
    include CONTENT_ADDRESSABLE_STORE

    module Key : Hash.S with type t = key

    module Val : S with type t = value
  end) :
    STORE with type 'a t = 'a S.t and type key = S.key and type value = S.value
end

(** User-defined branches. *)
module Branch : sig
  (** {1 Branches} *)

  (** The signature for branches. Irmin branches are similar to Git
      branches: they are used to associated user-defined names to head
      commits. Branches have a default value: the
      {{!Branch.S.master}master} branch. *)
  module type S = sig
    (** {1 Signature for Branches} *)

    type t
    (** The type for branches. *)

    val t : t Type.t
    (** [t] is the value type for {!t}. *)

    val master : t
    (** The name of the master branch. *)

    val is_valid : t -> bool
    (** Check if the branch is valid. *)
  end

  module String : S with type t = string
  (** [String] is an implementation of {{!Branch.S}S} where branches
      are strings. The [master] branch is ["master"]. Valid branch
      names contain only alpha-numeric characters, [-], [_], [.], and
      [/]. *)

  (** [STORE] specifies the signature for branch stores.

      A {i branch store} is a mutable and reactive key / value store,
      where keys are branch names created by users and values are keys
      are head commmits. *)
  module type STORE = sig
    (** {1 Branch Store} *)

    include ATOMIC_WRITE_STORE

    module Key : S with type t = key
    (** Base functions on keys. *)

    module Val : Hash.S with type t = value
    (** Base functions on values. *)
  end
end

type remote = ..
(** The type for remote stores. *)

type config
(** The type for backend-specific configuration values.

    Every backend has different configuration options, which are kept
    abstract to the user. *)

(** [Private] defines functions only useful for creating new
    backends. If you are just using the library (and not developing a
    new backend), you should not use this module. *)
module Private : sig
  (** Backend configuration.

    A backend configuration is a set of {{!keys}keys} mapping to
    typed values. Backends define their own keys. *)
  module Conf : sig
    (** {1 Configuration converters}

        A configuration converter transforms a string value to an OCaml
        value and vice-versa. There are a few
        {{!builtin_converters}built-in converters}. *)

    type 'a parser = string -> ('a, [ `Msg of string ]) result
    (** The type for configuration converter parsers. *)

    type 'a printer = 'a Fmt.t
    (** The type for configuration converter printers. *)

    type 'a converter = 'a parser * 'a printer
    (** The type for configuration converters. *)

    val parser : 'a converter -> 'a parser
    (** [parser c] is [c]'s parser. *)

    val printer : 'a converter -> 'a printer
    (** [converter c] is [c]'s printer. *)

    (** {1:keys Keys} *)

    type 'a key
    (** The type for configuration keys whose lookup value is ['a]. *)

    val key :
      ?docs:string ->
      ?docv:string ->
      ?doc:string ->
      string ->
      'a converter ->
      'a ->
      'a key
    (** [key ~docs ~docv ~doc name conv default] is a configuration key named
        [name] that maps to value [default] by default. [conv] is
        used to convert key values provided by end users.

        [docs] is the title of a documentation section under which the
        key is documented. [doc] is a short documentation string for the
        key, this should be a single sentence or paragraph starting with
        a capital letter and ending with a dot.  [docv] is a
        meta-variable for representing the values of the key
        (e.g. ["BOOL"] for a boolean).

        @raise Invalid_argument if the key name is not made of a
        sequence of ASCII lowercase letter, digit, dash or underscore.

        {b Warning.} No two keys should share the same [name] as this
        may lead to difficulties in the UI. *)

    val name : 'a key -> string
    (** The key name. *)

    val conv : 'a key -> 'a converter
    (** [tc k] is [k]'s converter. *)

    val default : 'a key -> 'a
    (** [default k] is [k]'s default value. *)

    val doc : 'a key -> string option
    (** [doc k] is [k]'s documentation string (if any). *)

    val docv : 'a key -> string option
    (** [docv k] is [k]'s value documentation meta-variable (if any). *)

    val docs : 'a key -> string option
    (** [docs k] is [k]'s documentation section (if any). *)

    val root : string option key
    (** Default [--root=ROOT] argument. *)

    (** {1:conf Configurations} *)

    type t = config
    (** The type for configurations. *)

    val empty : t
    (** [empty] is the empty configuration. *)

    val singleton : 'a key -> 'a -> t
    (** [singleton k v] is the configuration where [k] maps to [v]. *)

    val is_empty : t -> bool
    (** [is_empty c] is [true] iff [c] is empty. *)

    val mem : t -> 'a key -> bool
    (** [mem c k] is [true] iff [k] has a mapping in [c]. *)

    val add : t -> 'a key -> 'a -> t
    (** [add c k v] is [c] with [k] mapping to [v]. *)

    val rem : t -> 'a key -> t
    (** [rem c k] is [c] with [k] unbound. *)

    val union : t -> t -> t
    (** [union r s] is the union of the configurations [r] and [s]. *)

    val find : t -> 'a key -> 'a option
    (** [find c k] is [k]'s mapping in [c], if any. *)

    val get : t -> 'a key -> 'a
    (** [get c k] is [k]'s mapping in [c].

        {b Raises.} [Not_found] if [k] is not bound in [d]. *)

    (** {1:builtin_converters Built-in value converters}  *)

    val bool : bool converter
    (** [bool] converts values with [bool_of_string].  *)

    val int : int converter
    (** [int] converts values with [int_of_string]. *)

    val string : string converter
    (** [string] converts values with the identity function. *)

    val uri : Uri.t converter
    (** [uri] converts values with {!Uri.of_string}. *)

    val some : 'a converter -> 'a option converter
    (** [string] converts values with the identity function. *)
  end

  (** [Watch] provides helpers to register event notifications on
      read-write stores. *)
  module Watch : sig
    (** {1 Watch Helpers} *)

    (** The signature for watch helpers. *)
    module type S = sig
      (** {1 Watch Helpers} *)

      type key
      (** The type for store keys. *)

      type value
      (** The type for store values. *)

      type watch
      (** The type for watch handlers. *)

      type t
      (** The type for watch state. *)

      val stats : t -> int * int
      (** [stats t] is a tuple [(k,a)] represeting watch stats. [k] is
          the number of single key watchers for the store [t] and [a] the
          number of global watchers for [t]. *)

      val notify : t -> key -> value option -> unit Lwt.t
      (** Notify all listeners in the given watch state that a key has
          changed, with the new value associated to this key. [None]
          means the key has been removed. *)

      val v : unit -> t
      (** Create a watch state. *)

      val clear : t -> unit Lwt.t
      (** Clear all register listeners in the given watch state. *)

      val watch_key :
        t -> key -> ?init:value -> (value diff -> unit Lwt.t) -> watch Lwt.t
      (** Watch a given key for changes. More efficient than {!watch}. *)

      val watch :
        t ->
        ?init:(key * value) list ->
        (key -> value diff -> unit Lwt.t) ->
        watch Lwt.t
      (** Add a watch handler. To watch a specific key, use
          {!watch_key} which is more efficient. *)

      val unwatch : t -> watch -> unit Lwt.t
      (** Remove a watch handler. *)

      val listen_dir :
        t ->
        string ->
        key:(string -> key option) ->
        value:(key -> value option Lwt.t) ->
        (unit -> unit Lwt.t) Lwt.t
      (** Register a thread looking for changes in the given directory
          and return a function to stop watching and free up
          resources. *)
    end

    val workers : unit -> int
    (** [workers ()] is the number of background worker threads
        managing event notification currently active. *)

    type hook =
      int -> string -> (string -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
    (** The type for watch hooks. *)

    val none : hook
    (** [none] is the hooks which asserts false. *)

    val set_listen_dir_hook : hook -> unit
    (** Register a function which looks for file changes in a
        directory and return a function to stop watching. It is
        probably best to use {!Irmin_watcher.hook} there. By default,
        it uses {!none}. *)

    (** [Make] builds an implementation of watch helpers. *)
    module Make (K : Type.S) (V : Type.S) :
      S with type key = K.t and type value = V.t
  end

  module Lock : sig
    (** {1 Process locking helpers} *)

    module type S = sig
      type t
      (** The type for lock manager. *)

      type key
      (** The type for key to be locked. *)

      val v : unit -> t
      (** Create a lock manager. *)

      val with_lock : t -> key -> (unit -> 'a Lwt.t) -> 'a Lwt.t
      (** [with_lock t k f] executes [f ()] while holding the exclusive
          lock associated to the key [k]. *)

      val stats : t -> int
    end

    module Make (K : Type.S) : S with type key = K.t
    (** Create a lock manager implementation. *)
  end

  (** [Node] provides functions to describe the graph-like structured
      values.

      The node blocks form a labeled directed acyclic graph, labeled
      by {{!Path.S.step}steps}: a list of steps defines a
      unique path from one node to an other.

      Each node can point to user-defined {{!Contents.S}contents}
      values. *)
  module Node : sig
    module type S = sig
      (** {1 Node values} *)

      type t
      (** The type for node values. *)

      type metadata
      (** The type for node metadata. *)

      type hash
      (** The type for keys. *)

      type step
      (** The type for steps between nodes. *)

      type value = [ `Node of hash | `Contents of hash * metadata ]
      (** The type for either (node) keys or (contents) keys combined with
          their metadata. *)

      val v : (step * value) list -> t
      (** [create l] is a new node. *)

      val list : t -> (step * value) list
      (** [list t] is the contents of [t]. *)

      val empty : t
      (** [empty] is the empty node. *)

      val is_empty : t -> bool
      (** [is_empty t] is true iff [t] is {!empty}. *)

      val find : t -> step -> value option
      (** [find t s] is the value associated with [s] in [t].

          A node can point to user-defined
          {{!Node.S.contents}contents}. The edge between the node and
          the contents is labeled by a {{!Node.S.step}step}. *)

      val add : t -> step -> value -> t
      (** [add t s v] is the node where [find t v] is [Some s] but
          is similar to [t] otherwise. *)

      val remove : t -> step -> t
      (** [remove t s] is the node where [find t s] is [None] but is
          similar to [t] otherwise. *)

      (** {1 Value types} *)

      val t : t Type.t
      (** [t] is the value type for {!t}. *)

      val default : metadata
      (** [default] is the default metadata value. *)

      val metadata_t : metadata Type.t
      (** [metadata_t] is the value type for {!metadata}. *)

      val hash_t : hash Type.t
      (** [hash_t] is the value type for {!hash}. *)

      val step_t : step Type.t
      (** [step_t] is the value type for {!step}. *)

      val value_t : value Type.t
      (** [value_t] is the value type for {!value}. *)
    end

    (** [Make] provides a simple node implementation, parameterized by
        the contents and notes keys [K], paths [P] and metadata [M]. *)
    module Make
        (K : Type.S) (P : sig
          type step

          val step_t : step Type.t
        end)
        (M : Metadata.S) :
      S with type hash = K.t and type step = P.step and type metadata = M.t

    (** v1 serialisation *)
    module V1 (S : S) : sig
      include
        S
          with type hash = S.hash
           and type step = S.step
           and type metadata = S.metadata

      val import : S.t -> t

      val export : t -> S.t
    end

    (** [STORE] specifies the signature for node stores. *)
    module type STORE = sig
      include CONTENT_ADDRESSABLE_STORE

      module Path : Path.S
      (** [Path] provides base functions on node paths. *)

      val merge : [ `Read | `Write ] t -> key option Merge.t
      (** [merge] is the 3-way merge function for nodes keys. *)

      (** [Key] provides base functions for node keys. *)
      module Key : Hash.TYPED with type t = key and type value = value

      module Metadata : Metadata.S
      (** [Metadata] provides base functions for node metadata. *)

      (** [Val] provides base functions for node values. *)
      module Val :
        S
          with type t = value
           and type hash = key
           and type metadata = Metadata.t
           and type step = Path.step

      module Contents : Contents.STORE with type key = Val.hash
      (** [Contents] is the underlying contents store. *)
    end

    (** [Store] creates node stores. *)
    module Store
        (C : Contents.STORE)
        (P : Path.S)
        (M : Metadata.S) (S : sig
          include CONTENT_ADDRESSABLE_STORE with type key = C.key

          module Key : Hash.S with type t = key

          module Val :
            S
              with type t = value
               and type hash = key
               and type metadata = M.t
               and type step = P.step
        end) :
      STORE
        with type 'a t = 'a C.t * 'a S.t
         and type key = S.key
         and type value = S.value
         and module Path = P
         and module Metadata = M
         and type Key.t = S.Key.t
         and module Val = S.Val

    (** [Graph] specifies the signature for node graphs. A node graph
        is a deterministic DAG, labeled by steps. *)
    module type GRAPH = sig
      (** {1 Node Graphs} *)

      type 'a t
      (** The type for store handles. *)

      type metadata
      (** The type for node metadata. *)

      type contents
      (** The type of user-defined contents. *)

      type node
      (** The type for node values. *)

      type step
      (** The type of steps. A step is used to pass from one node to
          another. *)

      type path
      (** The type of store paths. A path is composed of
          {{!step}steps}. *)

      type value = [ `Node of node | `Contents of contents * metadata ]
      (** The type for store values. *)

      val empty : [> `Write ] t -> node Lwt.t
      (** The empty node. *)

      val v : [> `Write ] t -> (step * value) list -> node Lwt.t
      (** [v t n] is a new node containing [n]. *)

      val list : [> `Read ] t -> node -> (step * value) list Lwt.t
      (** [list t n] is the contents of the node [n]. *)

      val find : [> `Read ] t -> node -> path -> value option Lwt.t
      (** [find t n p] is the contents of the path [p] starting form
          [n]. *)

      val add : [ `Read | `Write ] t -> node -> path -> value -> node Lwt.t
      (** [add t n p v] is the node [x] such that [find t x p] is
          [Some v] and it behaves the same [n] for other
          operations. *)

      val remove : [ `Read | `Write ] t -> node -> path -> node Lwt.t
      (** [remove t n path] is the node [x] such that [find t x] is
          [None] and it behhaves then same as [n] for other
          operations. *)

      val closure :
        [> `Read ] t -> min:node list -> max:node list -> node list Lwt.t
      (** [closure t ~min ~max] is the transitive closure [c] of [t]'s
          nodes such that:

          {ul
          {- There is a path in [t] from any nodes in [min] to nodes
          in [c]. If [min] is empty, that condition is always true.}
          {- There is a path in [t] from any nodes in [c] to nodes in
          [max]. If [max] is empty, that condition is always false.}
          }

          {b Note:} Both [min] and [max] are subsets of [c].*)

      (** {1 Value Types} *)

      val metadata_t : metadata Type.t
      (** [metadat_t] is the value type for {!metadata}. *)

      val contents_t : contents Type.t
      (** [contents_t] is the value type for {!contents}. *)

      val node_t : node Type.t
      (** [node_t] is the value type for {!node}. *)

      val step_t : step Type.t
      (** [step_t] is the value type for {!step}. *)

      val path_t : path Type.t
      (** [path_t] is the value type for {!path}. *)

      val value_t : value Type.t
      (** [value_t] is the value type for {!value}. *)
    end

    module Graph (S : STORE) :
      GRAPH
        with type 'a t = 'a S.t
         and type contents = S.Contents.key
         and type metadata = S.Val.metadata
         and type node = S.key
         and type path = S.Path.t
         and type step = S.Path.step
  end

  (** Commit values represent the store history.

      Every commit contains a list of predecessor commits, and the
      collection of commits form an acyclic directed graph.

      Every commit also can contain an optional key, pointing to a
      {{!Private.Commit.STORE}node} value. See the
      {{!Private.Node.STORE}Node} signature for more details on node
      values. *)
  module Commit : sig
    module type S = sig
      (** {1 Commit values} *)

      type t
      (** The type for commit values. *)

      type hash
      (** Type for keys. *)

      val v : info:Info.t -> node:hash -> parents:hash list -> t
      (** Create a commit. *)

      val node : t -> hash
      (** The underlying node. *)

      val parents : t -> hash list
      (** The commit parents. *)

      val info : t -> Info.t
      (** The commit info. *)

      (** {1 Value Types} *)

      val t : t Type.t
      (** [t] is the value type for {!t}. *)

      val hash_t : hash Type.t
      (** [hash_t] is the value type for {!hash}. *)
    end

    module Make (K : Type.S) : S with type hash = K.t
    (** [Make] provides a simple implementation of commit values,
        parameterized by the commit and node keys [K]. *)

    (** V1 serialisation. *)
    module V1 (S : S) : sig
      include S with type hash = S.hash

      val import : S.t -> t

      val export : t -> S.t
    end

    (** [STORE] specifies the signature for commit stores. *)
    module type STORE = sig
      (** {1 Commit Store} *)

      include CONTENT_ADDRESSABLE_STORE

      val merge : [ `Read | `Write ] t -> info:Info.f -> key option Merge.t
      (** [merge] is the 3-way merge function for commit keys. *)

      (** [Key] provides base functions for commit keys. *)
      module Key : Hash.TYPED with type t = key and type value = value

      (** [Val] provides functions for commit values. *)
      module Val : S with type t = value and type hash = key

      module Node : Node.STORE with type key = Val.hash
      (** [Node] is the underlying node store. *)
    end

    (** [Store] creates a new commit store. *)
    module Store
        (N : Node.STORE) (S : sig
          include CONTENT_ADDRESSABLE_STORE with type key = N.key

          module Key : Hash.S with type t = key

          module Val : S with type t = value and type hash = key
        end) :
      STORE
        with type 'a t = 'a N.t * 'a S.t
         and type key = S.key
         and type value = S.value
         and type Key.t = S.Key.t
         and module Val = S.Val

    (** [History] specifies the signature for commit history. The
        history is represented as a partial-order of commits and basic
        functions to search through that history are provided.

        Every commit can point to an entry point in a node graph, where
        user-defined contents are stored. *)
    module type HISTORY = sig
      (** {1 Commit History} *)

      type 'a t
      (** The type for store handles. *)

      type node
      (** The type for node values. *)

      type commit
      (** The type for commit values. *)

      type v
      (** The type for commit objects. *)

      val v :
        [> `Write ] t ->
        node:node ->
        parents:commit list ->
        info:Info.t ->
        (commit * v) Lwt.t
      (** Create a new commit. *)

      val parents : [> `Read ] t -> commit -> commit list Lwt.t
      (** Get the commit parents.

          Commits form a append-only, fully functional, partial-order
          data-structure: every commit carries the list of its
          immediate predecessors. *)

      val merge : [ `Read | `Write ] t -> info:Info.f -> commit Merge.t
      (** [merge t] is the 3-way merge function for commit.  *)

      val lcas :
        [> `Read ] t ->
        ?max_depth:int ->
        ?n:int ->
        commit ->
        commit ->
        (commit list, [ `Max_depth_reached | `Too_many_lcas ]) result Lwt.t
      (** Find the lowest common ancestors
          {{:http://en.wikipedia.org/wiki/Lowest_common_ancestor}lca}
          between two commits. *)

      val lca :
        [ `Read | `Write ] t ->
        info:Info.f ->
        ?max_depth:int ->
        ?n:int ->
        commit list ->
        (commit option, Merge.conflict) result Lwt.t
      (** Compute the lowest common ancestors ancestor of a list of
          commits by recursively calling {!lcas} and merging the
          results.

          If one of the merges results in a conflict, or if a call to
          {!lcas} returns either [Error `Max_depth_reached] or
          [Error `Too_many_lcas] then the function returns the same
          error. *)

      val three_way_merge :
        [ `Read | `Write ] t ->
        info:Info.f ->
        ?max_depth:int ->
        ?n:int ->
        commit ->
        commit ->
        (commit, Merge.conflict) result Lwt.t
      (** Compute the {!lcas} of the two commit and 3-way merge the
          result. *)

      val closure :
        [> `Read ] t -> min:commit list -> max:commit list -> commit list Lwt.t
      (** Same as {{!Private.Node.GRAPH.closure}GRAPH.closure} but for
          the history graph. *)

      (** {1 Value Types} *)

      val commit_t : commit Type.t
      (** [commit_t] is the value type for {!commit}. *)
    end

    (** Build a commit history. *)
    module History (S : STORE) :
      HISTORY
        with type 'a t = 'a S.t
         and type node = S.Node.key
         and type commit = S.key
  end

  (** The signature for slices. *)
  module Slice : sig
    module type S = sig
      (** {1 Slices} *)

      type t
      (** The type for slices. *)

      type contents
      (** The type for exported contents. *)

      type node
      (** The type for exported nodes. *)

      type commit
      (** The type for exported commits. *)

      type value = [ `Contents of contents | `Node of node | `Commit of commit ]
      (** The type for exported values. *)

      val empty : unit -> t Lwt.t
      (** Create a new empty slice. *)

      val add : t -> value -> unit Lwt.t
      (** [add t v] adds [v] to [t]. *)

      val iter : t -> (value -> unit Lwt.t) -> unit Lwt.t
      (** [iter t f] calls [f] on all values of [t]. *)

      (** {1 Value Types} *)

      val t : t Type.t
      (** [t] is the value type for {!t}. *)

      val contents_t : contents Type.t
      (** [content_t] is the value type for {!contents}. *)

      val node_t : node Type.t
      (** [node_t] is the value type for {!node}. *)

      val commit_t : commit Type.t
      (** [commit_t] is the value type for {!commit}. *)

      val value_t : value Type.t
      (** [value_t] is the value type for {!value}. *)
    end

    (** Build simple slices. *)
    module Make (C : Contents.STORE) (N : Node.STORE) (H : Commit.STORE) :
      S
        with type contents = C.key * C.value
         and type node = N.key * N.value
         and type commit = H.key * H.value
  end

  module Sync : sig
    module type S = sig
      (** {1 Remote synchronization} *)

      type t
      (** The type for store handles. *)

      type commit
      (** The type for store heads. *)

      type branch
      (** The type for branch IDs. *)

      type endpoint
      (** The type for sync endpoints. *)

      val fetch :
        t ->
        ?depth:int ->
        endpoint ->
        branch ->
        (commit option, [ `Msg of string ]) result Lwt.t
      (** [fetch t uri] fetches the contents of the remote store
          located at [uri] into the local store [t]. Return the head
          of the remote branch with the same name, which is now in the
          local store. [No_head] means no such branch exists. *)

      val push :
        t ->
        ?depth:int ->
        endpoint ->
        branch ->
        (unit, [ `Msg of string | `Detached_head ]) result Lwt.t
      (** [push t uri] pushes the contents of the local store [t] into
          the remote store located at [uri]. *)
    end

    (** [None] is an implementation of {{!Private.Sync.S}S} which does
        nothing. *)
    module None (H : Type.S) (B : Type.S) : sig
      include S with type commit = H.t and type branch = B.t

      val v : 'a -> t Lwt.t
      (** Create a remote store handle. *)
    end
  end

  (** The complete collection of private implementations. *)
  module type S = sig
    (** {1 Private Implementations} *)

    module Hash : Hash.S
    (** Internal hashes. *)

    module Contents : Contents.STORE with type key = Hash.t
    (** Private content store. *)

    (** Private node store. *)
    module Node :
      Node.STORE with type key = Hash.t and type Val.hash = Contents.key

    (** Private commit store. *)
    module Commit :
      Commit.STORE with type key = Hash.t and type Val.hash = Node.key

    module Branch : Branch.STORE with type value = Commit.key
    (** Private branch store. *)

    (** Private slices. *)
    module Slice :
      Slice.S
        with type contents = Contents.key * Contents.value
         and type node = Node.key * Node.value
         and type commit = Commit.key * Commit.value

    (** Private repositories. *)
    module Repo : sig
      type t

      val v : config -> t Lwt.t

      val close : t -> unit Lwt.t

      val contents_t : t -> [ `Read ] Contents.t

      val node_t : t -> [ `Read ] Node.t

      val commit_t : t -> [ `Read ] Commit.t

      val branch_t : t -> Branch.t

      val batch :
        t ->
        ([ `Read | `Write ] Contents.t ->
        [ `Read | `Write ] Node.t ->
        [ `Read | `Write ] Commit.t ->
        'a Lwt.t) ->
        'a Lwt.t
    end

    (** URI-based low-level sync. *)
    module Sync : sig
      include Sync.S with type commit = Commit.key and type branch = Branch.key

      val v : Repo.t -> t Lwt.t
    end
  end
end

(** {1 High-level Stores}

    An Irmin store is a branch-consistent store where keys are lists
    of steps.

    An example is a Git repository where keys are filenames, {e i.e.}
    lists of ['/']-separated strings. More complex examples are
    structured values, where steps might contain first-class field
    accessors and array offsets.

    Irmin provides the following features:

    {ul
    {- Support for fast clones, branches and merges, in a fashion very
       similar to Git.}
    {- Efficient staging areas for fast, transient, in-memory operations.}
    {- Fast {{!Sync}synchronization} primitives between remote
       stores, using native backend protocols (as the Git protocol)
       when available.}
    }
*)

(** Irmin stores. *)
module type S = sig
  (** {1 Irmin stores}

      Irmin stores are tree-like read-write stores with
      extended capabilities. They allow an application (or a
      collection of applications) to work with multiple local states,
      which can be forked and merged programmatically, without having
      to rely on a global state. In a way very similar to version
      control systems, Irmin local states are called {i branches}.

      There are two kinds of store in Irmin: the ones based on
      {{!persistent}persistent} named branches and the ones based
      {{!temporary}temporary} detached heads. These exist relative to a
      local, larger (and shared) store, and have some (shared)
      contents. This is exactly the same as usual version control
      systems, that the informed user can see as an implicit purely
      functional data-structure. *)

  type repo
  (** The type for Irmin repositories. *)

  type t
  (** The type for Irmin stores. *)

  type step
  (** The type for {!key} steps. *)

  type key
  (** The type for store keys. A key is a sequence of {!step}s. *)

  type metadata
  (** The type for store metadata. *)

  type contents
  (** The type for store contents. *)

  type node
  (** The type for store nodes. *)

  type tree = [ `Node of node | `Contents of contents * metadata ]
  (** The type for store trees. *)

  type hash
  (** The type for object hashes. *)

  type commit
  (** Type for commit identifiers. Similar to Git's commit SHA1s. *)

  type branch
  (** Type for persistent branch names. Branches usually share a
      common global namespace and it's the user's responsibility to
      avoid name clashes. *)

  type slice
  (** Type for store slices. *)

  type lca_error = [ `Max_depth_reached | `Too_many_lcas ]
  (** The type for errors associated with functions computing least
      common ancestors *)

  type ff_error = [ `No_change | `Rejected | lca_error ]
  (** The type for errors for {!fast_forward}. *)

  (** Repositories. *)
  module Repo : sig
    (** {1 Repositories}

        A repository contains a set of branches. *)

    type t = repo
    (** The type of repository handles. *)

    val v : config -> t Lwt.t
    (** [v config] connects to a repository in a backend-specific
        manner. *)

    val close : t -> unit Lwt.t
    (** [close t] frees up all resources associated with [t]. Any
        operations run on a closed repository will raise [Closed]. *)

    val heads : t -> commit list Lwt.t
    (** [heads] is {!Head.list}. *)

    val branches : t -> branch list Lwt.t
    (** [branches] is {!Branch.list}. *)

    val export :
      ?full:bool ->
      ?depth:int ->
      ?min:commit list ->
      ?max:commit list ->
      t ->
      slice Lwt.t
    (** [export t ~depth ~min ~max] exports the store slice between
        [min] and [max], using at most [depth] history depth (starting
        from the max).

        If [max] is not specified, use the current [heads]. If [min] is
        not specified, use an unbound past (but can still be limited by
        [depth]).

        [depth] is used to limit the depth of the commit history. [None]
        here means no limitation.

        If [full] is set (default is true), the full graph, including the
        commits, nodes and contents, is exported, otherwise it is the
        commit history graph only. *)

    val import : t -> slice -> (unit, [ `Msg of string ]) result Lwt.t
    (** [import t s] imports the contents of the slice [s] in [t]. Does
        not modify branches. *)
  end

  val empty : repo -> t Lwt.t
  (** [empty repo] is a temporary, empty store. Becomes a normal
      temporary store after the first update. *)

  val master : repo -> t Lwt.t
  (** [master repo] is a persistent store based on [r]'s master
      branch. This operation is cheap, can be repeated multiple
      times. *)

  val of_branch : repo -> branch -> t Lwt.t
  (** [of_branch r name] is a persistent store based on the branch
      [name]. Similar to [master], but use [name] instead
      {!Branch.S.master}. *)

  val of_commit : commit -> t Lwt.t
  (** [of_commit c] is a temporary store, based on the commit [c].

      Temporary stores do not have stable names: instead they can be
      addressed using the hash of the current commit. Temporary stores
      are similar to Git's detached heads. In a temporary store, all
      the operations are performed relative to the current head and
      update operations can modify the current head: the current
      stores's head will automatically become the new head obtained
      after performing the update. *)

  val repo : t -> repo
  (** [repo t] is the repository containing [t]. *)

  val tree : t -> tree Lwt.t
  (** [tree t] is [t]'s current tree. Contents is not allowed at the
      root of the tree. *)

  (** [Status] provides base functions for store statuses. *)
  module Status : sig
    type t = [ `Empty | `Branch of branch | `Commit of commit ]
    (** The type for store status. *)

    val t : repo -> t Type.t
    (** [t] is the value type for {!t}. *)

    val pp : t Fmt.t
    (** [pp] is the pretty-printer for store status. *)
  end

  val status : t -> Status.t
  (** [status t] is [t]'s status. It can either be a branch, a commit
      or empty. *)

  (** Managing the store's heads. *)
  module Head : sig
    val list : repo -> commit list Lwt.t
    (** [list t] is the list of all the heads in local store. Similar
        to [git rev-list --all]. *)

    val find : t -> commit option Lwt.t
    (** [find t] is the current head of the store [t]. This works for
        both persistent and temporary branches. In the case of a
        persistent branch, this involves getting the the head
        associated with the branch, so this may block. In the case of
        a temporary store, it simply returns the current head. Returns
        [None] if the store has no contents. Similar to
        [git rev-parse HEAD]. *)

    val get : t -> commit Lwt.t
    (** Same as {!find} but raise [Invalid_argument] if the store does
        not have any contents. *)

    val set : t -> commit -> unit Lwt.t
    (** [set t h] updates [t]'s contents with the contents of the
        commit [h]. Can cause data loss as it discards the current
        contents. Similar to [git reset --hard <hash>]. *)

    val fast_forward :
      t ->
      ?max_depth:int ->
      ?n:int ->
      commit ->
      (unit, [ `No_change | `Rejected | lca_error ]) result Lwt.t
    (** [fast_forward t h] is similar to {!update} but the [t]'s head
        is updated to [h] only if [h] is stricly in the future of
        [t]'s current head. [max_depth] or [n] are used to limit the
        search space of the lowest common ancestors (see {!lcas}).

        The result is:
        {ul
        {- [Ok ()] if the operation is succesfull;}
        {- [Error `No_change] if [h] is already [t]'s head;}
        {- [Error `Rejected] if [h] is not in the strict future of [t]'s head.}
        {- [Error e] if the history exploration has been cut before
           getting useful results. In that case. the operation can be
           retried using different parameters of [n] and [max_depth]
           to get better results.}
        }

    *)

    val test_and_set :
      t -> test:commit option -> set:commit option -> bool Lwt.t
    (** Same as {!update_head} but check that the value is [test] before
        updating to [set]. Use {!update} or {!merge} instead if
        possible. *)

    val merge :
      into:t ->
      info:Info.f ->
      ?max_depth:int ->
      ?n:int ->
      commit ->
      (unit, Merge.conflict) result Lwt.t
    (** [merge ~into:t ?max_head ?n commit] merges the contents of the
        commit associated to [commit] into [t]. [max_depth] is the
        maximal depth used for getting the lowest common ancestor. [n]
        is the maximum number of lowest common ancestors. If present,
        [max_depth] or [n] are used to limit the search space of the
        lowest common ancestors (see {!lcas}). *)
  end

  module Hash : Hash.S with type t = hash
  (** Object hashes. *)

  (** [Commit] defines immutable objects to describe store updates. *)
  module Commit : sig
    type t = commit
    (** The type for store commits. *)

    val t : repo -> t Type.t
    (** [t] is the value type for {!t}. *)

    val pp_hash : t Fmt.t
    (** [pp] is the pretty-printer for commit. Display only the
        hash. *)

    val v : repo -> info:Info.t -> parents:hash list -> tree -> commit Lwt.t
    (** [v r i ~parents:p t] is the commit [c] such that:
        {ul
        {- [info c = i]}
        {- [parents c = p]}
        {- [tree c = t]}}
    *)

    val tree : commit -> tree
    (** [tree c] is [c]'s root tree. *)

    val parents : commit -> hash list
    (** [parents c] are [c]'s parents. *)

    val info : commit -> Info.t
    (** [info c] is [c]'s info. *)

    (** {1 Import/Export} *)

    val hash : commit -> hash
    (** [hash c] it [c]'s hash. *)

    val of_hash : repo -> hash -> commit option Lwt.t
    (** [of_hash r h] is the the commit object in [r] having [h] as
        hash, or [None] is no such commit object exists. *)
  end

  (** [Contents] provides base functions for the store's contents. *)
  module Contents : sig
    include Contents.S with type t = contents

    (** {1 Import/Export} *)

    val hash : contents -> hash
    (** [hash c] it [c]'s hash in the repository [r]. *)

    val of_hash : repo -> hash -> contents option Lwt.t
    (** [of_hash r h] is the the contents object in [r] having [h] as
        hash, or [None] is no such contents object exists. *)
  end

  (** Managing store's trees. *)

  module Tree : sig
    (** [Tree] provides immutable, in-memory partial mirror of the
        store, with lazy reads and delayed writes.

        Trees are like staging area in Git: they are immutable
        temporary non-persistent areas (they disappear if the host
        crash), held in memory for efficiency, where reads are done
        lazily and writes are done only when needed on commit: if you
        modify a key twice, only the last change will be written to
        the store when you commit. *)

    (** {1 Constructors} *)

    val empty : tree
    (** [empty] is the empty tree. The empty tree does not have
        associated backend configuration values, as they can perform
        in-memory operation, independently of any given backend. *)

    val of_contents : ?metadata:metadata -> contents -> tree
    (** [of_contents c] is the subtree built from the contents [c]. *)

    val of_node : node -> tree
    (** [of_node n] is the subtree built from the node [n]. *)

    val kind : tree -> key -> [ `Contents | `Node ] option Lwt.t
    (** [kind t k] is the type of [s] in [t]. It could either be a
        tree node or some file contents. It is [None] if [k] is not
        present in [t]. *)

    val list : tree -> key -> (step * [ `Contents | `Node ]) list Lwt.t
    (** [list t key] is the list of files and sub-nodes stored under [k]
        in [t]. *)

    (** {1 Diffs} *)

    val diff : tree -> tree -> (key * (contents * metadata) diff) list Lwt.t
    (** [diff x y] is the difference of contents between [x] and [y]. *)

    (** {1 Manipulating Contents} *)

    val mem : tree -> key -> bool Lwt.t
    (** [mem t k] is true iff [k] is associated to some contents in
        [t]. *)

    val find_all : tree -> key -> (contents * metadata) option Lwt.t
    (** [find_all t k] is [Some (b, m)] if [k] is associated to the
        contents [b] and metadata [m] in [t] and [None] if [k] is not
        present in [t]. *)

    val find : tree -> key -> contents option Lwt.t
    (** [find] is similar to {!find_all} but it discards metadata. *)

    val get_all : tree -> key -> (contents * metadata) Lwt.t
    (** Same as {!find_all} but raise [Invalid_arg] if [k] is not
        present in [t]. *)

    val get : tree -> key -> contents Lwt.t
    (** Same as {!get_all} but ignore the metadata. *)

    val add : tree -> key -> ?metadata:metadata -> contents -> tree Lwt.t
    (** [add t k c] is the tree where the key [k] is bound to the
        contents [c] but is similar to [t] for other bindings. *)

    val remove : tree -> key -> tree Lwt.t
    (** [remove t k] is the tree where [k] bindings has been removed
        but is similar to [t] for other bindings. *)

    (** {1 Manipulating Subtrees} *)

    val mem_tree : tree -> key -> bool Lwt.t
    (** [mem_tree t k] is false iff [find_tree k = None]. *)

    val find_tree : tree -> key -> tree option Lwt.t
    (** [find_tree t k] is [Some v] if [k] is associated to [v] in
        [t]. It is [None] if [k] is not present in [t]. *)

    val get_tree : tree -> key -> tree Lwt.t
    (** [get_tree t k] is [v] if [k] is associated to [v] in [t].
        Raise [Invalid_arg] if [k] is not present in [t].*)

    val add_tree : tree -> key -> tree -> tree Lwt.t
    (** [add_tree t k v] is the tree where the key [k] is bound to the
        tree [v] but is similar to [t] for other bindings *)

    val merge : tree Merge.t
    (** [merge] is the 3-way merge function for trees. *)

    (** {1 Folds} *)

    type marks
    (** The type for fold marks. *)

    val empty_marks : unit -> marks
    (** [empty_marks ()] is an empty collection of marks. *)

    type 'a force = [ `True | `False of key -> 'a -> 'a Lwt.t ]
    (** The type for {!fold}'s [force] parameter. [`True] forces the
        fold to read the objects of the lazy nodes. [`False f] is
        applying [f] on every lazy node instead. *)

    type uniq = [ `False | `True | `Marks of marks ]
    (** The type for {!fold}'s [uniq] parameters. [`False] folds over
        all the nodes. [`True] does not recurse on nodes already
        seen. [`Marks m] uses the collection of marks [m] to store the
        cache of keys: the fold will modify [m]. This can be used for
        incremental folds.  *)

    type 'a node_fn = key -> step list -> 'a -> 'a Lwt.t
    (** The type for {!fold}'s [pre] and [post] parameters. *)

    val fold :
      ?force:'a force ->
      ?uniq:uniq ->
      ?pre:'a node_fn ->
      ?post:'a node_fn ->
      (key -> contents -> 'a -> 'a Lwt.t) ->
      tree ->
      'a ->
      'a Lwt.t
    (** [fold f t acc] folds [f] over [t]'s leafs.

        For every node [n], ui [n] is a leaf node, call [f path n]. Otherwise:

        {ul
          {- Call [pre path n]. By default [pre] is the identity;}
          {- Recursively call [fold] on each children, in lexicographic order;}
          {- Call [post path n]; By default [post] is the identity.}}

        See {!force} for details about the [force] parameters. By default
        it is [`True].

        See {!uniq} for details about the [uniq] parameters. By default
        it is [`False].
    *)

    (** {1 Stats} *)

    type stats = {
      nodes : int;  (** Number of node. *)
      leafs : int;  (** Number of leafs. *)
      skips : int;  (** Number of lazy nodes. *)
      depth : int;  (** Maximal depth. *)
      width : int;  (** Maximal width. *)
    }
    (** The type for tree stats. *)

    val pp_stats : stats Fmt.t
    (** [pp_stats] is the pretty printer for tree statistics. *)

    val stats : ?force:bool -> tree -> stats Lwt.t
    (** [stats ~force t] are [t]'s statistics. If [force] is true,
        this will force the reading of lazy nodes. By default it is
        [false]. *)

    (** {1 Concrete Trees} *)

    type concrete =
      [ `Tree of (step * concrete) list | `Contents of contents * metadata ]
    (** The type for concrete trees. *)

    val of_concrete : concrete -> tree
    (** [of_concrete c] is the subtree equivalent to the concrete tree
        [c]. *)

    val to_concrete : tree -> concrete Lwt.t
    (** [to_concrete t] is the concrete tree equivalent to the subtree
        [t]. *)

    (** {1 Caches} *)

    val clear : ?depth:int -> tree -> unit
    (** [clear ?depth t] clears all the cache in the tree [t] for
        subtrees with a depth higher than [depth]. If [depth] is not
        set, all the subtrees are cleared. *)

    (** {1 Performance counters} *)

    type counters = {
      mutable contents_hash : int;
      mutable contents_find : int;
      mutable contents_add : int;
      mutable node_hash : int;
      mutable node_mem : int;
      mutable node_add : int;
      mutable node_find : int;
      mutable node_val_v : int;
      mutable node_val_find : int;
      mutable node_val_list : int;
    }

    val counters : unit -> counters

    val dump_counters : unit Fmt.t

    val reset_counters : unit -> unit

    val inspect : tree -> [ `Contents | `Node of [ `Map | `Hash | `Value ] ]

    (** {1 Import/Export} *)

    val hash : tree -> hash
    (** [hash r c] it [c]'s hash in the repository [r]. *)

    val of_hash : repo -> hash -> tree option Lwt.t
    (** [of_hash r h] is the the tree object in [r] having [h] as
        hash, or [None] is no such tree object exists. *)

    val shallow : repo -> hash -> tree
    (** [shallow r h] is the shallow tree object with the hash [h]. No
        check is performed to verify if [h] actually exists in [r]. *)
  end

  (** {1 Reads} *)

  val kind : t -> key -> [ `Contents | `Node ] option Lwt.t
  (** [kind] is {!Tree.kind} applied to [t]'s root tree. *)

  val list : t -> key -> (step * [ `Contents | `Node ]) list Lwt.t
  (** [list t] is {!Tree.list} applied to [t]'s root tree. *)

  val mem : t -> key -> bool Lwt.t
  (** [mem t] is {!Tree.mem} applied to [t]'s root tree. *)

  val mem_tree : t -> key -> bool Lwt.t
  (** [mem_tree t] is {!Tree.mem_tree} applied to [t]'s root tree. *)

  val find_all : t -> key -> (contents * metadata) option Lwt.t
  (** [find_all t] is {!Tree.find_all} applied to [t]'s root tree. *)

  val find : t -> key -> contents option Lwt.t
  (** [find t] is {!Tree.find} applied to [t]'s root tree. *)

  val get_all : t -> key -> (contents * metadata) Lwt.t
  (** [get_all t] is {!Tree.get_all} applied on [t]'s root tree. *)

  val get : t -> key -> contents Lwt.t
  (** [get t] is {!Tree.get} applied to [t]'s root tree. *)

  val find_tree : t -> key -> tree option Lwt.t
  (** [find_tree t] is {!Tree.find_tree} applied to [t]'s root
      tree. *)

  val get_tree : t -> key -> tree Lwt.t
  (** [get_tree t k] is {!Tree.get_tree} applied to [t]'s root
      tree. *)

  val hash : t -> key -> hash option Lwt.t
  (** [hash t k] *)

  (** {1 Udpates} *)

  type write_error =
    [ Merge.conflict | `Too_many_retries of int | `Test_was of tree option ]
  (** The type for write errors.

      {ul
      {- Merge conflict. }
      {- Concurrent transactions are competing to get the current
         operation committed and too many attemps have been tried
         (livelock). }
      {- A "test and set" operation has failed and the current value
         is [v] instead of the one we were waiting for. }}
  *)

  val set :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    contents ->
    (unit, write_error) result Lwt.t
  (** [set t k ~info v] sets [k] to the value [v] in [t]. Discard any
     previous results but ensure that no operation is lost in the
     history.

     This function always uses {!Metadata.default} as metadata.
     Use {!set_tree} with `[Contents (c, m)] for different ones.

     The result is [Error `Too_many_retries] if the concurrent
     operations do not allow the operation to commit to the underlying
     storage layer (livelock).  *)

  val set_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    contents ->
    unit Lwt.t
  (** [set_exn] is like {!set} but raise [Failure _] instead
      of using a result type. *)

  val set_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    tree ->
    (unit, write_error) result Lwt.t
  (** [set_tree] is like {!set} but for trees. *)

  val set_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    tree ->
    unit Lwt.t
  (** [set_tree] is like {!set_exn} but for trees. *)

  val remove :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    (unit, write_error) result Lwt.t
  (** [remove t ~info k] remove any bindings to [k] in [t].

      The result is [Error `Too_many_retries] if the concurrent
     operations do not allow the operation to commit to the underlying
     storage layer (livelock). *)

  val remove_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    unit Lwt.t
  (** [remove_exn] is like {!remove} but raise [Failure _] instead of
     a using result type. *)

  val test_and_set :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    test:contents option ->
    set:contents option ->
    (unit, write_error) result Lwt.t
  (** [test_and_set ~test ~set] is like {!set} but it atomically
     checks that the tree is [test] before modifying it to [set].

     This function always uses {!Metadata.default} as metadata.
     Use {!test_and_set_tree} with `[Contents (c, m)] for different ones.

     The result is [Error (`Test t)] if the current tree is [t]
     instead of [test].

     The result is [Error `Too_many_retries] if the concurrent
     operations do not allow the operation to commit to the underlying
     storage layer (livelock). *)

  val test_and_set_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    test:contents option ->
    set:contents option ->
    unit Lwt.t
  (** [test_and_set_exn] is like {!test_and_set} but raise [Failure _]
     instead of using a result type. *)

  val test_and_set_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    test:tree option ->
    set:tree option ->
    (unit, write_error) result Lwt.t
  (** [test_and_set_tree] is like {!test_and_set} but for trees. *)

  val test_and_set_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    test:tree option ->
    set:tree option ->
    unit Lwt.t
  (** [test_and_set_tree_exn] is like {!test_and_set_exn} but for
     trees. *)

  val merge :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t ->
    key ->
    contents option ->
    (unit, write_error) result Lwt.t
  (** [merge ~old] is like {!set} but merge the current tree
     and the new tree using [old] as ancestor in case of conflicts.

      This function always uses {!Metadata.default} as metadata.
      Use {!merge_tree} with `[Contents (c, m)] for different ones.

      The result is [Error (`Conflict c)] if the merge failed with the
      conflict [c].

      The result is [Error `Too_many_retries] if the concurrent
     operations do not allow the operation to commit to the underlying
     storage layer (livelock). *)

  val merge_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t ->
    key ->
    contents option ->
    unit Lwt.t
  (** [merge_exn] is like {!merge} but raise [Failure _] instead of
     using a result type. *)

  val merge_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option ->
    t ->
    key ->
    tree option ->
    (unit, write_error) result Lwt.t
  (** [merge_tree] is like {!merge_tree} but for trees. *)

  val merge_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option ->
    t ->
    key ->
    tree option ->
    unit Lwt.t
  (** [merge_tree] is like {!merge_tree} but for trees. *)

  val with_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[ `Set | `Test_and_set | `Merge ] ->
    info:Info.f ->
    t ->
    key ->
    (tree option -> tree option Lwt.t) ->
    (unit, write_error) result Lwt.t
  (** [with_tree t k ~info f] replaces {i atomically} the subtree [v]
      under [k] in the store [t] by the contents of the tree [f v],
      using the commit info [info ()].

      If [v = f v] and [allow_empty] is unset (default) then, the
      operation is a no-op.

      If [v != f v] and no other changes happen concurrently, [f v]
      becomes the new subtree under [k]. If other changes happen
      concurrently to that operations, the semantics depend on the
      value of [strategy]:

      {ul
      {- if [strategy = `Set], use {!set} and discard any concurrent
         updates to [k]. }
      {- if [strategy = `Test_and_set] (default), use {!test_and_set}
         and ensure that no concurrent operations are updating [k]. }
      {- if [strategy = `Merge], use {!merge} and ensure
         that concurrent updates and merged with the values present
         at the beginning of the transaction. }}

      {b Note:} Irmin transactions provides
      {{:https://en.wikipedia.org/wiki/Snapshot_isolation}snapshot
      isolation} guarantees: reads and writes are isolated in every
      transaction, but only write conflicts are visible on commit. *)

  val with_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[ `Set | `Test_and_set | `Merge ] ->
    info:Info.f ->
    t ->
    key ->
    (tree option -> tree option Lwt.t) ->
    unit Lwt.t
  (** [with_tree_exn] is like {!with_tree} but raise [Failure _]
     instead of using a return type. *)

  (** {1 Clones} *)

  val clone : src:t -> dst:branch -> t Lwt.t
  (** [clone ~src ~dst] makes [dst] points to [Head.get src]. [dst] is
      created if needed. Remove the current contents en [dst] if [src]
      is {!empty}. *)

  (** {1 Watches} *)

  type watch
  (** The type for store watches. *)

  val watch : t -> ?init:commit -> (commit diff -> unit Lwt.t) -> watch Lwt.t
  (** [watch t f] calls [f] every time the contents of [t]'s head is
      updated.

      {b Note:} even if [f] might skip some head updates, it will
      never be called concurrently: all consecutive calls to [f] are
      done in sequence, so we ensure that the previous one ended
      before calling the next one. *)

  val watch_key :
    t ->
    key ->
    ?init:commit ->
    ((commit * tree) diff -> unit Lwt.t) ->
    watch Lwt.t
  (** [watch_key t key f] calls [f] every time the [key]'s value is
      added, removed or updated. If the current branch is deleted,
      no signal is sent to the watcher. *)

  val unwatch : watch -> unit Lwt.t
  (** [unwatch w] disable [w]. Return once the [w] is fully
      disabled. *)

  (** {1 Merges and Common Ancestors.} *)

  type 'a merge =
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    'a ->
    (unit, Merge.conflict) result Lwt.t
  (** The type for merge functions. *)

  val merge_into : into:t -> t merge
  (** [merge_into ~into i t] merges [t]'s current branch into [x]'s
      current branch using the info [i]. After that operation, the two
      stores are still independent. Similar to [git merge <branch>]. *)

  val merge_with_branch : t -> branch merge
  (** Same as {!merge} but with a branch ID. *)

  val merge_with_commit : t -> commit merge
  (** Same as {!merge} but with a commit ID. *)

  val lcas :
    ?max_depth:int -> ?n:int -> t -> t -> (commit list, lca_error) result Lwt.t
  (** [lca ?max_depth ?n msg t1 t2] returns the collection of least
      common ancestors between the heads of [t1] and [t2] branches.

      {ul
      {- [max_depth] is the maximum depth of the exploration (default
      is [max_int]). Return [Error `Max_depth_reached] if this depth
      is exceeded.}
      {- [n] is the maximum expected number of lcas. Stop the
      exploration as soon as [n] lcas are found. Return
      [Error `Too_many_lcas] if more [lcas] are found. }
      }
  *)

  val lcas_with_branch :
    t ->
    ?max_depth:int ->
    ?n:int ->
    branch ->
    (commit list, lca_error) result Lwt.t
  (** Same as {!lcas} but takes a branch ID as argument. *)

  val lcas_with_commit :
    t ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    (commit list, lca_error) result Lwt.t
  (** Same as {!lcas} but takes a commit ID as argument. *)

  (** {1 History} *)

  module History : Graph.Sig.P with type V.t = commit
  (** An history is a DAG of heads. *)

  val history :
    ?depth:int -> ?min:commit list -> ?max:commit list -> t -> History.t Lwt.t
  (** [history ?depth ?min ?max t] is a view of the history of the
      store [t], of depth at most [depth], starting from the [max]
      (or from the [t]'s head if the list of heads is empty) and
      stopping at [min] if specified. *)

  val last_modified : ?depth:int -> ?n:int -> t -> key -> commit list Lwt.t
  (** [last_modified ?number c k] is the list of the last [number] commits
      that modified [key], in ascending order of date. [depth] is the maximum
      depth to be explored in the commit graph, if any. Default value for
      [number] is 1. *)

  (** Manipulate branches. *)
  module Branch : sig
    (** {1 Branch Store}

        Manipulate relations between {{!branch}branches} and
        {{!commit}commits}. *)

    val mem : repo -> branch -> bool Lwt.t
    (** [mem r b] is true iff [b] is present in [r]. *)

    val find : repo -> branch -> commit option Lwt.t
    (** [find r b] is [Some c] iff [c] is bound to [b] in [t]. It is
        [None] if [b] is not present in [t]. *)

    val get : repo -> branch -> commit Lwt.t
    (** [get t b] is similar to {!find} but raise [Invalid_argument]
        if [b] is not present in [t]. *)

    val set : repo -> branch -> commit -> unit Lwt.t
    (** [set t b c] bounds [c] to [b] in [t]. *)

    val remove : repo -> branch -> unit Lwt.t
    (** [remove t b] removes [b] from [t]. *)

    val list : repo -> branch list Lwt.t
    (** [list t] is the list of branches present in [t]. *)

    val watch :
      repo ->
      branch ->
      ?init:commit ->
      (commit diff -> unit Lwt.t) ->
      watch Lwt.t
    (** [watch t b f] calls [f] on every change in [b]. *)

    val watch_all :
      repo ->
      ?init:(branch * commit) list ->
      (branch -> commit diff -> unit Lwt.t) ->
      watch Lwt.t
    (** [watch_all t f] calls [f] on every branch-related change in
        [t], including creation/deletion events. *)

    include Branch.S with type t = branch
    (** Base functions for branches. *)
  end

  (** [Key] provides base functions for the stores's paths. *)
  module Key : Path.S with type t = key and type step = step

  module Metadata : Metadata.S with type t = metadata
  (** [Metadata] provides base functions for node metadata. *)

  (** {1 Value Types} *)

  val step_t : step Type.t
  (** [step_t] is the value type for {!step}. *)

  val key_t : key Type.t
  (** [key_t] is the value type for {!key}. *)

  val metadata_t : metadata Type.t
  (** [metadata_t] is the value type for {!metadata}. *)

  val contents_t : contents Type.t
  (** [contents_t] is the value type for {!contents}. *)

  val node_t : node Type.t
  (** [node_t] is the value type for {!node}. *)

  val tree_t : tree Type.t
  (** [tree_t] is the value type for {!tree}. *)

  val commit_t : repo -> commit Type.t
  (** [commit_t r] is the value type for {!commit}. *)

  val branch_t : branch Type.t
  (** [branch_t] is the value type for {!branch}. *)

  val slice_t : slice Type.t
  (** [slice_t] is the value type for {!slice}. *)

  val kind_t : [ `Node | `Contents ] Type.t
  (** [kind_t] is the value type for values returned by {!kind}. *)

  val lca_error_t : lca_error Type.t
  (** [lca_error_t] is the value type for {!lca_error}. *)

  val ff_error_t : ff_error Type.t
  (** [ff_error_t] is the value type for {!ff_error}. *)

  val write_error_t : write_error Type.t
  (** [write_error_t] is the value type for {!write_error}. *)

  (** Private functions, which might be used by the backends. *)
  module Private : sig
    include
      Private.S
        with type Contents.value = contents
         and module Node.Path = Key
         and type Hash.t = Hash.t
         and type Node.Metadata.t = metadata
         and type Branch.key = branch
         and type Slice.t = slice
         and type Repo.t = repo
  end

  type remote +=
    | E of Private.Sync.endpoint
          (** Extend the [remote] type with [endpoint]. *)

  (** {2 Converters to private types} *)

  val to_private_node : node -> Private.Node.value option Lwt.t
  (** [to_private_node n] is the private node objects built using [n].
     The operation can fetch the database to read an object as [n]
     could be represented as a hash. The result is [None] iff that
     hash doesn't exist in the database. *)

  val of_private_node : repo -> Private.Node.value -> node
  (** [of_private_node r n] is the node build from the private node
     object [n]. *)

  val to_private_commit : commit -> Private.Commit.value
  (** [to_private_commit c] is the private commit object associated
     with the commit [c]. *)

  val of_private_commit : repo -> Private.Commit.value -> commit
  (** [of_private_commit r c] is the commit associated with the
     private commit object [c]. *)

  val save_contents : [> `Write ] Private.Contents.t -> contents -> hash Lwt.t
  (** Save a content into the database *)

  val save_tree :
    ?clear:bool ->
    repo ->
    [> `Write ] Private.Contents.t ->
    [ `Read | `Write ] Private.Node.t ->
    tree ->
    hash Lwt.t
  (** Save a tree into the database. Does not do any reads. If
        [clear] is set (it is by default), the tree cache will be
        cleared after the save. *)
end

(** [Json_tree] is used to project JSON values onto trees. Instead of the entire
    object being stored under one key, it is split across several keys starting
    at the specified root key.  *)
module Json_tree (Store : S with type contents = Contents.json) : sig
  include Contents.S with type t = Contents.json

  val to_concrete_tree : t -> Store.Tree.concrete

  val of_concrete_tree : Store.Tree.concrete -> t

  val get_tree : Store.tree -> Store.key -> t Lwt.t
  (** Extract a [json] value from tree at the given key. *)

  val set_tree : Store.tree -> Store.key -> t -> Store.tree Lwt.t
  (** Project a [json] value onto a tree at the given key. *)

  val get : Store.t -> Store.key -> t Lwt.t
  (** Extract a [json] value from a store at the given key. *)

  val set : Store.t -> Store.key -> t -> info:Info.f -> unit Lwt.t
  (** Project a [json] value onto a store at the given key. *)
end

(** [S_MAKER] is the signature exposed by any backend providing {!S}
    implementations. [M] is the implementation of user-defined
    metadata, [C] is the one for user-defined contents, [B] is the
    implementation for branches and [H] is the implementation for
    object (blobs, trees, commits) hashes. It does not use any native
    synchronization primitives. *)
module type S_MAKER = functor
  (M : Metadata.S)
  (C : Contents.S)
  (P : Path.S)
  (B : Branch.S)
  (H : Hash.S)
  ->
  S
    with type key = P.t
     and type step = P.step
     and type metadata = M.t
     and type contents = C.t
     and type branch = B.t
     and type hash = H.t

(** [KV] is similar to {!S} but chooses sensible implementations for
    path and branch. *)
module type KV =
  S with type key = string list and type step = string and type branch = string

module type KV_MAKER = functor (C : Contents.S) -> KV with type contents = C.t
(** [KV_MAKER] is like {!S_MAKER} but where everything except the
    contents is replaced by sensible default implementations. *)

(** {2 Synchronization} *)

val remote_store : (module S with type t = 'a) -> 'a -> remote
(** [remote_store t] is the remote corresponding to the local store
    [t]. Synchronization is done by importing and exporting store
    {{!BC.slice}slices}, so this is usually much slower than native
    synchronization using {!Store.remote} but it works for all
    backends. *)

(** [SYNC] provides functions to synchronize an Irmin store with local
    and remote Irmin stores. *)
module type SYNC = sig
  (** {1 Native Synchronization} *)

  type db
  (** Type type for store handles. *)

  type commit
  (** The type for store heads. *)

  type status = [ `Empty | `Head of commit ]
  (** The type for remote status. *)

  val status_t : db -> status Type.t
  (** [status_t db] is the value type for {!status} of remote [db]. *)

  val pp_status : status Fmt.t
  (** [pp_status] pretty-prints return statuses. *)

  val fetch :
    db -> ?depth:int -> remote -> (status, [ `Msg of string ]) result Lwt.t
  (** [fetch t ?depth r] populate the local store [t] with objects for
      the remote store [r], using [t]'s current branch. The [depth]
      parameter limits the history depth. Return [`Empty] if either the
      local or remote store do not have a valid head. *)

  val fetch_exn : db -> ?depth:int -> remote -> status Lwt.t
  (** Same as {!fetch} but raise [Invalid_argument] if either the
      local or remote store do not have a valid head. *)

  type pull_error = [ `Msg of string | Merge.conflict ]
  (** The type for pull errors. *)

  val pp_pull_error : pull_error Fmt.t
  (** [pp_push_error] pretty-prints pull errors. *)

  val pull :
    db ->
    ?depth:int ->
    remote ->
    [ `Merge of Info.f | `Set ] ->
    (status, pull_error) result Lwt.t
  (** [pull t ?depth r s] is similar to {{!Sync.fetch}fetch} but it
      also updates [t]'s current branch. [s] is the update strategy:

      {ul
      {- [`Merge] uses [Head.merge]. Can return a conflict.}
      {- [`Set] uses [S.Head.set].}
      } *)

  val pull_exn :
    db -> ?depth:int -> remote -> [ `Merge of Info.f | `Set ] -> status Lwt.t
  (** Same as {!pull} but raise [Invalid_arg] in case of conflict. *)

  type push_error = [ `Msg of string | `Detached_head ]
  (** The type for push errors. *)

  val pp_push_error : push_error Fmt.t
  (** [pp_push_error] pretty-prints push errors. *)

  val push : db -> ?depth:int -> remote -> (status, push_error) result Lwt.t
  (** [push t ?depth r] populates the remote store [r] with objects
      from the current store [t], using [t]'s current branch. If [b]
      is [t]'s current branch, [push] also updates the head of [b] in
      [r] to be the same as in [t].

      {b Note:} {e Git} semantics is to update [b] only if the new
      head if more recent. This is not the case in {e Irmin}. *)

  val push_exn : db -> ?depth:int -> remote -> status Lwt.t
  (** Same as {!push} but raise [Invalid_argument] if an error
      happens. *)
end

(** The default [Sync] implementation. *)
module Sync (S : S) : SYNC with type db = S.t and type commit = S.commit

(** {1:examples Examples}

    These examples are in the [examples] directory of the
    distribution.

    {3 Syncing with a remote}

    A simple synchronization example, using the
    {{!Irmin_unix.Git}Git} backend and the {!Sync} helpers. The
    code clones a fresh repository if the repository does not exist
    locally, otherwise it performs a fetch: in this case, only
    the missing contents are downloaded.

{[
open Lwt.Infix

module S = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module Sync = Irmin.Sync(S)
let config = Irmin_git.config "/tmp/test"

let upstream =
  if Array.length Sys.argv = 2 then (Uri.of_string (Store.remote Sys.argv.(1)))
  else (Printf.eprintf "Usage: sync [uri]\n%!"; exit 1)

let test () =
  S.Repo.v config >>= S.master
  >>= fun t  -> Sync.pull_exn t upstream `Set
  >>= fun () -> S.get t ["README.md"]
  >|= fun r  -> Printf.printf "%s\n%!" r

let () = Lwt_main.run (test ())
]}

    {3 Mergeable logs}

    We will demonstrate the use of custom merge operators by
    defining mergeable debug log files. We first define a log entry
    as a pair of a timestamp and a message, using the combinator
    exposed by {!Irmin.Type}:

{[
module Entry : sig
  include Irmin.Type.S
  val v: string -> t
  val timestamp: t -> int
end = struct

  type t = { timestamp: int; message : string; }

  let compare x y = compare x.timestamp y.timestamp

  let time = ref 0

  let v message =
    incr time;
    { timestamp = !time; message }

  let timestamp t = t.timestamp

  let pp ppf { timestamp; message } =
    Fmt.pf ppf "%04d: %s" timestamp message

  let of_string str =
    match String.split_on_char '\t' str with
    | [] -> Error (`Msg ("invalid entry: " ^ str))
    | ts :: msg_sects ->
      let message = String.concat "\t" msg_sects in
      try Ok { timestamp = int_of_string ts; message }
      with Failure e -> Error (`Msg e)

  let t =
    let open Irmin.Type in
    record "entry" (fun t32 message -> { timestamp = Int32.to_int t32; message })
    |+ field "timestamp" int32  (fun t -> Int32.of_int t.timestamp)
    |+ field "message"   string (fun t -> t.message)
    |> sealr

  let t = Irmin.Type.like ~cli:(pp, of_string) ~compare t

end
]}

    A log file is a list of entries (one per line), ordered by
    decreasing order of timestamps. The 3-way [merge] operator for log
    files concatenates and sorts the new entries and prepend them
    to the common ancestor's ones.

{[
(* A log file *)
module Log: sig
  include Irmin.Contents.S
  val add: t -> Entry.t -> t
  val empty: t
end = struct

  type t = Entry.t list

  let empty = []

  let pp ppf l = List.iter (Fmt.pf ppf "%a\n" Entry.pp ) (List.rev l)

  let of_string str =
    let lines = String.cuts ~empty:false ~sep:"\n" str in
    try
      List.fold_left (fun acc l ->
          match Entry.of_string l with
          | Ok x           -> x :: acc
          | Error (`Msg e) -> failwith e
        ) [] lines
      |> fun l -> Ok l
    with Failure e ->
      Error (`Msg e)

  let t = Irmin.Type.(list Entry.t)
  let t = Irmin.Type.like' ~cli:(pp, of_string) t

  let timestamp = function
    | [] -> 0
    | e :: _ -> Entry.timestamp e

  let newer_than timestamp file =
    let rec aux acc = function
      | [] -> List.rev acc
      | h:: _ when Entry.timestamp h <= timestamp -> List.rev acc
      | h::t -> aux (h::acc) t
    in
    aux [] file

  let merge ~old t1 t2 =
    let open Irmin.Merge.Infix in
    old () >>=* fun old ->
    let old = match old with None -> [] | Some o -> o in
    let ts = timestamp old in
    let t1 = newer_than ts t1 in
    let t2 = newer_than ts t2 in
    let t3 = List.sort Entry.compare (List.rev_append t1 t2) in
    Irmin.Merge.ok (List.rev_append t3 old)

  let merge = Irmin.Merge.(option (v t merge))

  let add t e = e :: t

end ]}

    {b Note:} The serialisation primitives used in that example are
    not very efficient in this case as they parse the file
    every time. For real usage, you would write buffered versions of
    [Log.pp] and [Log.of_string].

    To persist the log file on disk, we need to choose a backend. We
    show here how to use the on-disk [Git] backend on Unix.

{[
  (* Build an Irmin store containing log files. *)
  module S = Irmin_unix.Git.FS.KV(Log)

  (* Set-up the local configuration of the Git repository. *)
  let config = Irmin_git.config ~bare:true "/tmp/irmin/test"

  (* Set-up the commit info function *)
  let info fmt = Irmin_unix.info ~author:"logger" fmt
]}

  We can now define a toy example to use our mergeable log files.

{[
  open Lwt.Infix

  (* Name of the log file. *)
  let file = [ "local"; "debug" ]

  (* Read the entire log file. *)
  let read_file t =
    S.find t file >|= function
    | None   -> []
    | Some l -> l

  (* Persist a new entry in the log. *)
  let log t fmt =
    Fmt.kstrf (fun message ->
        read_file t >>= fun logs ->
        let logs = Log.add logs (Entry.v message) in
        S.set t (info "Adding a new entry") file logs
      ) fmt

  let () =
    Lwt_main.run begin
      S.Repo.v config >>= S.master
      >>= fun t  -> log t "Adding a new log entry"
      >>= fun () -> Irmin.clone_force ~src:t ~dst:"x"
      >>= fun x  -> log x "Adding new stuff to x"
      >>= fun () -> log x "Adding more stuff to x"
      >>= fun () -> log x "More. Stuff. To x."
      >>= fun () -> log t "I can add stuff on t also"
      >>= fun () -> log t "Yes. On t!"
      >>= fun () -> S.merge (info "Merging x into t") x ~into:t
      >|= function Ok () -> () | Error _ -> failwith "merge conflict!"
    end
]}

*)

(** {1 Helpers} *)

(** [Dot] provides functions to export a store to the Graphviz `dot`
    format. *)
module Dot (S : S) : sig
  (** {1 Dot Export} *)

  val output_buffer :
    S.t ->
    ?html:bool ->
    ?depth:int ->
    ?full:bool ->
    date:(int64 -> string) ->
    Buffer.t ->
    unit Lwt.t
  (** [output_buffer t ?html ?depth ?full buf] outputs the Graphviz
        representation of [t] in the buffer [buf].

        [html] (default is false) enables HTML labels.

        [depth] is used to limit the depth of the commit history. [None]
        here means no limitation.

        If [full] is set (default is not) the full graph, including the
        commits, nodes and contents, is exported, otherwise it is the
        commit history graph only. *)
end

(** {1:backend Backends}

    API to create new Irmin backends. A backend is an implementation
    exposing either a concrete implementation of {!S} or a functor
    providing {!S} once applied.

    There are two ways to create a concrete {!Irmin.S} implementation:

    {ul
    {- {!Make} creates a store where all the objects are stored in the
    same store, using the same internal keys format and a custom binary
    format based on {{:https://github.com/janestreet/bin_prot}bin_prot},
    with no native synchronization primitives: it is usually what is
    needed to quickly create a new backend.}
    {- {!Make_ext} creates a store with a {e deep} embedding of each
    of the internal stores into separate store, with total control over
    the binary format and using the native synchronization protocols
    when available.}
    }
*)

(** [APPEND_ONLY_STORE_MAKER] is the signature exposed by
    append-only store backends. [K] is the implementation of keys
    and [V] is the implementation of values. *)
module type APPEND_ONLY_STORE_MAKER = functor (K : Type.S) (V : Type.S) -> sig
  include APPEND_ONLY_STORE with type key = K.t and type value = V.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t
  (** [batch t f] applies the writes in [f] in a separate batch. The
     exact guarantees depends on the backends. *)

  val v : config -> [ `Read ] t Lwt.t
  (** [v config] is a function returning fresh store handles, with the
      configuration [config], which is provided by the backend. *)

  val close : 'a t -> unit Lwt.t
  (** [close t] frees up all the resources associated to [t]. Any
      operations run on a closed store will raise [Closed].*)
end

(** [CONTENT_ADDRESSABLE_STOREMAKER] is the signature exposed by
    content-addressable store backends. [K] is the implementation of keys
    and [V] is the implementation of values. *)
module type CONTENT_ADDRESSABLE_STORE_MAKER = functor
  (K : Hash.S)
  (V : Type.S)
  -> sig
  include CONTENT_ADDRESSABLE_STORE with type key = K.t and type value = V.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t
  (** [batch t f] applies the writes in [f] in a separate batch. The
     exact guarantees depends on the backends. *)

  val v : config -> [ `Read ] t Lwt.t
  (** [v config] is a function returning fresh store handles, with the
      configuration [config], which is provided by the backend. *)

  val close : 'a t -> unit Lwt.t
  (** [close t] frees up all the resources associated to [t]. Any
      operations run on a closed store will raise [Closed].*)
end

module Content_addressable
    (S : APPEND_ONLY_STORE_MAKER)
    (K : Hash.S)
    (V : Type.S) : sig
  include
    CONTENT_ADDRESSABLE_STORE
      with type 'a t = 'a S(K)(V).t
       and type key = K.t
       and type value = V.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t
  (** [batch t f] applies the writes in [f] in a separate batch. The
      exact guarantees depends on the backends. *)

  val v : config -> [ `Read ] t Lwt.t
  (** [v config] is a function returning fresh store handles, with the
      configuration [config], which is provided by the backend. *)

  val close : 'a t -> unit Lwt.t
  (** [close t] frees up all the resources associated to [t]. Any
      operations run on a closed store will raise [Closed]. *)
end

(** [ATOMIC_WRITE_STORE_MAKER] is the signature exposed by atomic-write
    store backends. [K] is the implementation of keys and [V] is the
    implementation of values.*)
module type ATOMIC_WRITE_STORE_MAKER = functor (K : Type.S) (V : Type.S) -> sig
  include ATOMIC_WRITE_STORE with type key = K.t and type value = V.t

  val v : config -> t Lwt.t
  (** [v config] is a function returning fresh store handles, with the
      configuration [config], which is provided by the backend. *)

  val close : t -> unit Lwt.t
  (** [close t] frees up all the resources associated to [t]. Any
      operations run on a closed store will raise [Closed]. *)
end

module Make
    (CA : CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW : ATOMIC_WRITE_STORE_MAKER) : S_MAKER
(** Simple store creator. Use the same type of all of the internal
    keys and store all the values in the same store. *)

module Make_ext
    (CA : CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW : ATOMIC_WRITE_STORE_MAKER)
    (Metadata : Metadata.S)
    (Contents : Contents.S)
    (Path : Path.S)
    (Branch : Branch.S)
    (Hash : Hash.S)
    (Node : Private.Node.S
              with type metadata = Metadata.t
               and type hash = Hash.t
               and type step = Path.step)
    (Commit : Private.Commit.S with type hash = Hash.t) :
  S
    with type key = Path.t
     and type contents = Contents.t
     and type branch = Branch.t
     and type hash = Hash.t
     and type step = Path.step
     and type metadata = Metadata.t
     and type Key.step = Path.step

(** Advanced store creator. *)
module Of_private (P : Private.S) :
  S
    with type key = P.Node.Path.t
     and type contents = P.Contents.value
     and type branch = P.Branch.key
     and type hash = P.Hash.t
     and type step = P.Node.Path.step
     and type metadata = P.Node.Val.metadata
     and type Key.step = P.Node.Path.step
     and type repo = P.Repo.t
     and type slice = P.Slice.t
     and module Private = P
