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

module Lens = Optics.Lens (Monad.Identity)
module Prism = Optics.Prism (Monad.Identity)

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

and ('a, 'b) map = { x : 'a t; f : 'a -> 'b; g : 'b -> 'a; mwit : 'b Witness.t }

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
  | Fields : ('a, 'b, _, _) fields * 'b -> 'a fields_and_constr

(** We need to maintain two kinds of type-level difference list. The type
    parameters [('record, 'constr, 'lens, 'lens_nil)] have the following
    meanings:

    - ['record] : the type of the record under construction.

    - ['constr] : function type consuming the fields that have been described so
      far and returning ['record] (that is, a list with [(::) = (->)] and
      [(\[\]) = 'record]). This gradually builds the type of the constructor
      function, such that the variant may be sealed when ['constr] is equal to
      the supplied constructor type.

    - ['lens] : list of lenses for the fields passed so far with [(::) = ( * )]
      and [(\[\]) = 'lens_nil].

    - ['lens_nil] : the nil-variable of ['lens].

    For example, given a record of type [foo] with fields of type [int],
    [string] and [bool] (two of which have been passed) the type parameters are
    as follows:

    {[
      ('a, 'b, 'c, 'd)  =  (foo,     ->         ,          *      , 'd)
                                    /  \                 /   \
                                   /    \               /     \
                                  /      \             /       \
                                int      ->     (foo, int)      *
                                        /  \     Lens.mono    /   \
                                       /    \                /     \
                                      /      \              /       \
                                   string    foo     (foo, string)  'd
                                                       Lens.mono
    ]} *)

and ('record, 'constr, 'lenses, 'lens_nil) fields =
  | Fields_nil : ('record, 'record, 'lens_nil, 'lens_nil) fields
  | Fields_cons :
      ('record, 'field) field * ('record, 'constr, 'lenses, 'lens_nil) fields
      -> ( 'record,
           'field -> 'constr,
           ('record, 'field) Lens.mono * 'lenses,
           'lens_nil )
         fields

and ('a, 'b) field = { fname : string; ftype : 'b t; fget : 'a -> 'b }

and 'a variant = {
  vwit : 'a Witness.t;
  vname : string;
  vcases : 'a a_case array;
  vget : 'a -> 'a case_v;
}

(* Cases are built without knowing what the tags are *)
and ('v, 'case, 'constr) case = int -> ('v, 'case, 'constr) case_with_tag

(* | C0_tagless : (int -> 'v case0) -> ('v, unit) case
 * | C1_tagless : (int -> ('v, 'case) case1) -> ('v, 'case) case *)
and ('v, 'case, 'constr) case_with_tag =
  | C0 : 'v case0 -> ('v, unit, 'v case_v) case_with_tag
  | C1 : ('v, 'case) case1 -> ('v, 'case, 'case -> 'v case_v) case_with_tag

(** {!case_with_tag} without the ['case] parameter *)
and 'v a_case =
  | CP0 : 'v case0 -> 'v a_case
  | CP1 : ('v, 'case) case1 -> 'v a_case

and 'v case_v =
  | CV0 : 'v case0 -> 'v case_v
  | CV1 : ('v, 'case) case1 * 'case -> 'v case_v

and 'a case0 = { ctag0 : int; cname0 : string; c0 : 'a }

and ('a, 'b) case1 = {
  ctag1 : int;
  cname1 : string;
  ctype1 : 'b t;
  c1 : 'b -> 'a;
}

and ('variant, 'pat, 'pat_nil, 'prisms, 'prism_nil) cases =
  | Cases_nil : ('variant, 'pat_nil, 'pat_nil, 'prism_nil, 'prism_nil) cases
  | Cases_cons :
      ('variant, 'case, 'constr) case_with_tag
      * ('variant, 'remaining, 'pat_nil, 'prisms, 'prism_nil) cases
      -> ( 'variant,
           'constr -> 'remaining,
           'pat_nil,
           ('variant, 'case) Prism.mono * 'prisms,
           'prism_nil )
         cases

type 'a ty = 'a t

type _ a_field = Field : ('a, 'b) field -> 'a a_field

let rec fields_aux : type a b c d. (a, b, c, d) fields -> a a_field list =
  function
  | Fields_nil -> []
  | Fields_cons (h, t) -> Field h :: fields_aux t

let fields r = match r.rfields with Fields (f, _) -> fields_aux f
