open Staging

module Types = struct
  type len = [ `Int | `Int8 | `Int16 | `Int32 | `Int64 | `Fixed of int ]

  type 'a pp = 'a Fmt.t

  type 'a of_string = string -> ('a, [ `Msg of string ]) result

  type 'a to_string = 'a -> string

  type 'a encode_json = Jsonm.encoder -> 'a -> unit

  type json_decoder = { mutable lexemes : Jsonm.lexeme list; d : Jsonm.decoder }

  type 'a decode_json = json_decoder -> ('a, [ `Msg of string ]) result

  type 'a bin_seq = 'a -> (string -> unit) -> unit

  type 'a pre_hash = 'a bin_seq staged

  type 'a encode_bin = 'a bin_seq staged

  type 'a decode_bin = (string -> int -> int * 'a) staged

  type 'a size_of = ('a -> int option) staged

  type 'a compare = 'a -> 'a -> int

  type 'a equal = 'a -> 'a -> bool

  type 'a short_hash = ?seed:int -> 'a -> int

  type 'a t =
    | Var : string -> 'a t
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
    | Boxed : 'a t -> 'a t

  and 'a len_v = { len : len; v : 'a t }

  and 'a custom = {
    cwit : [ `Type of 'a t | `Witness of 'a Witness.t ];
    pp : 'a pp;
    of_string : 'a of_string;
    encode_json : 'a encode_json;
    decode_json : 'a decode_json;
    short_hash : 'a short_hash;
    pre_hash : 'a encode_bin;
    compare : 'a compare;
    equal : 'a equal;
    (* boxed binary encoding *)
    encode_bin : 'a encode_bin;
    decode_bin : 'a decode_bin;
    size_of : 'a size_of;
    (* unboxed binary encoding *)
    unboxed_encode_bin : 'a encode_bin;
    unboxed_decode_bin : 'a decode_bin;
    unboxed_size_of : 'a size_of;
  }

  and ('a, 'b) map = {
    x : 'a t;
    f : 'a -> 'b;
    g : 'b -> 'a;
    mwit : 'b Witness.t;
  }

  and 'a self = { self_unroll : 'a t -> 'a t; mutable self_fix : 'a t }

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

  and 'a a_case =
    | C0 : 'a case0 -> 'a a_case
    | C1 : ('a, 'b) case1 -> 'a a_case

  and 'a case_v =
    | CV0 : 'a case0 -> 'a case_v
    | CV1 : ('a, 'b) case1 * 'b -> 'a case_v

  and 'a case0 = { ctag0 : int; cname0 : string; c0 : 'a }

  and ('a, 'b) case1 = {
    ctag1 : int;
    cname1 : string;
    ctype1 : 'b t;
    cwit1 : 'b Witness.t;
    c1 : 'b -> 'a;
  }

  type 'a ty = 'a t

  exception Unbound_type_variable of string

  type _ a_field = Field : ('a, 'b) field -> 'a a_field

  module Case_folder = struct
    type ('a, 'f) t = {
      c0 : 'a case0 -> 'f staged;
      c1 : 'b. ('a, 'b) case1 -> ('b -> 'f) staged;
    }
  end
end

module type Type_core = sig
  include module type of Types
  (** @inline *)

  val fields : 'a record -> 'a a_field list

  module Fields_folder (Acc : sig
    type ('a, 'b) t
  end) : sig
    type 'a t = {
      nil : ('a, 'a) Acc.t;
      cons : 'b 'c. ('a, 'b) field -> ('a, 'c) Acc.t -> ('a, 'b -> 'c) Acc.t;
    }

    val fold : 'a t -> ('a, 'c) fields -> ('a, 'c) Acc.t
  end

  val fold_variant : ('a, 'b) Case_folder.t -> 'a variant -> ('a -> 'b) staged

  val partial :
    ?pp:'a pp ->
    ?of_string:'a of_string ->
    ?encode_json:'a encode_json ->
    ?decode_json:'a decode_json ->
    ?short_hash:'a short_hash ->
    ?pre_hash:'a pre_hash ->
    ?compare:'a compare ->
    ?equal:'a equal ->
    ?encode_bin:'a encode_bin ->
    ?decode_bin:'a decode_bin ->
    ?size_of:'a size_of ->
    ?unboxed_encode_bin:'a encode_bin ->
    ?unboxed_decode_bin:'a decode_bin ->
    ?unboxed_size_of:'a size_of ->
    unit ->
    'a t

  module Json : sig
    type decoder = json_decoder

    val decoder :
      ?encoding:[< Jsonm.encoding ] -> [< Jsonm.src ] -> json_decoder

    val decoder_of_lexemes : Jsonm.lexeme list -> json_decoder

    val rewind : json_decoder -> Jsonm.lexeme -> unit

    val decode :
      json_decoder ->
      [> `Await | `End | `Error of Jsonm.error | `Lexeme of Jsonm.lexeme ]
  end
end
