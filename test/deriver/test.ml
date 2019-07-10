(* Tests of the basic types *)
type test_unit = unit [@@deriving irmin]
type test_bool = bool [@@deriving irmin]
type test_char = char [@@deriving irmin]
type test_int32 = int32 [@@deriving irmin]
type test_int64 = int64 [@@deriving irmin]
type test_float = float [@@deriving irmin]
type test_string = string [@@deriving irmin]

(* Tests of the composite type combinators *)
type test_list1 = string list [@@deriving irmin]
type test_list2 = int32 list list list [@@deriving irmin]
type test_array = bool array [@@deriving irmin]
type test_option = unit option [@@deriving irmin]
type test_pair = (string) * int32 [@@deriving irmin]
type test_triple = string * int32 * bool [@@deriving irmin]
type test_result = (int32, string) result [@@deriving irmin]

(* Miscellaneous *)
type test_alias = test_result [@@deriving irmin]
type t = test_alias
module S = struct
  type nonrec t = t list
end

(* Variants *)
type test_variant1 = A [@@deriving irmin]
type test_variant2 = A2 of int64 [@@deriving irmin]
type test_variant3 = A3 of string * test_variant2 [@@deriving irmin]
type test_variant4 =
  | A4
  | B4
  | C4 [@@deriving irmin]
type test_variant5 =
  | A5
  | B5 of string * test_variant4
  | C5 of int32 * string * unit [@@deriving irmin]
type test_variant6 =
  | Nil
  | Cons of string * test_variant6 [@@deriving irmin]

(* Records *)
type test_record1 = {
  alpha: string;
  beta: int64 list;
  gamma: test_variant5
} [@@deriving irmin]
type test_record2 = {
  the_FIRST_identifier: test_record1 option;
  the_SECOND_identifier: (string, int32) result list;
} [@@deriving irmin]

(* Tests of the arguments/attributes *)
type c = string [@@deriving irmin { name = "c_wit" }]
type d = int [@@deriving irmin { name = "witness_for_d" }]
type point_elsewhere1 = (c [@witness c_wit]) [@@deriving irmin]
type point_elsewhere2 = int * (c [@witness c_wit]) [@@deriving irmin]
type point_elsewhere3 =
  | A of int * (c [@witness c_wit])
  | B of (c [@witness c_wit]) [@@deriving irmin]
type point_elsewhere4 = {
  lorem: string;
  ipsum: (c [@witness c_wit]);
  dolor: int;
  sit: (d [@witness witness_for_d]);
} [@@deriving irmin]

(* Tests of the signature deriver *)
module type S = sig
  type t = string [@@deriving irmin]

  type my_int = int32 * t [@@deriving irmin]

  type my_variant =
    | A of (my_int, int) result
    | B of unit
    | C of string * int32 [@@deriving irmin]
end
