(* Tests of the composite type combinators *)
type test_list1 = string list [@@deriving irmin]

type test_list2 = int32 list list list [@@deriving irmin]

type test_array = bool array [@@deriving irmin]

type test_option = unit option [@@deriving irmin]

type test_pair = string * int32 [@@deriving irmin]

type test_triple = string * int32 * bool [@@deriving irmin]

type test_result = (int32, string) result [@@deriving irmin]
