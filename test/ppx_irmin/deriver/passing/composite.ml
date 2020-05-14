(* Tests of the composite type combinators *)
type test_list1 = string list [@@deriving irmin]

type test_list2 = int32 list list list [@@deriving irmin]

type test_array = bool array [@@deriving irmin]

type test_option = unit option [@@deriving irmin]

type test_pair = string * int32 [@@deriving irmin]

type test_triple = string * int32 * bool [@@deriving irmin]

type test_result = (int32, string) result [@@deriving irmin]

let (_ : test_list1 Irmin.Type.t) = test_list1_t

let (_ : test_list2 Irmin.Type.t) = test_list2_t

let (_ : test_array Irmin.Type.t) = test_array_t

let (_ : test_option Irmin.Type.t) = test_option_t

let (_ : test_pair Irmin.Type.t) = test_pair_t

let (_ : test_triple Irmin.Type.t) = test_triple_t

let (_ : test_result Irmin.Type.t) = test_result_t
