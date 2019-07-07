type test_unit = unit[@@deriving irmin]
let test_unit = Irmin.Type.unit
type test_bool = bool[@@deriving irmin]
let test_bool = Irmin.Type.bool
type test_char = char[@@deriving irmin]
let test_char = Irmin.Type.char
type test_int32 = int32[@@deriving irmin]
let test_int32 = Irmin.Type.int32
type test_int64 = int64[@@deriving irmin]
let test_int64 = Irmin.Type.int64
type test_float = float[@@deriving irmin]
let test_float = Irmin.Type.float
type test_string = string[@@deriving irmin]
let test_string = Irmin.Type.string
type test_list1 = string list[@@deriving irmin]
let test_list1 = let open Irmin.Type in list string
type test_list2 = int32 list list list[@@deriving irmin]
let test_list2 = let open Irmin.Type in list (list (list int32))
type test_array = bool array[@@deriving irmin]
let test_array = let open Irmin.Type in array bool
type test_option = unit option[@@deriving irmin]
let test_option = let open Irmin.Type in option unit
type test_pair = (string * int32)[@@deriving irmin]
let test_pair = let open Irmin.Type in pair string int32
type test_triple = (string * int32 * bool)[@@deriving irmin]
let test_triple = let open Irmin.Type in triple string int32 bool
type test_result = (int32, string) result[@@deriving irmin]
let test_result = let open Irmin.Type in result int32 string
type test_variant1 = A[@@deriving irmin]
let test_variant1 = let open Irmin.Type in
  variant "test_variant1" (fun f A -> f)
  |~ case0 "A" A
(* let test_variant2 = let open Irmin.Type in
 *   variant "test_variant2" (fun a b c -> function
 *       | A -> a
 *       | B -> b
 *       | C -> c)
 *   |~ case0 "A" A
 *   |~ case0 "B" B
 *   |~ case0 "C" C *)
