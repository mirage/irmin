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
type test_variant1 =
  | A [@@deriving irmin]
let test_variant1 =
  let open Irmin.Type in
    ((variant "test_variant1" (fun a -> function | A -> a)) |~ (case0 "A" A))
      |> sealv
type test_variant2 =
  | A2 of int64 [@@deriving irmin]
let test_variant2 =
  let open Irmin.Type in
    ((variant "test_variant2" (fun a2 -> function | A2 x1 -> a2 x1)) |~
       (case1 "A2" int64 (fun x1 -> A2 x1)))
      |> sealv
type test_variant3 =
  | A3 of string * test_variant2 [@@deriving irmin]
let test_variant3 =
  let open Irmin.Type in
    ((variant "test_variant3"
        (fun a3 -> function | A3 (x1, x2) -> a3 (x1, x2)))
       |~
       (case1 "A3" (pair string test_variant2) (fun (x1, x2) -> A3 (x1, x2))))
      |> sealv
type test_variant4 =
  | A4 
  | B4 
  | C4 [@@deriving irmin]
let test_variant4 =
  let open Irmin.Type in
    ((((variant "test_variant4"
          (fun a4 ->
             fun b4 -> fun c4 -> function | A4 -> a4 | B4 -> b4 | C4 -> c4))
         |~ (case0 "A4" A4))
        |~ (case0 "B4" B4))
       |~ (case0 "C4" C4))
      |> sealv
type test_variant5 =
  | A5 
  | B5 of string * test_variant4 
  | C5 of int32 * string * unit [@@deriving irmin]
let test_variant5 =
  let open Irmin.Type in
    ((((variant "test_variant5"
          (fun a5 ->
             fun b5 ->
               fun c5 ->
                 function
                 | A5 -> a5
                 | B5 (x1, x2) -> b5 (x1, x2)
                 | C5 (x1, x2, x3) -> c5 (x1, x2, x3)))
         |~ (case0 "A5" A5))
        |~
        (case1 "B5" (pair string test_variant4) (fun (x1, x2) -> B5 (x1, x2))))
       |~
       (case1 "C5" (triple int32 string unit)
          (fun (x1, x2, x3) -> C5 (x1, x2, x3))))
      |> sealv
type test_record1 = {
  alpha: string ;
  beta: int64 list ;
  gamma: test_variant5 }[@@deriving irmin]
let test_record1 =
  let open Irmin.Type in
    ((((record "irmin"
          (fun alpha -> fun beta -> fun gamma -> { alpha; beta; gamma }))
         |+ (field "alpha" string (fun t -> t.alpha)))
        |+ (field "beta" (list int64) (fun t -> t.beta)))
       |+ (field "gamma" test_variant5 (fun t -> t.gamma)))
      |> sealr
type test_record2 =
  {
  the_FIRST_identifier: test_record1 option ;
  the_SECOND_identifier: (string, int32) result list }[@@deriving irmin]
let test_record2 =
  let open Irmin.Type in
    (((record "irmin"
         (fun the_FIRST_identifier ->
            fun the_SECOND_identifier ->
              { the_FIRST_identifier; the_SECOND_identifier }))
        |+
        (field "the_FIRST_identifier" (option test_record1)
           (fun t -> t.the_FIRST_identifier)))
       |+
       (field "the_SECOND_identifier" (list (result string int32))
          (fun t -> t.the_SECOND_identifier)))
      |> sealr
