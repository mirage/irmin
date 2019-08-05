type test_unit = unit[@@deriving irmin]
let test_unit_t = Irmin.Type.unit
type test_bool = bool[@@deriving irmin]
let test_bool_t = Irmin.Type.bool
type test_char = char[@@deriving irmin]
let test_char_t = Irmin.Type.char
type test_int32 = int32[@@deriving irmin]
let test_int32_t = Irmin.Type.int32
type test_int64 = int64[@@deriving irmin]
let test_int64_t = Irmin.Type.int64
type test_float = float[@@deriving irmin]
let test_float_t = Irmin.Type.float
type test_string = string[@@deriving irmin]
let test_string_t = Irmin.Type.string
type test_list1 = string list[@@deriving irmin]
let test_list1_t = let open Irmin.Type in list string
type test_list2 = int32 list list list[@@deriving irmin]
let test_list2_t = let open Irmin.Type in list (list (list int32))
type test_array = bool array[@@deriving irmin]
let test_array_t = let open Irmin.Type in array bool
type test_option = unit option[@@deriving irmin]
let test_option_t = let open Irmin.Type in option unit
type test_pair = (string * int32)[@@deriving irmin]
let test_pair_t = let open Irmin.Type in pair string int32
type test_triple = (string * int32 * bool)[@@deriving irmin]
let test_triple_t = let open Irmin.Type in triple string int32 bool
type test_result = (int32, string) result[@@deriving irmin]
let test_result_t = let open Irmin.Type in result int32 string
module ModuleQualifiedTypes =
  struct
    module X =
      struct type t = int[@@deriving irmin]
             let t = Irmin.Type.int end
    module Y =
      struct
        type foo = X.t list[@@deriving irmin]
        let foo_t = let open Irmin.Type in list X.t
      end
    type t = X.t[@@deriving irmin]
    let t = X.t
    type t_result = (X.t, unit) result[@@deriving irmin]
    let t_result_t = let open Irmin.Type in result X.t unit
    type foo = Y.foo[@@deriving irmin]
    let foo_t = Y.foo_t
    type foo_list = Y.foo list[@@deriving irmin]
    let foo_list_t = let open Irmin.Type in list Y.foo_t
  end
type deep_tuple =
  ((((int32 * int32) * int32 * int32) * int32 * int32) * int32 * int32)
[@@deriving irmin]
let deep_tuple_t =
  let open Irmin.Type in
    triple (triple (triple (pair int32 int32) int32 int32) int32 int32) int32
      int32
type t_alias = test_result[@@deriving irmin]
let t_alias_t = test_result_t
type t = t_alias[@@deriving irmin]
let t = t_alias_t
module S1 :
  sig
    type nonrec t = t list[@@deriving irmin]
    val t : t Irmin.Type.t
    type nonrec t_alias = t_alias list[@@deriving irmin]
    val t_alias_t : t_alias Irmin.Type.t
  end =
  struct
    type nonrec t = t list[@@deriving irmin]
    let t = let open Irmin.Type in list t
    type nonrec t_alias = t_alias list[@@deriving irmin]
    let t_alias_t = let open Irmin.Type in list t_alias_t
  end 
module S2 :
  sig
    type nonrec t = t list[@@deriving irmin { name = "t_witness" }]
    val t_witness : t Irmin.Type.t
    type nonrec t_alias = t_alias list[@@deriving
                                        irmin { name = "t_witness" }]
    val t_witness : t_alias Irmin.Type.t
  end =
  struct
    type nonrec t = t list[@@deriving irmin { name = "t_witness" }]
    let t_witness = let open Irmin.Type in list t
    type nonrec t_alias = t_alias list[@@deriving
                                        irmin { name = "t_witness" }]
    let t_witness = let open Irmin.Type in list t_alias_t
  end 
type test_variant1 =
  | A [@@deriving irmin]
let test_variant1_t =
  let open Irmin.Type in
    ((variant "test_variant1" (fun a -> function | A -> a)) |~ (case0 "A" A))
      |> sealv
type test_variant2 =
  | A2 of int64 [@@deriving irmin]
let test_variant2_t =
  let open Irmin.Type in
    ((variant "test_variant2" (fun a2 -> function | A2 x1 -> a2 x1)) |~
       (case1 "A2" int64 (fun x1 -> A2 x1)))
      |> sealv
type test_variant3 =
  | A3 of string * test_variant2 [@@deriving irmin]
let test_variant3_t =
  let open Irmin.Type in
    ((variant "test_variant3"
        (fun a3 -> function | A3 (x1, x2) -> a3 (x1, x2)))
       |~
       (case1 "A3" (pair string test_variant2_t)
          (fun (x1, x2) -> A3 (x1, x2))))
      |> sealv
type test_variant4 =
  | A4 
  | B4 
  | C4 [@@deriving irmin]
let test_variant4_t =
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
let test_variant5_t =
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
        (case1 "B5" (pair string test_variant4_t)
           (fun (x1, x2) -> B5 (x1, x2))))
       |~
       (case1 "C5" (triple int32 string unit)
          (fun (x1, x2, x3) -> C5 (x1, x2, x3))))
      |> sealv
type test_variant6 =
  | Nil 
  | Cons of string * test_variant6 [@@deriving irmin]
let test_variant6_t =
  Irmin.Type.mu
    (fun test_variant6_t ->
       let open Irmin.Type in
         (((variant "test_variant6"
              (fun nil ->
                 fun cons ->
                   function | Nil -> nil | Cons (x1, x2) -> cons (x1, x2)))
             |~ (case0 "Nil" Nil))
            |~
            (case1 "Cons" (pair string test_variant6_t)
               (fun (x1, x2) -> Cons (x1, x2))))
           |> sealv)
type test_record1 = {
  alpha: string ;
  beta: int64 list ;
  gamma: test_variant5 }[@@deriving irmin]
let test_record1_t =
  let open Irmin.Type in
    ((((record "test_record1"
          (fun alpha -> fun beta -> fun gamma -> { alpha; beta; gamma }))
         |+ (field "alpha" string (fun t -> t.alpha)))
        |+ (field "beta" (list int64) (fun t -> t.beta)))
       |+ (field "gamma" test_variant5_t (fun t -> t.gamma)))
      |> sealr
type test_record2 =
  {
  the_FIRST_identifier: test_record1 option ;
  the_SECOND_identifier: (string, int32) result list }[@@deriving irmin]
let test_record2_t =
  let open Irmin.Type in
    (((record "test_record2"
         (fun the_FIRST_identifier ->
            fun the_SECOND_identifier ->
              { the_FIRST_identifier; the_SECOND_identifier }))
        |+
        (field "the_FIRST_identifier" (option test_record1_t)
           (fun t -> t.the_FIRST_identifier)))
       |+
       (field "the_SECOND_identifier" (list (result string int32))
          (fun t -> t.the_SECOND_identifier)))
      |> sealr
type c = string[@@deriving irmin { name = "c_wit" }]
let c_wit = Irmin.Type.string
type d = int[@@deriving irmin { name = "witness_for_d" }]
let witness_for_d = Irmin.Type.int
type point_elsewhere1 = ((c)[@witness c_wit])[@@deriving irmin]
let point_elsewhere1_t = c_wit
type point_elsewhere2 = (int * ((c)[@witness c_wit]))[@@deriving irmin]
let point_elsewhere2_t = let open Irmin.Type in pair int c_wit
type point_elsewhere3 =
  | A of int * ((c)[@witness c_wit]) 
  | B of ((c)[@witness c_wit]) [@@deriving irmin]
let point_elsewhere3_t =
  let open Irmin.Type in
    (((variant "point_elsewhere3"
         (fun a ->
            fun b -> function | A (x1, x2) -> a (x1, x2) | B x1 -> b x1))
        |~ (case1 "A" (pair int c_wit) (fun (x1, x2) -> A (x1, x2))))
       |~ (case1 "B" c_wit (fun x1 -> B x1)))
      |> sealv
type point_elsewhere4 =
  {
  lorem: string ;
  ipsum: ((c)[@witness c_wit]) ;
  dolor: int ;
  sit: ((d)[@witness witness_for_d]) }[@@deriving irmin]
let point_elsewhere4_t =
  let open Irmin.Type in
    (((((record "point_elsewhere4"
           (fun lorem ->
              fun ipsum ->
                fun dolor -> fun sit -> { lorem; ipsum; dolor; sit }))
          |+ (field "lorem" string (fun t -> t.lorem)))
         |+ (field "ipsum" c_wit (fun t -> t.ipsum)))
        |+ (field "dolor" int (fun t -> t.dolor)))
       |+ (field "sit" witness_for_d (fun t -> t.sit)))
      |> sealr
module SigTests :
  sig
    type t = string[@@deriving irmin]
    val t : t Irmin.Type.t
    type foo = unit[@@deriving irmin { name = "foo_witness" }]
    val foo_witness : foo Irmin.Type.t
    type my_int = (int32 * t)[@@deriving irmin]
    val my_int_t : my_int Irmin.Type.t
    type my_variant =
      | A of (my_int, int) result 
      | B of unit 
      | C of string * int32 [@@deriving irmin]
    val my_variant_t : my_variant Irmin.Type.t
  end =
  struct
    type t = string[@@deriving irmin]
    let t = Irmin.Type.string
    type foo = unit[@@deriving irmin { name = "foo_witness" }]
    let foo_witness = Irmin.Type.unit
    type my_int = (int32 * t)[@@deriving irmin]
    let my_int_t = let open Irmin.Type in pair int32 t
    type my_variant =
      | A of (my_int, int) result 
      | B of unit 
      | C of string * int32 [@@deriving irmin]
    let my_variant_t =
      let open Irmin.Type in
        ((((variant "my_variant"
              (fun a ->
                 fun b ->
                   fun c ->
                     function
                     | A x1 -> a x1
                     | B x1 -> b x1
                     | C (x1, x2) -> c (x1, x2)))
             |~ (case1 "A" (result my_int_t int) (fun x1 -> A x1)))
            |~ (case1 "B" unit (fun x1 -> B x1)))
           |~ (case1 "C" (pair string int32) (fun (x1, x2) -> C (x1, x2))))
          |> sealv
  end 
