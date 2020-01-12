(* Variants *)
type test_variant1 = A [@@deriving irmin]

type test_variant2 = A2 of int64 [@@deriving irmin]

type test_variant3 = A3 of string * test_variant2 [@@deriving irmin]

type test_variant4 = A4 | B4 | C4 [@@deriving irmin]

type test_variant5 =
  | A5
  | B5 of string * test_variant4
  | C5 of int32 * string * unit
[@@deriving irmin]

type test_variant6 = Nil | Cons of string * test_variant6 [@@deriving irmin]
