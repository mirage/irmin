(* Polymorphic variants *)
type test_polyvar1 = [ `On of int | `Off ] [@@deriving irmin]

type test_polyvar2 =
  [ `Outer_a of [ `Inner_a | `Inner_b ]
  | `Outer_b of [ `Inner_a ]
  | `Outer_c of [ `Inner_a of string | `Inner_c of int ] ]
[@@deriving irmin]

type test_polyvar3 =
  [ `Branch of test_polyvar3 * test_polyvar3 | `Leaf of string ]
[@@deriving irmin]
