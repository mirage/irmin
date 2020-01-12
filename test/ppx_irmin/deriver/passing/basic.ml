(* Tests of the basic types *)
type test_unit = unit [@@deriving irmin]

type test_bool = bool [@@deriving irmin]

type test_char = char [@@deriving irmin]

type test_int32 = int32 [@@deriving irmin]

type test_int64 = int64 [@@deriving irmin]

type test_float = float [@@deriving irmin]

type test_string = string [@@deriving irmin]
