(* Tests of type aliases *)
type test_result = (int, string) result [@@deriving irmin]

type t_alias = test_result [@@deriving irmin]

type t = t_alias [@@deriving irmin]
