(* Tests of type aliases *)
type test_result = (int, string) result [@@deriving irmin]

type t_alias = test_result [@@deriving irmin]

type t = t_alias [@@deriving irmin]

let (_ : test_result Irmin_type.Type.t) = test_result_t

let (_ : t_alias Irmin_type.Type.t) = t_alias_t

let (_ : t Irmin_type.Type.t) = t
