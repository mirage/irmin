(* Nested tuple type *)
type deep_tuple =
  (((int32 * int32) * int32 * int32) * int32 * int32) * int32 * int32
[@@deriving irmin]

let (_ : deep_tuple Irmin.Type.t) = deep_tuple_t
