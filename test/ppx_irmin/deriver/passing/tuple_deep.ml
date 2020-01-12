(* Nested tuple type *)
type deep_tuple =
  (((int32 * int32) * int32 * int32) * int32 * int32) * int32 * int32
[@@deriving irmin]
