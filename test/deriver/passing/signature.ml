(* Tests of the signature deriver *)
module SigTests : sig
  type t = string [@@deriving irmin]

  type foo = unit [@@deriving irmin { name = "foo_generic" }]

  type my_int = int32 * t [@@deriving irmin]

  type my_variant =
    | A of (my_int, int) result
    | B of unit
    | C of string * int32
  [@@deriving irmin]
end = struct
  type t = string [@@deriving irmin]

  type foo = unit [@@deriving irmin { name = "foo_generic" }]

  type my_int = int32 * t [@@deriving irmin]

  type my_variant =
    | A of (my_int, int) result
    | B of unit
    | C of string * int32
  [@@deriving irmin]
end
