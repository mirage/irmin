(* When a type is annotated with the [nobuiltin] annotation, it should be
   considered as an abstract type (i.e. don't pull representations from
   [Irmin_type.Type]). *)

type unit = string [@@deriving irmin]

(* Shadow [Stdlib.unit] *)
module Nobuiltin_t = struct
  type t = (unit[@nobuiltin]) [@@deriving irmin]

  (* [t]'s repr should be for strings. *)
  let (_ : string Irmin_type.Type.t) = t
end

module Nobuiltin_foo = struct
  type foo = (unit[@irmin.nobuiltin]) [@@deriving irmin]

  (* [foo]'s repr should be for strings too. *)
  let (_ : string Irmin_type.Type.t) = foo_t
end

module Nobuiltin_operator = struct
  (* Define our own representation of [result]. *)
  let result_t a b = Irmin_type.Type.pair a b

  let int32_t = Irmin_type.Type.int

  let int64_t = Irmin_type.Type.bool

  type u = (((int32[@nobuiltin]), int64) result[@nobuiltin]) [@@deriving irmin]

  let (_ : (int * int64) Irmin_type.Type.t) = u_t
end
