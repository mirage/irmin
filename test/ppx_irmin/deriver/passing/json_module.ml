(* Ensure that the [Json] module in [Irmin_type.Type] doesn't shadow references to
   types contained in a different [Json] module.

   Regression test for https://github.com/mirage/irmin/issues/923. *)

module Json = struct
  type t = string

  let t = Irmin_type.Type.string
end

type foo = { contents : Json.t } [@@deriving irmin]

let (_ : foo Irmin_type.Type.t) = foo_t
