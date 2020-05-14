(* Ensure that the [Json] module in [Irmin.Type] doesn't shadow references to
   types contained in a different [Json] module.

   Disabled due to https://github.com/mirage/irmin/issues/923. *)

module Json = struct
  type t = string

  let t = Irmin.Type.string
end

type foo = { contents : Json.t } [@@deriving irmin]

let (_ : foo Irmin.Type.t) = foo_t
