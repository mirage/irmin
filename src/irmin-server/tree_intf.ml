module type S = sig
  type kinded_key

  val kinded_key_t : kinded_key Irmin.Type.t

  type concrete

  val concrete_t : concrete Irmin.Type.t

  type t = Key of kinded_key | ID of int | Concrete of concrete
  [@@deriving irmin]
end

module type Tree = sig
  module type S = S

  module Make (Store : Irmin.Generic_key.S) :
    S
      with type kinded_key = Store.Tree.kinded_key
       and type concrete = Store.Tree.concrete
end
