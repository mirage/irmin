include Tree_intf

module Make (Store : Irmin.Generic_key.S) = struct
  type concrete = Store.Tree.concrete [@@deriving irmin]
  type kinded_key = Store.Tree.kinded_key [@@deriving irmin]

  type t = Key of kinded_key | ID of int | Concrete of concrete
  [@@deriving irmin]
end
