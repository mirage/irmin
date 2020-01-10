open Ppxlib

module Unsupported : sig
  val tuple_size : loc:location -> int -> 'a

  val type_arrow : loc:location -> core_type -> 'a

  val type_var : loc:location -> label -> 'a

  val type_open : loc:location -> 'a

  val type_poly : loc:location -> core_type -> 'a

  val type_open_polyvar : loc:location -> core_type -> 'a

  val type_package : loc:location -> core_type -> 'a

  val type_extension : loc:location -> core_type -> 'a

  val type_alias : loc:location -> core_type -> 'a
end
