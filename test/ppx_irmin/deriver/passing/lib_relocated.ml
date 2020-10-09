module Elsewhere : sig
  module Foo : module type of Irmin_type.Type

  type t [@@deriving irmin { lib = Some "Foo" }]
end = struct
  module Foo = Irmin_type.Type

  module Irmin = struct end

  type t = unit * unit [@@deriving irmin { lib = Some "Foo" }]
end

module Locally_avaliable : sig
  type 'a ty

  type t [@@deriving irmin { lib = None }]
end = struct
  let pair, unit = Irmin_type.Type.(pair, unit)

  type 'a ty = 'a Irmin_type.Type.ty

  module Irmin = struct end

  type t = unit * unit [@@deriving irmin { lib = None }]
end
