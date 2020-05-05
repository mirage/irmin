module Elsewhere : sig
  module Foo : module type of Irmin.Type

  type t [@@deriving irmin { lib = Some "Foo" }]
end = struct
  module Foo = Irmin.Type

  module Irmin = struct end

  type t = unit * unit [@@deriving irmin { lib = Some "Foo" }]
end

module Locally_avaliable : sig
  type 'a ty

  type t [@@deriving irmin { lib = None }]
end = struct
  include Irmin.Type

  module Irmin = struct end

  type t = unit * unit [@@deriving irmin { lib = None }]
end
