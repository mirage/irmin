module Elsewhere : sig
  type t [@@deriving irmin { lib = Some "Foo" }]
end = struct
  (* module Foo = Irmin.Type
   * 
   * module Irmin = struct end *)

  type t = unit * unit [@@deriving irmin { lib = Some "Foo" }]
end

module Locally_avaliable : sig
  type t [@@deriving irmin { lib = None }]
end = struct
  (* open Irmin.Type
   * 
   * module Irmin = struct end *)

  type t = unit * unit [@@deriving irmin { lib = None }]
end
