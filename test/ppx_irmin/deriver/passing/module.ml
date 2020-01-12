(* Types within modules *)
module ModuleQualifiedTypes = struct
  module X = struct
    type t = int [@@deriving irmin]
  end

  module Y = struct
    type foo = X.t list [@@deriving irmin]
  end

  type t = X.t [@@deriving irmin]

  type t_result = (X.t, unit) result [@@deriving irmin]

  type foo = Y.foo [@@deriving irmin]

  type foo_list = Y.foo list [@@deriving irmin]
end
