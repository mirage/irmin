type 'a typ = 'a Irmin.Type.t

module Trivial : sig
  type t [@@deriving irmin]
end = struct
  type t = int as 'a [@@deriving irmin]
end

module Recursive : sig
  type 'a tree [@@deriving irmin]
end = struct
  type 'a tree = [ `Branch of 'tree * int * 'tree | `Leaf of 'a ] as 'tree
  [@@deriving irmin]
end
