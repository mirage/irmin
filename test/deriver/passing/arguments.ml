(* Tests of the arguments/attributes *)
type c = string [@@deriving irmin { name = "c_wit" }]

type d = int [@@deriving irmin { name = "witness_for_d" }]

type point_elsewhere1 = (c[@witness c_wit]) [@@deriving irmin]

type point_elsewhere2 = int * (c[@witness c_wit]) [@@deriving irmin]

type point_elsewhere3 =
  | A of int * (c[@witness c_wit])
  | B of (c[@witness c_wit])
[@@deriving irmin]

type point_elsewhere4 = {
  lorem : string;
  ipsum : (c[@witness c_wit]);
  dolor : int;
  sit : (d[@witness witness_for_d]);
}
[@@deriving irmin]
