(* Tests of the arguments/attributes *)
type c = string [@@deriving irmin { name = "c_wit" }]

let (_ : c Irmin_type.Type.t) = c_wit

type d = int [@@deriving irmin { name = "repr_for_d" }]

let (_ : d Irmin_type.Type.t) = repr_for_d

type point_elsewhere1 = (c[@repr c_wit]) [@@deriving irmin]

type point_elsewhere2 = int * (c[@repr c_wit]) [@@deriving irmin]

type point_elsewhere3 = A of int * (c[@repr c_wit]) | B of (c[@repr c_wit])
[@@deriving irmin]

type point_elsewhere4 = {
  lorem : string;
  ipsum : (c[@repr c_wit]);
  dolor : int;
  sit : (d[@repr repr_for_d]);
}
[@@deriving irmin]

let (_ : point_elsewhere1 Irmin_type.Type.t) = point_elsewhere1_t

let (_ : point_elsewhere2 Irmin_type.Type.t) = point_elsewhere2_t

let (_ : point_elsewhere3 Irmin_type.Type.t) = point_elsewhere3_t

let (_ : point_elsewhere4 Irmin_type.Type.t) = point_elsewhere4_t
