(* Tests of the arguments/attributes *)
type c = string [@@deriving irmin { name = "c_wit" }]

let (_ : c Irmin.Type.t) = c_wit

type d = int [@@deriving irmin { name = "generic_for_d" }]

let (_ : d Irmin.Type.t) = generic_for_d

type point_elsewhere1 = (c[@generic c_wit]) [@@deriving irmin]

type point_elsewhere2 = int * (c[@generic c_wit]) [@@deriving irmin]

type point_elsewhere3 =
  | A of int * (c[@generic c_wit])
  | B of (c[@generic c_wit])
[@@deriving irmin]

type point_elsewhere4 = {
  lorem : string;
  ipsum : (c[@generic c_wit]);
  dolor : int;
  sit : (d[@generic generic_for_d]);
}
[@@deriving irmin]

let (_ : point_elsewhere1 Irmin.Type.t) = point_elsewhere1_t

let (_ : point_elsewhere2 Irmin.Type.t) = point_elsewhere2_t

let (_ : point_elsewhere3 Irmin.Type.t) = point_elsewhere3_t

let (_ : point_elsewhere4 Irmin.Type.t) = point_elsewhere4_t
