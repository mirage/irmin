(* Ensure that 'nonrec' assertions are respected *)
module S1 : sig
  type nonrec t = t list [@@deriving irmin]

  type nonrec t_alias = t_alias list [@@deriving irmin]
end = struct
  type nonrec t = t list [@@deriving irmin]

  type nonrec t_alias = t_alias list [@@deriving irmin]
end

(* Now test the interaction of 'nonrec' with custom naming *)
module S2 : sig
  type nonrec t = t list [@@deriving irmin { name = "t_witness" }]

  type nonrec t_alias = t_alias list [@@deriving irmin { name = "t_witness" }]
end = struct
  type nonrec t = t list [@@deriving irmin { name = "t_witness" }]

  type nonrec t_alias = t_alias list [@@deriving irmin { name = "t_witness" }]
end
