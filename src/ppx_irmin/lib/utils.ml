open Ppxlib

let ( >> ) f g x = g (f x)

let ( >|= ) x f = List.map f x

module Make (A : Ast_builder.S) : sig
  val compose_all : ('a -> 'a) list -> 'a -> 'a
  (** Left-to-right composition of a list of functions. *)

  val lambda : string list -> expression -> expression
  (** [lambda \[ "x_1"; ...; "x_n" \] e] is [fun x1 ... x_n -> e] *)

  val arrow : core_type list -> core_type -> core_type
  (** [arrow \[ "t_1"; ...; "t_n" \] u] is [t_1 -> ... -> t_n -> u] *)
end = struct
  open A

  let compose_all l x = List.fold_left ( |> ) x (List.rev l)

  let lambda = List.map (pvar >> pexp_fun Nolabel None) >> compose_all

  let arrow = List.map (ptyp_arrow Nolabel) >> compose_all
end
