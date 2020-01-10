open Ppxlib

module type S = sig
  val derive_str :
    ?name:string -> rec_flag * type_declaration list -> structure_item list
  (** Deriver for Irmin generics. *)

  val derive_sig :
    ?name:string -> rec_flag * type_declaration list -> signature_item list
  (** Deriver for Irmin generic type signatures. *)
end

module Located (A : Ast_builder.S) : S
