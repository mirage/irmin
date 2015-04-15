
(*

module type KEY_DERIVATION = sig
    val derivate: Cstruct.t -> Cstruct.t -> password:Cstruct.t option -> Cstruct.t
  end


(* Key derivation module *)
module Keyderivation = struct
   (* Derviate key TODO : Key Derivation algorithm *)
    let derivate ukey mkey ~password =
      match  password with
      | None -> ukey lxor mkey
      | Some x -> ukey lxor mkey lxor x
  end
 *)
