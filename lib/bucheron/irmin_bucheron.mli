
(**



*)


open Lwt
open Irmin



module type AO_MAKER_RAW =
  functor (K: Hash.S) ->
  functor (V: Tc.S0 with type t = Cstruct.t) ->
  AO with type key = K.t and type value = V.t


module BUCHERON_AO (S:AO_MAKER_RAW) (K:Irmin.Hash.S) (V:Tc.S0) : AO
					    
(*
module type AO_MAKER_CSTRUCT =
  functor (IK: Hash.S) ->
  functor (K: Hash.S) ->
  functor (V: Tc.S0 with type t = Cstruct.t) ->
  AO with type key = IK.t and type value = V.t
 *)					     

(*					    
module BUCHERON_AO (S:AO_MAKER_RAW) (K:Irmin.Hash.S) (V:Tc.S0) :
AO with type value = Cstruct.t
 *)     
