
(**

  Krypto : Irmin Crypto Backend
  The kryptonite for protect your data of nasties

  TODO :
  Padding into blobs, and cut blobs on a defined size block -> We don't want to guess the size of content

*)


open Lwt
open Irmin


module type CIPHER_BLOCK = Irmin_krypto_cipher.MAKER
module Make_km = Irmin_krypto_km.Make
module Make_cipher = Irmin_krypto_cipher.Make


module type AO_MAKER_RAW =
  functor (K: Hash.S) ->
  functor (V: Tc.S0 with type t = Cstruct.t ) ->
  AO with type key = K.t and type value = V.t


module type AO_MAKER_CSTRUCT =
  functor (IK: Hash.S) ->
  functor (K: Hash.S) ->
  functor (V: Tc.S0 with type t = Cstruct.t) ->
  AO with type key = IK.t and type value = V.t				     


module KRYPTO_AO (C: CIPHER_BLOCK) (S:AO_MAKER_RAW) (K:Irmin.Hash.S) (V:Tc.S0) : AO
					     
(*					  
module Make_Krypto_AO (CB:CIPHER_BLOCK) (S: AO_MAKER_RAW) (K:Irmin.Hash.S) (V:Tc.S0) : AO_MAKER_RAW
					     
 *)
