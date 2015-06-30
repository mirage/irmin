open Irmin

module type RAW = Tc.S0 with type t = Cstruct.t

(* TDOO: move in its own file and share it with krypto *)
module type AO_MAKER_RAW =
  functor (K: Hash.S) ->
  functor (V: RAW) ->
  AO with type key = K.t and type value = V.t


module CHUNCK_AO (S:AO_MAKER_RAW): AO_MAKER_RAW

				       (*
val config: ?conf:Irmin.config -> ?size:int -> unit -> Irmin.config
					*)
				       
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
