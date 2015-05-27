open Irmin

module type PERSISTANT_INDEX = sig
  type index
  type key

  val find: index -> key
  val add: index -> key -> unit
  val length_index: int
  val digest_index: Cstruct.t -> index
  val length_key: int
  val digest_key: Cstruct.t -> key

  (*
    type index
    type key

    val find: 'a -> 'b
    val add: 'a -> 'b -> unit
    val length_index: int
    val digest_index: Cstruct.t -> 'a
    val length_key: int
    val digest_key: Cstruct.t -> 'b
   *)

end

module type PERSISTANT_INDEX_MAKER =
  (*  functor (IK:Irmin.Hash.S) -> *)
  functor (K:Irmin.Hash.S) ->
  PERSISTANT_INDEX with type index = K.t and type key = K.t


module type RAW = Tc.S0 with type t = Cstruct.t

module type AO_MAKER_RAW =
  functor (K: Irmin.Hash.S) ->
  functor (V: RAW) ->
  AO with type key = K.t and type value = V.t

(*
module AOI (P: PERSISTANT_INDEX_MAKER) (*(IK:Irmin.Hash.S)*) (S:AO_MAKER_RAW) (K: Irmin.Hash.S) (V: Tc.S0) : AO
 *)

module Make (P: PERSISTANT_INDEX_MAKER) (AO: AO_MAKER_RAW) (RW:RW_MAKER) : S_MAKER

module HT: PERSISTANT_INDEX_MAKER
