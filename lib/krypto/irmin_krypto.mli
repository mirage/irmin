(**

  Krypto : Irmin Crypto Backend
  The kryptonite for protect your data of nasties

  TODO :
  Padding into blobs, and cut blobs on a defined size block -> We don't want to guess the size of content

*)

open Irmin

module type CIPHER_BLOCK = Irmin_krypto_cipher.MAKER
module Make_km = Irmin_krypto_km.Make
module Make_cipher = Irmin_krypto_cipher.Make

module type RAW = Tc.S0 with type t = Cstruct.t

module type AO_MAKER_RAW =
  functor (K: Hash.S) ->
  functor (V: RAW) ->
  AO with type key = K.t and type value = V.t

module KRYPTO_AO (C: CIPHER_BLOCK) (S:AO_MAKER_RAW): AO_MAKER_RAW
