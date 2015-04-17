
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

module KRYPTO_AO (C: CIPHER_BLOCK) (S:AO_MAKER) (K: Irmin.Hash.S) (V: Tc.S0) : AO

module Make (CB:CIPHER_BLOCK) (K_AO: AO_MAKER) (RW:RW_MAKER) : S_MAKER
