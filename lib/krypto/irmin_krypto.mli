
(**

  Krypto : Irmin Crypto Backend
  The kryptonite for protect your data of nasties

  TODO :
  Padding into blobs, and cut blobs on a defined size block -> We don't want to guess the size of content

*)


open Lwt


module type CIPHER_BLOCK = Irmin_krypto_cipher.MAKER

module type AO_MAKER = Irmin.AO_MAKER
module type RW_MAKER = Irmin.RW_MAKER
module type STORE_MAKER = Irmin.S_MAKER

module type STORE = Irmin.S
module type AO = Irmin.AO
module type RW = Irmin.RW

module Make_km = Irmin_krypto_km.Make
module Make_cipher = Irmin_krypto_cipher.Make

module KRYPTO_AO (C: CIPHER_BLOCK) (S:AO_MAKER) (K: Irmin.Hash.S) (V: Tc.S0) : AO

module Make (CB:CIPHER_BLOCK) (K_AO: AO_MAKER) (RW:RW_MAKER) (C: Irmin.Contents.S) (T: Irmin.Tag.S) (H: Irmin.Hash.S) : STORE_MAKER
