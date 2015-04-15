
(*

  The Kryptonite : Irmin Crypto Backend

   TODO :
   Padding into blobs, and cut blobs on a defined size block -> We don't want to guess the size of content

*)


open Lwt

module Log = Log.Make(struct let section = "KRYPO" end)


module type CIPHER_BLOCK = Krypto_cipher.MAKER
module type AO_MAKER = Irmin.AO_MAKER
module type AO = Ir_ao.STORE
module type RW = Ir_rw.STORE

module KRYPTO_AO (C: CIPHER_BLOCK) (S:AO_MAKER) (K: Irmin.Hash.S) (V: Tc.S0) = struct

    module AO = S(K)(V)

    type key = AO.key

    type value = AO.value

    let to_cstruct x = Tc.write_cstruct (module V) x
    let of_cstruct x = Tc.read_cstruct (module V) x

    let create config task =
      AO.create config task

    let task t =
      AO.task t

    let read t key =
      AO.read t key >>= function
      | None -> return_none
      | Some v -> return (Some (of_cstruct (C.decrypt (to_cstruct v))))


    let read_exn t key =
      try
        AO.read_exn t key >>= function x -> return (of_cstruct (C.decrypt (to_cstruct x)))
      with
      | Not_found -> fail Not_found

    let mem t k =
      AO.mem t k

    let add t v =
      to_cstruct v |> C.encrypt |> of_cstruct |> AO.add t

  end



module Make (CB:CIPHER_BLOCK) (AO: AO) (RW:RW) (C: Irmin.Contents.S) (T: Irmin.Tag.S) (H: Irmin.Hash.S) =
  Irmin.Make(KRYPTO_AO(CB)(AO))(RW)(C)(T)(H)
