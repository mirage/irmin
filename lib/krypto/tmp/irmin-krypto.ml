
(*

  The Kryptonite : Irmin Crypto Backend

   TODO :
   Padding into blobs, and cut blobs on a defined size block -> We don't want to guess the size of content

*)


module Log = Log.Make(struct let section = "KRYPO" end)


module type CIPHER_BLOCK = Krypto_cipher.CIPHER_BLOCK
module type AO = Irmin.AO


module KRYPTO_AO (C: CIPHER_BLOCK) (H: Irmin.Hash.S) (S:AO) = struct

    type t
    type key
    type value

    let create config task =
      S.create config task

    let task t =
      S.task t

    let read t key =
      (*      let ekey = CH.encrypt key in *)
      let content = S.read t ekey.message in
      C.decrypt ~hash:dkey ~content:v

    let read_exn t key =
      let ekey:CH.result = CH.encrypt key in
      try
        let content = S.read_exn t ekey.message in
        CC.decrypt ~hash:dkey ~content:v
      with
      | Not_found -> fail Not_found

    let mem t k =
      let ekey:CH.result = CH.encrypt key in
      S.mem t ekey.message

    let add t v =
      let ev = CC.encrypt ~content:v in
      S.add t ev

  end
