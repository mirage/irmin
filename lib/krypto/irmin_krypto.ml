(**

  Krypto : Irmin Crypto Backend
  The kryptonite for protect your data of nasties

  TODO :
  Padding into blobs, and cut blobs on a defined size block -> We don't want to guess the size of content

*)

open Lwt
open Irmin


module Log = Log.Make(struct let section = "KRYPO" end)

module type CIPHER_BLOCK = Irmin_krypto_cipher.MAKER
module Make_km = Irmin_krypto_km.Make
module Make_cipher = Irmin_krypto_cipher.Make

module type RAW = Tc.S0 with type t = Cstruct.t

module type AO_MAKER_RAW =
  functor (K: Hash.S) ->
  functor (V: RAW) ->
  AO with type key = K.t and type value = V.t

module KRYPTO_AO (C: CIPHER_BLOCK) (S:AO_MAKER_RAW) (K:Irmin.Hash.S) (V:RAW) = struct

    module AO = S(K)(V)

    type key = AO.key

    type value = AO.value

    type t = AO.t

    let hash_size = K.length

    let compute_ctr v = Nocrypto.Hash.SHA1.digest v (* MUST TO BE PARAMETRABLE *)

    let inject_ctr ~ctr blob =
      let len_blob = Cstruct.len blob in
      let res = Cstruct.create (hash_size + len_blob) in
      Cstruct.blit ctr 0 res 0 hash_size;
      Cstruct.blit blob 0 res hash_size len_blob;
      res

    let extract_ctr blob =
      let res = Cstruct.create hash_size in
      Cstruct.blit blob 0 res 0 hash_size;
      res

    let extract_value blob =
      let len_blob = Cstruct.len blob in
      let len = len_blob - hash_size in
      let res = Cstruct.create len in
      Cstruct.blit blob hash_size res 0 len;
      res

    let create config task =
      AO.create config task

    let task t =
      AO.task t

    let read t key =
      AO.read t key >>= function
      | None -> return_none
      | Some v ->
         let ctr = extract_ctr v in
         let value = extract_value v in
         return (Some (C.decrypt ~ctr value))

    let read_exn t key =
      try
        AO.read_exn t key >>=
          function x ->
                   let ctr = extract_ctr x in
                   let value = extract_value x in
                   return (C.decrypt ~ctr value)
      with
      | Not_found -> fail Not_found

    let mem t k =
      AO.mem t k

    let add t v =
      let ctr = compute_ctr v in
      let ctr2 = compute_ctr v in (* Temporary : there is a side effect into nocrypto *)
      C.encrypt ~ctr v |> inject_ctr ~ctr:ctr2 |> AO.add t

    (* TODO iter .... *)
    let iter _t (_fn : key -> value Lwt.t -> unit Lwt.t) =
      failwith "TODO"
  (* AO.iter t (fun k v ->
                 let ctr = Cstruct.of_string "1234abcd1234abcd" in
                 let v = v >|= fun v -> (C.decrypt ~ctr v) in
                 fn k v) *)

end

(*
module Make_Krypto_AO (CB:CIPHER_BLOCK) (S: AO_MAKER_RAW) (K:Irmin.Hash.S) (V:Tc.S0) (*: AO_MAKER*) = KRYPTO_AO (CB) (S)
 *)
