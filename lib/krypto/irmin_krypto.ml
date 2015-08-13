(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2015 Mounir Nasr Allah <mounir@nasrallah.co>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(**

  Krypto : Irmin Crypto Backend
  The kryptonite for protect your data of nasties

  TODO :
  Padding into blobs, and cut blobs on a defined size block -> We don't want to guess the size of content

*)

open Lwt
open Irmin


module Log = Log.Make(struct let section = "KRYPO" end)

		     
module type CIPHER = Irmin_krypto_cipher.CIPHER
module Make_km = Irmin_krypto_km.Make
module Make_cipher = Irmin_krypto_cipher.Make_CTR

					    
module KRYPTO_AO (C: CIPHER) (S:AO_MAKER_RAW) (K:Irmin.Hash.S) (V:RAW) = struct

    module AO = S(K)(V)

    type key = AO.key

    type value = AO.value

    type t = AO.t

    let hash_size = 16 (* K.length *)

    let compute_ctr v = 
    let tmp = Nocrypto.Hash.SHA1.digest v in
    let res = Cstruct.create hash_size in 
    Cstruct.blit tmp 0 res 0 hash_size;
    res


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
      C.encrypt ~ctr v |> inject_ctr ~ctr |> AO.add t


    let iter _t (_fn : key -> value Lwt.t -> unit Lwt.t) =
      failwith "NOT NEEDED"

end


