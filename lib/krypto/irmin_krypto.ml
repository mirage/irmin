
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

  Krypto : Irmin Cryptographic Backend
  The kryptonite for protect your data of nasties

  TODO :
  Padding into blobs, and cut blobs on a defined size block -> We don't want to guess the size of content

*)

open Lwt


module Log = Log.Make(struct let section = "KRYPO" end)

module type RAW = Irmin.RAW
module type AO_MAKER_RAW = Irmin.AO_MAKER_RAW
module type CIPHER = Irmin_krypto_cipher.CIPHER
module Make_km = Irmin_krypto_km.Make
module Make_CTR = Irmin_krypto_cipher.Make_CTR


module KRYPTO_AO (C: CIPHER) (S:AO_MAKER_RAW) (K:Irmin.Hash.S) (V:RAW) = struct

    module AO = S(K)(V)

    type key = AO.key

    type value = AO.value

    type t = AO.t

    let create config task =
      AO.create config task

    let task t =
      AO.task t

    let read t key =
      AO.read t key >>= function
      | None -> return_none
      | Some v -> return (Some (C.decrypt v))

    let read_exn t key =
      try
        AO.read_exn t key >>= fun x ->
        return (C.decrypt x)
      with
      | Not_found -> fail Not_found

    let mem t k =
       AO.mem t k

    let add t v =
      let x = C.encrypt v in
      AO.add t x

     let iter _t (_fn : key -> value Lwt.t -> unit Lwt.t) =
      failwith "TODO"

end



module Make_Krypto (C: CIPHER) (S:AO_MAKER_RAW) : AO_MAKER_RAW = KRYPTO_AO (C) (S)
