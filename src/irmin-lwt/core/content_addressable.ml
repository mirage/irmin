(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
include Content_addressable_intf

module Make (AO : Append_only.Maker) (K : Hash.S) (V : Type.S) = struct
  include AO (K) (V)
  open Lwt.Infix
  module H = Hash.Typed (K) (V)

  let hash = H.hash
  let pp_key = Type.pp K.t
  let equal_hash = Type.(unstage (equal K.t))

  let find t k =
    find t k >>= function
    | None -> Lwt.return_none
    | Some v as r ->
        let k' = hash v in
        if equal_hash k k' then Lwt.return r
        else
          Fmt.kstr Lwt.fail_invalid_arg "corrupted value: got %a, expecting %a"
            pp_key k' pp_key k

  let unsafe_add t k v = add t k v

  let add t v =
    let k = hash v in
    add t k v >|= fun () -> k
end

module Check_closed (CA : Maker) (K : Hash.S) (V : Type.S) = struct
  module S = CA (K) (V)

  type 'a t = { closed : bool ref; t : 'a S.t }
  type key = S.key
  type value = S.value

  let check_not_closed t = if !(t.closed) then raise Store_properties.Closed

  let mem t k =
    check_not_closed t;
    S.mem t.t k

  let find t k =
    check_not_closed t;
    S.find t.t k

  let add t v =
    check_not_closed t;
    S.add t.t v

  let unsafe_add t k v =
    check_not_closed t;
    S.unsafe_add t.t k v

  let batch t f =
    check_not_closed t;
    S.batch t.t (fun w -> f { t = w; closed = t.closed })

  let v conf =
    let+ t = S.v conf in
    { closed = ref false; t }

  let close t =
    if !(t.closed) then Lwt.return_unit
    else (
      t.closed := true;
      S.close t.t)
end
