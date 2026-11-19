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

module Check_closed_store (CA : S) = struct
  type 'a t = { closed : bool ref; t : 'a CA.t }
  type value = CA.value
  type key = CA.key

  let make_closeable t = { closed = ref false; t }
  let make_closeable_with closed t = { closed; t }

  let get_if_open_exn t =
    if !(t.closed) then raise Store_properties.Closed else t.t

  let mem t k = (get_if_open_exn t |> CA.mem) k
  let find t k = (get_if_open_exn t |> CA.find) k
  let add t v = (get_if_open_exn t |> CA.add) v
  let unsafe_add t k v = (get_if_open_exn t |> CA.unsafe_add) k v

  let batch t f =
    (get_if_open_exn t |> CA.batch) (fun w -> f { t = w; closed = t.closed })

  let close t =
    if !(t.closed) then Lwt.return_unit
    else (
      t.closed := true;
      CA.close t.t)
end

module Check_closed (CA : Maker) (K : Hash.S) (V : Type.S) = struct
  module S = CA (K) (V)
  include Check_closed_store (S)

  let v conf =
    let+ t = S.v conf in
    make_closeable t
end
