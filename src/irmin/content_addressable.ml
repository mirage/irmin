(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Make
    (Append_only_maker : Append_only.Maker)
    (Hash_untyped : Hash.S)
    (Value : Type.S) =
struct
  include Append_only_maker (Hash_untyped) (Value)
  module Hash = Hash.Typed (Hash_untyped) (Value)

  type nonrec key = Key.t [@@deriving irmin ~pp]
  type nonrec hash = Hash_untyped.t [@@deriving irmin ~equal ~pp]

  let hash = Hash.hash

  let find t k =
    find t k >>= function
    | None -> Lwt.return_none
    | Some v as r ->
        let h = hash v in
        let h' = Key.to_hash k in
        if equal_hash h h' then Lwt.return r
        else
          Fmt.kstrf Lwt.fail_invalid_arg "corrupted value: got %a, expecting %a"
            pp_hash h pp_hash h'

  let unsafe_add t k v = add t k v >|= fun _ -> ()

  let add t v =
    let h = hash v in
    add t h v
end

module Check_closed (CA : Maker) (Hash : Hash.S) (Value : Type.S) = struct
  module S = CA (Hash) (Value)

  type 'a t = { closed : bool ref; t : 'a S.t }
  type value = S.value
  type key = S.key

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

  let clear t =
    check_not_closed t;
    S.clear t.t
end
