(*
 * Copyright (c) 2021 Craig Ferguson <craig@tarides.com>
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
include Indexable_intf

module Maker_concrete_key2_of_1 (X : Maker_concrete_key1) = struct
  type ('h, _) key = 'h X.key

  module Key (H : Hash.S) (_ : Type.S) = X.Key (H)
  module Make = X.Make
end

module Of_content_addressable (Key : Type.S) (S : Content_addressable.S) =
struct
  include S

  type hash = key
  type key = Key.t

  module Key = struct
    include Key

    type nonrec hash = hash

    let to_hash x = x
  end

  let index _ h = Lwt.return_some h
  let unsafe_add t h v = unsafe_add t h v >|= fun () -> h
end

module Check_closed (CA : Maker) (Hash : Hash.S) (Value : Type.S) = struct
  module S = CA (Hash) (Value)
  module Key = S.Key

  type 'a t = { closed : bool ref; t : 'a S.t }
  type value = S.value
  type key = S.key
  type hash = S.hash

  let check_not_closed t = if !(t.closed) then raise Store_properties.Closed

  let mem t k =
    check_not_closed t;
    S.mem t.t k

  let index t h =
    check_not_closed t;
    S.index t.t h

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
