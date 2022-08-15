(*
 * Copyright (c) 2021 Craig Ferguson <craig@tarides.com>
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

module Check_closed_store (CA : S) = struct
  module Key = CA.Key

  type 'a t = { closed : bool ref; t : 'a CA.t }
  type value = CA.value
  type key = CA.key
  type hash = CA.hash

  let make_closeable t = { closed = ref false; t }

  let get_if_open_exn t =
    if !(t.closed) then raise Store_properties.Closed else t.t

  let mem t k = (get_if_open_exn t |> CA.mem) k
  let index t h = (get_if_open_exn t |> CA.index) h
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

module Check_closed (M : Maker) (Hash : Hash.S) (Value : Type.S) = struct
  module CA = M (Hash) (Value)
  include Check_closed_store (CA)

  let v conf =
    let+ t = CA.v conf in
    { closed = ref false; t }
end
