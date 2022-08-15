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

open Import
include Atomic_write_intf

module Check_closed_store (AW : S) = struct
  type t = { closed : bool ref; t : AW.t }
  type key = AW.key
  type value = AW.value
  type watch = AW.watch

  let make_closeable t = { closed = ref false; t }

  let get_if_open_exn t =
    if !(t.closed) then raise Store_properties.Closed else t.t

  let mem t k = (get_if_open_exn t |> AW.mem) k
  let find t k = (get_if_open_exn t |> AW.find) k
  let set t k v = (get_if_open_exn t |> AW.set) k v

  let test_and_set t k ~test ~set =
    (get_if_open_exn t |> AW.test_and_set) k ~test ~set

  let remove t k = (get_if_open_exn t |> AW.remove) k
  let list t = get_if_open_exn t |> AW.list
  let watch t ?init f = (get_if_open_exn t |> AW.watch) ?init f
  let watch_key t k ?init f = (get_if_open_exn t |> AW.watch_key) k ?init f
  let unwatch t w = (get_if_open_exn t |> AW.unwatch) w

  let close t =
    if !(t.closed) then Lwt.return_unit
    else (
      t.closed := true;
      AW.close t.t)

  let clear t = get_if_open_exn t |> AW.clear
end

module Check_closed (Make_atomic_write : Maker) (K : Type.S) (V : Type.S) =
struct
  module AW = Make_atomic_write (K) (V)
  include Check_closed_store (AW)

  let v conf =
    let+ t = AW.v conf in
    { closed = ref false; t }
end
