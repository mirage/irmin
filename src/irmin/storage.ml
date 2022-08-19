(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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
include Storage_intf

module Read_only (M : Make) =
functor
  (K : Type.S)
  (V : Type.S)
  ->
  struct
    module S = M (K) (V)

    type 'a t = S.t
    type key = S.key
    type value = S.value

    let v = S.v
    let mem = S.mem
    let find = S.find
    let close = S.close
  end

module Content_addressable (M : Make) : Content_addressable.Maker =
functor
  (H : Hash.S)
  (V : Type.S)
  ->
  struct
    include Read_only (M) (H) (V)
    module H = Hash.Typed (H) (V)

    let batch = S.batch

    let add t value =
      let key = H.hash value in
      let+ () = S.set t key value in
      key

    let equal_hash = Type.(equal H.t |> unstage)
    let pp_hash = Type.(pp H.t)

    let unsafe_add t k v =
      let+ hash' = add t v in
      if equal_hash k hash' then ()
      else
        Fmt.failwith
          "[unsafe_append] %a is not a valid key. Expecting %a instead.\n"
          pp_hash k pp_hash hash'
  end

module Append_only (M : Make) : Append_only.Maker =
functor
  (Key : Type.S)
  (Value : Type.S)
  ->
  struct
    include Read_only (M) (Key) (Value)

    let batch = S.batch
    let add = S.set
  end

module Atomic_write (M : Make) : Atomic_write.Maker =
functor
  (Key : Type.S)
  (Value : Type.S)
  ->
  struct
    module S = M (Key) (Value)
    module W = Watch.Make (Key) (Value)
    module L = Lock.Make (Key)

    type t = { t : S.t; w : W.t; l : L.t }
    type key = S.key
    type value = S.value
    type watch = W.watch

    let watches = W.v ()
    let lock = L.v ()

    let v config =
      let* t = S.v config in
      Lwt.return { t; w = watches; l = lock }

    let find { t; _ } = S.find t
    let mem { t; _ } = S.mem t

    module Internal = struct
      let set t w key value =
        let* () = S.set t key value in
        W.notify w key (Some value)

      let remove t w key =
        let* () = S.remove t key in
        W.notify w key None
    end

    let list { t; _ } = S.keys t

    let set { t; l; w } key value =
      L.with_lock l key @@ fun () -> Internal.set t w key value

    let remove { t; l; w } key =
      L.with_lock l key @@ fun () -> Internal.remove t w key

    let test_and_set =
      let value_equal = Type.(unstage (equal (option Value.t))) in
      fun { t; l; w } key ~test ~set:set_value ->
        L.with_lock l key @@ fun () ->
        let* v = S.find t key in
        if value_equal v test then
          let* () =
            match set_value with
            | Some set_value -> Internal.set t w key set_value
            | None -> Internal.remove t w key
          in
          Lwt.return_true
        else Lwt.return_false

    let watch_key { w; _ } key = W.watch_key w key
    let watch { w; _ } = W.watch w
    let unwatch { w; _ } = W.unwatch w

    let clear { t; w; _ } =
      let* () = W.clear w in
      S.clear t

    let close { t; w; _ } =
      let* () = W.clear w in
      S.close t
  end
