(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

module Append_only (S : S.APPEND_ONLY_STORE) = struct
  type 'a t = { closed : bool ref; t : 'a S.t }

  type key = S.key

  type value = S.value

  type ctx = S.ctx

  let check_not_closed t = if !(t.closed) then raise Irmin.Closed

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

  let v ?ctx uri item items =
    S.v ?ctx uri item items >|= fun t -> { closed = ref false; t }

  let close t =
    if !(t.closed) then Lwt.return_unit
    else (
      t.closed := true;
      S.close t.t)

  let batch t f =
    check_not_closed t;
    S.batch t.t (fun w -> f { t = w; closed = t.closed })
end

module Atomic_write (S : S.ATOMIC_WRITE_STORE) = struct
  type t = { closed : bool ref; t : S.t }

  type key = S.key

  type value = S.value

  type ctx = S.ctx

  let check_not_closed t = if !(t.closed) then raise Irmin.Closed

  let mem t k =
    check_not_closed t;
    S.mem t.t k

  let find t k =
    check_not_closed t;
    S.find t.t k

  let set t k v =
    check_not_closed t;
    S.set t.t k v

  let test_and_set t k ~test ~set =
    check_not_closed t;
    S.test_and_set t.t k ~test ~set

  let remove t k =
    check_not_closed t;
    S.remove t.t k

  let list t =
    check_not_closed t;
    S.list t.t

  type watch = S.watch

  let watch t ?init f =
    check_not_closed t;
    S.watch t.t ?init f

  let watch_key t k ?init f =
    check_not_closed t;
    S.watch_key t.t k ?init f

  let unwatch t w =
    check_not_closed t;
    S.unwatch t.t w

  let v ?ctx uri item items =
    S.v ?ctx uri item items >|= fun t -> { closed = ref false; t }

  let close t =
    if !(t.closed) then Lwt.return_unit
    else (
      t.closed := true;
      S.close t.t)
end
