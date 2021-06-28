(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

include Content_addressable_intf
open! Import

(* FIXME: remove code duplication with irmin/content_addressable *)
module Closeable (S : S) = struct
  type 'a t = { closed : bool ref; t : 'a S.t }
  type key = S.key
  type value = S.value

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

  let batch t f =
    check_not_closed t;
    S.batch t.t (fun w -> f { t = w; closed = t.closed })

  let close t =
    if !(t.closed) then Lwt.return_unit
    else (
      t.closed := true;
      S.close t.t)

  let unsafe_append ~ensure_unique ~overcommit t k v =
    check_not_closed t;
    S.unsafe_append ~ensure_unique ~overcommit t.t k v

  let unsafe_mem t k =
    check_not_closed t;
    S.unsafe_mem t.t k

  let unsafe_find ~check_integrity t k =
    check_not_closed t;
    S.unsafe_find ~check_integrity t.t k

  let clear t =
    check_not_closed t;
    S.clear t.t

  let generation t =
    check_not_closed t;
    S.generation t.t

  let clear_keep_generation t =
    check_not_closed t;
    S.clear_keep_generation t.t

  let make_closeable t = { closed = ref false; t }

  let get_open_exn t =
    check_not_closed t;
    t.t
end
