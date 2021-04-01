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

include Atomic_write_intf
open Import

module Wrap_close (S : S) = struct
  module X = struct
    include S

    type 'a t = S.t
  end

  include Read_only.Wrap_close (X)

  type nonrec t = read t

  let s t = fst (raw t)

  let set t k v =
    check_not_closed t;
    S.set (s t) k v

  let test_and_set t k ~test ~set =
    check_not_closed t;
    S.test_and_set (s t) k ~test ~set

  let remove t k =
    check_not_closed t;
    S.remove (s t) k

  let list t =
    check_not_closed t;
    S.list (s t)

  let clear t =
    check_not_closed t;
    S.clear (s t)

  type watch = S.watch

  let watch t ?init f =
    check_not_closed t;
    S.watch (s t) ?init f

  let watch_key t k ?init f =
    check_not_closed t;
    S.watch_key (s t) k ?init f

  let unwatch t w =
    check_not_closed t;
    S.unwatch (s t) w
end
