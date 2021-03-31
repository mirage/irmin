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

include Read_only_intf

module Check_closed (S : S) = struct
  type -'a t = { closed : bool ref; t : 'a S.t }

  let check_not_closed t = if !(t.closed) then raise Store_properties.Closed

  type key = S.key
  type value = S.value

  let is_closed t = !(t.closed)

  let mem t k =
    check_not_closed t;
    S.mem t.t k

  let find t k =
    check_not_closed t;
    S.find t.t k

  let v ?(closed = ref false) t = { closed; t }

  let close t =
    if !(t.closed) then Lwt.return_unit
    else (
      t.closed := true;
      S.close t.t)

  let raw t = t.t
end
