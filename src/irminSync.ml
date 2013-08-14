(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open IrminTypes

module Make (KS: KEY_STORE) (TS: TAG_STORE with type key = KS.key) = struct

  (** Type of keys *)
  type key = KS.key

  (** Graph of keys *)
  type graph = key list * (key * key) list

  (** Type of remote tags *)
  type tag = TS.tag

  (** Dummy channels *)
  type t = unit

  let pull_keys () _ =
    failwith "TODO"

  let pull_tags () =
    failwith "TODO"

  let push_keys () _ =
    failwith "TODO"

  let push_tags () _ =
    failwith "TODO"

  let watch () _ =
    failwith "TODO"

end
