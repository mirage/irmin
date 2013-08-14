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

module type S = sig
  include KEY
  include IO with type t := t
end

type t = K of string
type key = t

module SHA1 (C: CHANNEL) = struct

  type t = key

  type channel = C.t

  let compare (K k1) (K k2) = String.compare k1 k2

  let hash (K k) = Hashtbl.hash k

  let pretty (K k) =
    Printf.sprintf "%s" (IrminMisc.hex_encode k)

  let to_json (K k) =
    IrminJSON.of_string k

  let of_json j =
    K (IrminJSON.to_string j)

  let create value =
    let str = Marshal.to_string value [] in
    K (IrminMisc.sha1 str)

  let key_length = 20

  let length (K _) = key_length

  let read fd =
    lwt key = C.read_string fd key_length in
    Lwt.return (K key)

  let write fd (K k) =
    C.write_string fd k

end
