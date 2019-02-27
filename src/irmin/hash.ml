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

module Make (H: Digestif.S) = struct

  type t = H.t

  external get_64: string -> int -> int64 = "%caml_string_get64u"

  let hash c = Int64.to_int (get_64 (H.to_raw_string c) 0)
  let digest_size = H.digest_size
  let digest x = H.digest_string x

  let of_hex s = match H.consistent_of_hex s with
    | x -> Ok x
    | exception Invalid_argument e -> Error (`Msg e)

  let pp_hex ppf x = Fmt.string ppf (H.to_hex x)

  let t =
    Type.like_map ~cli:(pp_hex, of_hex)
      Type.(string_of (`Fixed digest_size))
      H.of_raw_string H.to_raw_string
end

module SHA1 = Make(Digestif.SHA1)
module RMD160 = Make(Digestif.RMD160)
module SHA224 = Make(Digestif.SHA224)
module SHA256 = Make(Digestif.SHA256)
module SHA384 = Make(Digestif.SHA384)
module SHA512 = Make(Digestif.SHA512)
module BLAKE2B = Make(Digestif.BLAKE2B)
module BLAKE2S = Make(Digestif.BLAKE2S)

module With_digest (K: S.HASH) (V: Type.S) = struct
  include K
  let digest v = K.digest (Type.to_bin_string V.t v)
end
