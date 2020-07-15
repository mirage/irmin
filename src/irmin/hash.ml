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

module Make (H : Digestif.S) = struct
  type t = H.t

  external get_64 : string -> int -> int64 = "%caml_string_get64u"

  let short_hash c = Int64.to_int (get_64 (H.to_raw_string c) 0)

  let hash_size = H.digest_size

  let of_hex s =
    match H.consistent_of_hex s with
    | x -> Ok x
    | exception Invalid_argument e -> Error (`Msg e)

  let pp_hex ppf x = Fmt.string ppf (H.to_hex x)

  let t =
    Type.map ~cli:(pp_hex, of_hex)
      Type.(string_of (`Fixed hash_size))
      H.of_raw_string H.to_raw_string

  let hash s = H.digesti_string s
end

module Make_BLAKE2B (D : sig
  val digest_size : int
end) =
  Make (Digestif.Make_BLAKE2B (D))
module Make_BLAKE2S (D : sig
  val digest_size : int
end) =
  Make (Digestif.Make_BLAKE2S (D))
module SHA1 = Make (Digestif.SHA1)
module RMD160 = Make (Digestif.RMD160)
module SHA224 = Make (Digestif.SHA224)
module SHA256 = Make (Digestif.SHA256)
module SHA384 = Make (Digestif.SHA384)
module SHA512 = Make (Digestif.SHA512)
module BLAKE2B = Make (Digestif.BLAKE2B)
module BLAKE2S = Make (Digestif.BLAKE2S)

module Typed (K : S.HASH) (V : Type.S) = struct
  include K

  type value = V.t

  let pre_hash = Type.unstage (Type.pre_hash V.t)

  let hash v = K.hash (pre_hash v)
end

module V1 (K : S.HASH) : S.HASH with type t = K.t = struct
  type t = K.t

  let hash = K.hash

  let short_hash = K.short_hash

  let hash_size = K.hash_size

  let h = Type.string_of `Int64

  let to_bin_key = Type.unstage (Type.to_bin_string K.t)

  let of_bin_key = Type.unstage (Type.of_bin_string K.t)

  let size_of =
    let size_of = Type.unstage (Type.size_of h) in
    Type.stage (fun x -> size_of (to_bin_key x))

  let encode_bin =
    let encode_bin = Type.unstage (Type.encode_bin h) in
    Type.stage (fun e -> encode_bin (to_bin_key e))

  let decode_bin =
    let decode_bin = Type.unstage (Type.decode_bin h) in
    Type.stage @@ fun buf off ->
    let n, v = decode_bin buf off in
    ( n,
      match of_bin_key v with
      | Ok v -> v
      | Error (`Msg e) -> Fmt.failwith "decode_bin: %s" e )

  let t = Type.like K.t ~bin:(encode_bin, decode_bin, size_of)
end
