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

include Hash_intf

module Make (H : Digestif.S) = struct
  type t = H.t

  external get_64 : string -> int -> int64 = "%caml_string_get64u"
  external swap64 : int64 -> int64 = "%bswap_int64"

  let get_64_little_endian str idx =
    if Sys.big_endian then swap64 (get_64 str idx) else get_64 str idx

  let short_hash c = Int64.to_int (get_64_little_endian (H.to_raw_string c) 0)

  let short_hash_substring bigstring ~off =
    Int64.to_int (Bigstringaf.get_int64_le bigstring off)

  let hash_size = H.digest_size

  let of_hex s =
    match H.consistent_of_hex s with
    | x -> Ok x
    | exception Invalid_argument e -> Error (`Msg e)

  let pp_hex ppf x = Fmt.string ppf (H.to_hex x)

  let t =
    Type.map ~pp:pp_hex ~of_string:of_hex
      Type.(string_of (`Fixed hash_size))
      H.of_raw_string H.to_raw_string

  let hash s = H.digesti_string s
  let to_raw_string s = H.to_raw_string s
  let unsafe_of_raw_string s = H.of_raw_string s
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

module Typed (K : S) (V : Type.S) = struct
  include K

  type value = V.t [@@deriving irmin ~pre_hash]

  let hash v = K.hash (pre_hash_value v)
end

module V1 (K : S) : S with type t = K.t = struct
  type t = K.t [@@deriving irmin ~encode_bin ~decode_bin]

  let hash = K.hash
  let short_hash = K.short_hash
  let short_hash_substring = K.short_hash_substring
  let hash_size = K.hash_size
  let int64_to_bin_string = Type.(unstage (to_bin_string int64))
  let hash_size_str = int64_to_bin_string (Int64.of_int K.hash_size)
  let to_raw_string = K.to_raw_string
  let unsafe_of_raw_string = K.unsafe_of_raw_string

  let encode_bin e f =
    f hash_size_str;
    encode_bin e f

  let decode_bin buf pos_ref =
    pos_ref := !pos_ref + 8;
    decode_bin buf pos_ref

  let size_of = Type.Size.custom_static (8 + hash_size)
  let t = Type.like K.t ~bin:(encode_bin, decode_bin, size_of)
end

module Set = struct
  module Make (Hash : S) = struct
    include Irmin_data.Fixed_size_string_set

    let create ?(initial_slots = 0) () =
      let elt_length = Hash.hash_size
      and hash s = Hash.(short_hash (unsafe_of_raw_string s))
      and hash_substring t ~off ~len:_ = Hash.short_hash_substring t ~off in
      create ~elt_length ~initial_slots ~hash ~hash_substring ()

    let add t h = add t (Hash.to_raw_string h)
    let mem t h = mem t (Hash.to_raw_string h)
  end

  module type S = Set
end
