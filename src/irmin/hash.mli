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

(** Digestif hashes. *)
module Make (H : Digestif.S) : S.HASH with type t = H.t

module Make_BLAKE2B (D : sig
  val digest_size : int
end) : S.HASH

module Make_BLAKE2S (D : sig
  val digest_size : int
end) : S.HASH

module SHA1 : S.HASH
module RMD160 : S.HASH
module SHA224 : S.HASH
module SHA256 : S.HASH
module SHA384 : S.HASH
module SHA512 : S.HASH
module BLAKE2B : S.HASH
module BLAKE2S : S.HASH

(** v1 serialisation *)
module V1 (H : S.HASH) : S.HASH with type t = H.t

(** Typed hashes. *)
module Typed (K : S.HASH) (V : Type.S) :
  S.TYPED_HASH with type t = K.t and type value = V.t
