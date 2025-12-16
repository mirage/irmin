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

module type S = sig
  (** Signature for digest hashes, inspired by Digestif. *)

  type t
  (** The type for digest hashes. *)

  val hash : ((string -> unit) -> unit) -> t
  (** Compute a deterministic store key from a sequence of strings. *)

  val short_hash : t -> int
  (** [short_hash h] is a small hash of [h], to be used for instance as the
      `hash` function of an OCaml [Hashtbl]. *)

  val hash_size : int
  (** [hash_size] is the size of hash results, in bytes. *)

  val to_raw_string : t -> string
  (** [to_raw_string t] is the raw sequence of bytes in [t] (of length
      {!hash_size}). *)

  val unsafe_of_raw_string : string -> t
  (** [unsafe_of_raw_string b] is the hash consisting of the raw sequence of
      bytes [b].

      {b Warning}: this function cannot guarantee that the supplied byte string
      is a valid output of the hash process, so should only be used on strings
      that are known to have been built with {!to_raw_string}. *)

  val short_hash_substring : Bigstringaf.t -> off:int -> int
  (** [short_hash_substring t off] computes the short-hash of the raw hash data
      contained in [t] at offset [off]. It has behaviour equivalent to:

      {[
        Bigstringaf.substring t ~off ~len:hash_size
        |> unsafe_of_raw_string
        |> short_hash
      ]}

      but may be more efficient due to not needing to allocate an intermediate
      [string]. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!type-t}. *)
end

module type Typed = sig
  type t
  type value

  val hash : value -> t
  (** Compute a deterministic store key from a string. *)

  val short_hash : t -> int
  (** [short_hash h] is a small hash of [h], to be used for instance as the
      `hash` function of an OCaml [Hashtbl]. *)

  val hash_size : int
  (** [hash_size] is the size of hash results, in bytes. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!type-t}. *)
end

module type Set = sig
  type t
  type hash

  val create : ?initial_slots:int -> unit -> t
  val add : t -> hash -> [ `Ok | `Duplicate ]
  val mem : t -> hash -> bool
end

module type Sigs = sig
  module type S = S
  (** Signature for hash values. *)

  module type Typed = Typed
  (** Signature for typed hashes, where [hash] directly takes a value as
      argument and incremental hashing is not possible. *)

  (** Digestif hashes. *)
  module Make (H : Digestif.S) : S with type t = H.t

  module Make_BLAKE2B (D : sig
    val digest_size : int
  end) : S

  module Make_BLAKE2S (D : sig
    val digest_size : int
  end) : S

  module SHA1 : S
  module RMD160 : S
  module SHA224 : S
  module SHA256 : S
  module SHA384 : S
  module SHA512 : S
  module BLAKE2B : S
  module BLAKE2S : S

  (** v1 serialisation *)
  module V1 (H : S) : S with type t = H.t

  (** Typed hashes. *)
  module Typed (K : S) (V : Type.S) :
    Typed with type t = K.t and type value = V.t

  module Set : sig
    module Make (Hash : S) : Set with type hash := Hash.t

    module type S = Set
  end
end
