(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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
  type t [@@deriving irmin]
  (** The type for keys. *)

  type hash

  val to_hash : t -> hash
end

module type Hash_like = sig
  include S

  val of_hash : hash -> t
end

module Store_spec = struct
  module type S = sig
    type ('h, 'v) contents_key
    type 'h node_key
    type 'h commit_key
  end

  module type Hash_keyed =
    S
      with type ('h, _) contents_key = 'h
       and type 'h node_key = 'h
       and type 'h commit_key = 'h

  module rec Hash_keyed : Hash_keyed = Hash_keyed
end

module type Sigs = sig
  module type S = S
  module type Hash_like = Hash_like

  (** The simplest possible [Key] implementation is just a hash of the
      corresponding value, attaching no additional metadata about the value. *)
  module Of_hash (H : Type.S) : Hash_like with type t = H.t and type hash = H.t

  module Store_spec : sig
    module type S = Store_spec.S
    module type Hash_keyed = Store_spec.Hash_keyed

    module Hash_keyed : Hash_keyed
  end
end
