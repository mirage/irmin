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

open Import
open Store_properties
module Irmin_key = Key

module type S = sig
  (** {1 Append-only stores}

      Append-only stores are store where it is possible to read and add new
      values. *)

  type -'a t
  type hash
  type key
  type value

  val mem : [> read ] t -> key -> bool Lwt.t
  val find : [> read ] t -> key -> value option Lwt.t
  val index : [> read ] t -> hash -> key option Lwt.t

  val add : [> write ] t -> hash -> value -> key Lwt.t
  (** Write the contents of a value to the store. *)

  include Clearable with type 'a t := 'a t
  (** @inline *)

  include Closeable with type 'a t := 'a t
  (** @inline *)

  include Batch with type 'a t := 'a t
  (** @inline *)
end

module Append_only_is_a_read_only (X : S) : Read_only.S = X

module type Maker = functor (Hash : Hash.S) (Value : Type.S) -> sig
  module Key :
    Key.Hash_like with type t = Key.Of_hash(Hash).t and type hash = Hash.t

  include
    S with type value = Value.t and type hash = Hash.t and type key = Key.t

  include Of_config with type 'a t := 'a t
  (** @inline *)
end

module type Sigs = sig
  module type S = S
  module type Maker = Maker
end
