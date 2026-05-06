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

open Import
open Store_properties

module type S = sig
  (** A {i content-addressable} store is an indexed read-write store in which
      values are keyed directly by their hashes. *)

  include Read_only.S
  (** @inline *)

  val add : [> write ] t -> value -> key Lwt.t
  (** Write the contents of a value to the store. It's the responsibility of the
      content-addressable store to generate a consistent key. *)

  val unsafe_add : [> write ] t -> key -> value -> unit Lwt.t
  (** Same as {!add} but allows specifying the key directly. The backend might
      choose to discard that key and/or can be corrupt if the key scheme is not
      consistent. *)

  include Closeable with type 'a t := 'a t
  (** @inline *)

  include Batch with type 'a t := 'a t
  (** @inline *)
end

module type Maker = functor (Hash : Hash.S) (Value : Type.S) -> sig
  include S with type value = Value.t and type key = Hash.t

  include Of_config with type 'a t := 'a t
  (** @inline *)
end

module type Sigs = sig
  module type S = S
  module type Maker = Maker

  module Make
      (Append_only_maker : Append_only.Maker)
      (Hash : Hash.S)
      (Value : Type.S) : sig
    include
      S
        with type 'a t = 'a Append_only_maker(Hash)(Value).t
         and type value = Value.t
         and type key = Hash.t

    include Of_config with type 'a t := 'a t
    (** @inline *)
  end

  module Check_closed (M : Maker) : Maker
end
