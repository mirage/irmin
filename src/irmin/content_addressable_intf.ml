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

module type S = sig
  (** {1 Content-addressable stores}

      Content-addressable stores are store where it is possible to read and add
      new values. Keys are derived from the values raw contents and hence are
      deterministic. *)

  include Read_only.S
  (** @inline *)

  val add : [> write ] t -> value -> key Lwt.t
  (** Write the contents of a value to the store. It's the responsibility of the
      content-addressable store to generate a consistent key. *)

  val unsafe_add : [> write ] t -> key -> value -> unit Lwt.t
  (** Same as {!add} but allows to specify the key directly. The backend might
      choose to discared that key and/or can be corrupt if the key scheme is not
      consistent. *)

  include Clearable with type 'a t := 'a t
  (** @inline *)

  include Closeable with type 'a t := 'a t
  (** @inline *)

  include Batch with type 'a t := 'a t
  (** @inline *)
end

module type Maker = functor (K : Hash.S) (V : Type.S) -> sig
  include S with type key = K.t and type value = V.t

  include Of_config with type 'a t := 'a t
  (** @inline *)
end

module type Sigs = sig
  module type S = S
  module type Maker = Maker

  module Make (X : Append_only.Maker) (K : Hash.S) (V : Type.S) : sig
    include
      S with type 'a t = 'a X(K)(V).t and type key = K.t and type value = V.t

    include Of_config with type 'a t := 'a t
    (** @inline *)
  end

  module Check_closed (S : S) : sig
    (** @inline *)
    include S with type key = S.key and type value = S.value

    (** @inline *)
    include Checkable with type 'a t := 'a t and type 'a raw := 'a S.t
  end
end
