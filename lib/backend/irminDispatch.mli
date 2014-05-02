(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Dispatch between multiple backends. *)

module Make
    (K: IrminKey.S)
    (C: IrminContents.S)
    (R: IrminReference.S) :
sig

  (** Dispatch based on the SHA1. All the backend should use the same
      function to compute a key from a value, and every dispatch
      usually involves computing the hash of the value to know which
      backend to choose, so it might be a bit slow. *)


  (** This kind of low-level dispatch can be used to distribute the
      values based on their hash, typically using a DHT implementation
      (which is not provided here). *)

  type value = (K.t, C.t) IrminValue.t
  (** Type of values. *)

  val create: key:(K.t -> int) -> value:(value -> (K.t -> unit) * int) ->
    (K.t, C.t, R.t) Irmin.t array ->  (K.t, C.t, R.t) Irmin.t
  (** [create ~key ~value backends] create a new store where every reads
      and writes will be dispatched to one of the [backends]. The [key]
      function return the offset of the backend in [backends] to read
      from. The [value] function returns a callback function to register the
      key of the value, and the offset of the backend in [backends] to write
      to. *)


  val cast: (K.t, C.t, R.t) Irmin.t -> (module Irmin.S)
  (** Cast to an abstract Irminsule store. *)

end
