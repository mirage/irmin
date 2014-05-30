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

(** In-memory store *)

module type S_MAKER =
  functor (K: IrminKey.S)      ->
  functor (C: IrminContents.S) ->
  functor (T: IrminTag.S)      ->
    sig
      include Irmin.S with type Block.key = K.t
                       and type value     = C.t
                       and type branch    = T.t
      val clear: unit -> unit
      (** Clear the store. *)
    end

module type BACKEND = sig
  module RO  : Irmin.RO_MAKER
  module AO  : Irmin.AO_MAKER
  module RW  : Irmin.RW_MAKER
  module Make: S_MAKER
end

include BACKEND

module Fresh (X: sig end): BACKEND
