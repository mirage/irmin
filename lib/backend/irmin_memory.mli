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

module type BC_MAKER =
  functor (K: Irmin.Sig.Uid)      ->
  functor (C: Irmin.Sig.Contents) ->
  functor (T: Irmin.Sig.Tag)      ->
  sig
    include Irmin.Sig.BC with type value     = C.t
                          and type branch    = T.t
    val clear: unit -> unit
    (** Clear the store. *)
  end

module type BACKEND = sig
  module RO: Irmin.Sig.RO_MAKER
  module AO: Irmin.Sig.AO_MAKER
  module RW: Irmin.Sig.RW_MAKER
  module BC: BC_MAKER
end

include BACKEND

module Fresh (X: sig end): BACKEND
