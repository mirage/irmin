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

(** Values. *)

module type S = sig
  include Tc.I0
  type origin
  module Origin: Ir_origin.S with type t = origin
  val merge: (t, origin) Ir_merge.t
end

module String (O: Ir_origin.S): S with type t = string and type origin = O.t
module Json (O: Ir_origin.S): S with type t = Ezjsonm.t and type origin = O.t
module Cstruct (O: Ir_origin.S): S with type t = Cstruct.t and type origin = O.t

module type STORE = sig
  include Ir_ao.STORE
  val merge: t -> (key, origin) Ir_merge.t
  module Key: Ir_hash.S with type t = key
  module Val: S with type t = value and type origin = origin
end

module type MAKER =
  functor (K: Ir_hash.S) ->
  functor (V: S) ->
    STORE with type key = K.t and type value = V.t and type origin = V.origin

module Make (Contents: Ir_ao.MAKER): MAKER

module Rec (S: STORE): S with type t = S.key
