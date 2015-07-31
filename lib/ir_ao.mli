(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Append-only stores. *)

module type STORE = sig
  include Ir_ro.STORE
  val add: t -> value -> key Lwt.t
end

module type MAKER =
  functor (K: Ir_hash.S) ->
  functor (V: Tc.S0) ->
    STORE with type key = K.t and type value = V.t

module type RAW = Tc.S0 with type t = Cstruct.t					
module type AO_MAKER_RAW =
  functor (K: Ir_hash.S) ->
  functor (V: RAW) ->
  STORE with type key = K.t and type value = V.t						 
module type STORE_LINK = sig
  include Ir_ro.STORE
  val add: t -> key -> value -> unit Lwt.t
end

module type AO_LINK_MAKER =
  functor (K: Ir_hash.S) ->
  STORE_LINK with type key = K.t and type value = K.t
