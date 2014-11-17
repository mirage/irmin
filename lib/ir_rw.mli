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

(** Read-write stores. *)

module type STORE = sig
  include Ir_ro.STORE
  val update: t -> origin -> key -> value -> unit Lwt.t
  val remove: t -> origin -> key -> unit Lwt.t
  val watch: t -> origin -> key -> value Lwt_stream.t
end

module type MAKER =
  functor (K: Tc.I0) ->
  functor (V: Tc.I0) ->
  functor (O: Tc.I0) ->
  STORE with type key = K.t and type value = V.t and type origin = O.t
(** Signature of functors creating read-write stores. *)

module type BINARY = STORE
  with type key = Cstruct.t
   and type value = Cstruct.t
   and type origin = Cstruct.t
(** Binary read-write store. Keys, values and origins are cstruct buffers. *)

module type JSON = STORE
  with type key = Ezjsonm.t
   and type value = Ezjsonm.t
   and type origin = Ezjsonm.t
(** JSON read-write store. Keys, values and origins are JSON objects. *)

module Binary (S: BINARY) (K: Tc.I0) (V: Tc.I0) (O: Tc.I0):
  STORE with type t = S.t
         and type key = K.t
         and type value = V.t
         and type origin = O.t
(** Create a typed read-write store from a binary one. *)

module Json (S: JSON) (K: Tc.I0) (V: Tc.I0) (O: Tc.I0):
  STORE with type t = S.t
         and type key = K.t
         and type value = V.t
         and type origin = O.t
(** Create a typed read-write store from a JSON one. *)
