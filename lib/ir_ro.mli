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

(** Read-only stores. *)

module type STORE = sig
  type t
  type key
  type value
  type origin
  val create: unit -> t Lwt.t
  val read: t -> origin -> key -> value option Lwt.t
  val read_exn: t -> origin -> key -> value Lwt.t
  val mem: t -> origin -> key -> bool Lwt.t
  val list: t -> origin -> key list -> key list Lwt.t
  val dump: t -> origin -> (key * value) list Lwt.t
end

module type BINARY = STORE
  with type key = Cstruct.t
   and type value = Cstruct.t
   and type origin = Cstruct.t
(** Binary read-only stores. Keys, values and origin are cstruct
    buffers. *)

module type JSON = STORE
  with type key = Ezjsonm.t
   and type value = Ezjsonm.t
   and type origin = Ezjsonm.t
(** Binary read-only stores. Keys, values and origin are cstruct
    buffers. *)

module type MAKER =
  functor (K: Tc.I0) ->
  functor (V: Tc.I0) ->
  functor (O: Tc.I0) ->
    STORE with type key = K.t and type value = V.t and type origin = O.t
(** Signature for functor creating read-only stores. *)

module Binary (S: BINARY) (K: Tc.I0) (V: Tc.I0) (O: Tc.I0):
  STORE with type t = S.t
         and type key = K.t
         and type value = V.t
         and type origin = O.t
(** Create a typed read-only store from a binary one. *)

module Json (S: JSON) (K: Tc.I0) (V: Tc.I0) (O: Tc.I0):
  STORE with type t = S.t
         and type key = K.t
         and type value = V.t
         and type origin = O.t
(** Create a typed read-only store from a JSON one. *)
