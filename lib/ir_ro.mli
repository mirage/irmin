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
  val create: (string * Ir_univ.t) list -> Ir_task.t -> t
  val config: t -> (string * Ir_univ.t) list
  val task: t -> Ir_task.t
  val read: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t
  val mem: t -> key -> bool Lwt.t
  val list: t -> key list -> key list Lwt.t
  val dump: t -> (key * value) list Lwt.t
end

module type CSTRUCT = STORE
  with type key = Cstruct.t
   and type value = Cstruct.t
(** Binary read-only stores. Keys, values and origin are cstruct
    buffers. *)

module type JSON = STORE
  with type key = Ezjsonm.t
   and type value = Ezjsonm.t
(** Binary read-only stores. Keys, values and origin are cstruct
    buffers. *)

module type MAKER =
  functor (K: Tc.S0) ->
  functor (V: Tc.S0) ->
    STORE with type key = K.t
           and type value = V.t
(** Signature for functor creating read-only stores. *)

module Cstruct (S: CSTRUCT) (K: Tc.S0) (V: Tc.S0):
  STORE with type t = S.t
         and type key = K.t
         and type value = V.t
(** Create a typed read-only store from a binary one. *)

module Json (S: JSON) (K: Tc.S0) (V: Tc.S0):
  STORE with type t = S.t
         and type key = K.t
         and type value = V.t
(** Create a typed read-only store from a JSON one. *)
