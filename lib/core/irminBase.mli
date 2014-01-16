(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Core_kernel.Std

(** Base types *)

module type S = sig

  (** Base elements. *)

  type t
  (** Abstract base values. *)

  include Identifiable.S with type t := t

  val name: string
  (** Name of the element kind. *)

  val pretty: t -> string
  (** Pretty-printing *)

  val of_json: Ezjsonm.t -> t
  (** Convert from JSON *)

  val to_json: t -> Ezjsonm.t
  (** Convert to JSON *)

  val sizeof: t -> int
  (** Size of serialized value (to pre-allocate bufIO). *)

  val get: Mstruct.t -> t option
  (** Unmarshal from a buffer. Return [None] if it is not a valid
      marshaled value. *)

  val set: Mstruct.t -> t -> unit
  (** Marshal to a buffer. *)

end

(** {2 Lifts} *)

module String: S with type t = string
(** Abstract strings. *)

module List (E: S): S with type t = E.t list
(** Lift IO operation to lists. *)

module Option (E: S): S with type t = E.t option
(** Lift IO operation to options. *)

module Pair (K: S) (V: S): S with type t = K.t * V.t
(** Lift IO operations to pairs. *)
