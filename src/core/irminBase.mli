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

(** Base types *)

module type S = sig

  (** Base types of elements. *)

  type t
  (** Abstract type *)

  val name: string
  (** Component name. *)

  val compare: t -> t -> int
  (** Compare two elements. *)

  val equal: t -> t -> bool
  (** Are two elements equal ? *)

  val hash: t -> int
  (** Compute the hash of an element. *)

  val pretty: t -> string
  (** Pretty-printing *)

  val to_string: t -> string
  (** Convert the contents to a string. *)

  val of_json: IrminJSON.t -> t
  (** Convert from JSON *)

  val to_json: t -> IrminJSON.t
  (** Convert to IrminJSON *)

  val sizeof: t -> int
  (** Size of serialized value (to pre-allocate bufIO). *)

  val get: IrminBuffer.t -> t option
  (** Unmarshal from a buffer. Return [None] if it is not a valid
      marshaled value. *)

  val set: IrminBuffer.t -> t -> unit
  (** Marshal to a buffer. *)

end

module type STRINGABLE = sig

  (** Signature for stringable objects. *)

  type t

  val to_string: t -> string
  (** Convert an element to a string. *)

  val of_string: string -> t
  (** Convert a string to an element. *)

end

(** {2 String Elements} *)

module PrivateString: sig

  (** Lift IO operations to private strings *)

  include S with type t = private string
  (** Base types. *)

  val to_string: t -> string
  (** Convert to string. *)

  val of_string: string -> t
  (** Convert from string. *)

end

module String: (module type of PrivateString with type t = string)
(** Lift IO operations to strings *)

(** {2 Lifts} *)

module List (E: S): S with type t = E.t list
(** Lift IO operation to lists. *)

module Option (E: S): S with type t = E.t option
(** Lift IO operation to options. *)

module Pair (K: S) (V: S): S with type t = K.t * V.t
(** Lift IO operations to pairs. *)
