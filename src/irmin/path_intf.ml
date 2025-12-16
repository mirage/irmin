(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type S = sig
  (** {1 Path} *)

  type t
  (** The type for path values. *)

  type step
  (** Type type for path's steps. *)

  val empty : t
  (** The empty path. *)

  val v : step list -> t
  (** Create a path from a list of steps. *)

  val is_empty : t -> bool
  (** Check if the path is empty. *)

  val cons : step -> t -> t
  (** Prepend a step to the path. *)

  val rcons : t -> step -> t
  (** Append a step to the path. *)

  val decons : t -> (step * t) option
  (** Deconstruct the first element of the path. Return [None] if the path is
      empty. *)

  val rdecons : t -> (t * step) option
  (** Deconstruct the last element of the path. Return [None] if the path is
      empty. *)

  val map : t -> (step -> 'a) -> 'a list
  (** [map t f] maps [f] over all steps of [t]. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!type-t}. *)

  val step_t : step Type.t
  (** [step_t] is the value type for {!step}. *)
end

module type Sigs = sig
  module type S = S
  (** Signature for path implementations.*)

  (** An implementation of paths as string lists. *)
  module String_list : S with type step = string and type t = string list
end
