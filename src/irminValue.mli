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

(** Values. *)

(** {2 Base Values} *)

exception Conflict
(** Exception raised during merge conflicts. *)

module type S = sig

  (** Signature for values. *)

  include IrminBase.S
  (** Base types. *)

  val merge: old: t -> t -> t
  (** Merge function. *)

end

module Simple: sig

  (** String base values, where only the last modified value is kept
      on merge. If the value has been modified concurrently, the
      [merge] function raises [Conflict]. *)

  include S

  val create: string -> t
  (** Create a value from a string. *)

end

(** {2 Store} *)

module type STORE = sig

  (** Signature of value stores. *)

  include IrminStore.S

  include S with type t := value

end

module Make (S: IrminStore.S) (K: IrminKey.S) (V: S):
  STORE with type key = K.t
         and type value = V.t
(** Create a value store. *)
