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

(** Implementation of keys *)

open Core_kernel.Std

exception Invalid of string
(** Exception raised when a key is not valid. *)

exception Unknown of string
(** Exception raised when no value is associated to a key. *)

module type S = sig

  (** Signature for deterministic keys. *)

  include IrminIdent.S

  val of_raw: string -> t
  (** Cast a raw string into a key. Check that the format of the raw
      string is valid. Raise [Invalid 'key'] if that's not the case. *)

  val to_raw: t -> string
  (** Return the raw key. *)

  val of_bytes: Bigstring.t -> t
  (** Compute a (deterministic) key from a bigstring. *)

  val of_bytes': string -> t
  (** Compute a (deterministic) key from a sequence of bytes. *)

end

module SHA1: S
(** SHA1 keys *)
