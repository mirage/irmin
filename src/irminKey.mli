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

(** Implementation of keys *)

module type S = sig

  (** Signature for keys. *)

  include IrminBase.S

  val of_bytes: string -> t
  (** Compute a key from a sequence of bytes. *)

  val of_bar: IrminBuffer.t -> t
  (** Compute a key from a bigbuffer. *)

  val to_hex: t -> string
  (** Convert a key to an hexa representation. *)

  val of_hex: string -> t
  (** Convert an hexa representation to a key. *)

  val concat: t list -> t
  (** Compute a key from a list of keys. *)

  val length: t -> int
  (** Compute the key length. *)

  module Graph: IrminGraph.S with type Vertex.t = t
  (** Graph of keys *)

end

module SHA1: S with type t = private string
(** SHA1 keys *)
