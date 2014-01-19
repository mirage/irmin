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

(** Store dumps. *)

open Core_kernel.Std

type ('key, 'blob) t = ('key * ('key, 'blob) IrminValue.t) list
(** Dump values. *)

val of_json: (Ezjsonm.t -> 'a) -> (Ezjsonm.t -> 'b) -> Ezjsonm.t -> ('a, 'b) t
val to_json: ('a -> Ezjsonm.t) -> ('b -> Ezjsonm.t) -> ('a, 'b) t -> Ezjsonm.t

module type S = sig

  (** Signature for dump values .*)

  type key
  (** Keys. *)

  type blob
  (** Blobs. *)

  include Identifiable.S with type t = (key, blob) t
  (** Base functions over dump values. *)

  val of_json: Ezjsonm.t -> t
  val to_json: t -> Ezjsonm.t

end

module S (K: IrminKey.S) (B: IrminBlob.S):
  S with type key = K.t and type blob = B.t
(** Base functions over dump values. *)
