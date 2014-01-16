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

  type key
  (** Abstract type for keys. *)

  val key: t -> key
  (** Compute the blob key. *)

  val of_bytes: string -> t
  (** Convert a raw sequence of bytes into a value. *)

  val merge: old:t -> t -> t -> t
  (** Merge function. *)

end

module Simple: S with type key = IrminKey.SHA1.t
                  and type t = string
(** String values with SHA1 hashes, where only the last modified value
    is kept on merge. If the value has been modified concurrently, the
    [merge] function raises [Conflict]. *)

module type STORE = sig

  include IrminStore.AO
  (** Blob stores are append-only. *)

  module Key: IrminKey.S with type t = key
  (** Base functions for keys. *)

  module Value: S with type key = key and type t = value
  (** Base functions for values. *)

end
(** Blobs are stored in append-only stores. *)

module Make
    (K: IrminKey.S)
    (B: S with type key = K.t)
    (Blob: IrminStore.AO with type key = K.t and type value = B.t)
  : STORE with type t = Blob.t
           and type key = K.t
           and type value = B.t
(** Build a blob store. *)
