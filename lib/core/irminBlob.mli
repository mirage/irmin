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

open Core_kernel.Std

exception Conflict
(** Exception raised during merge conflicts. *)

exception Invalid of string
(** Invalid parsing. *)

module type S = sig

  (** Signature for values. *)

  include Identifiable.S
  (** Base types. *)

  val to_json: t -> Ezjsonm.t
  (** Convert a blob to JSON. *)

  val of_json: Ezjsonm.t -> t
  (** Read a blob which has been JSON encoded. *)

  val of_bytes: string -> t option
  (** Convert a raw sequence of bytes into a value. Return [None] if
      the sequence cannot be decoded. *)

  val of_bytes_exn: string -> t
  (** Same as [of_bytes] but raise [Invalid] if the sequence of bytes
      does not correspond to a valid blob. *)

  val merge: old:t -> t -> t -> t
  (** Merge function. *)

end

module String: S with type t = string
(** String values where only the last modified value is kept on
    merge. If the value has been modified concurrently, the [merge]
    function raises [Conflict]. *)

module JSON: S with type t = Ezjsonm.t
(** JSON values where only the last modified value is kept on
    merge. If the value has been modified concurrently, the [merge]
    function raises [Conflict]. *)

(** JSON values. *)
module type STORE = sig

  include IrminStore.AO
  (** Blob stores are append-only. *)

  module Key: IrminKey.S with type t = key
  (** Base functions for keys. *)

  module Value: S with type t = value
  (** Base functions for values. *)

end
(** Blobs are stored in append-only stores. *)

module Make
    (K: IrminKey.S)
    (B: S)
    (Blob: IrminStore.AO with type key = K.t and type value = B.t)
  : STORE with type t = Blob.t
           and type key = K.t
           and type value = B.t
(** Build a blob store. *)
