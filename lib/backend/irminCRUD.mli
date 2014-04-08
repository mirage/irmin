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

(** JSON CRUD interface. *)

open Core_kernel.Std

module type Jsonable = sig
  include Identifiable.S
  val to_json: t -> Ezjsonm.t
  val of_json: Ezjsonm.t -> t
end

module type S = sig

  (** Signature for CRUD interfaces. *)

  module type U = sig

    val uri: Uri.t
    (** The server URI. *)

  end

  module RO (U: U) (K: IrminKey.S) (V: Jsonable):
    IrminStore.RO with type key = K.t and type value = V.t
  (** Build a read-only store using the given url. *)

  module AO (U: U) (K: IrminKey.S) (V: Jsonable):
    IrminStore.AO with type key = K.t and type value = V.t
  (** Build an append-only store using the given url. *)

  module RW (U: U) (K: IrminKey.S) (V: Jsonable):
    IrminStore.RW with type key = K.t and type value = V.t
  (** Build an a mutable store using the given url. *)

  module S (U: U)
      (K: IrminKey.S)
      (V: Jsonable)
      (S: IrminKey.S)
      (B: IrminKey.S)
      (D: Jsonable)
    : IrminStore.S with type key = K.t
                    and type value = V.t
                    and type snapshot = S.t
                    and type branch = B.t
                    and type dump = D.t
  (** Build an irminsule store using the given uri. *)

  val create: [`JSON|`String] -> Uri.t -> (module Irmin.S)
  (** Create a store using a JSON CRUD interface on the given uri. *)

end

module Make (C: Cohttp_lwt.Client): S
(** Build a CRUD client using the given cohttp client
    implementation. *)
