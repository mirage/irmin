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

(** JSON CRUD interface. *)

module type S = sig

  (** Signature for CRUD interfaces. *)

  module type U = sig

    val uri: Uri.t
    (** The server URI. *)

  end

  module A (U: U): IrminStore.A_MAKER
  (** Build an append-only store using the given url. *)

  module M (U: U): IrminStore.M_MAKER
  (** Build an a mutable store using the given url. *)

  module S (U: U): IrminStore.S_MAKER
  (** Build an irminsule store using the given uri. *)

  val simple: Uri.t -> (module Irmin.SIMPLE)
  (** Simple store using a JSON CRUD interface on the given uri. *)

end

module Make (C: Cohttp_lwt.Client): S
(** Build a CRUD client using the given cohttp client
    implementation. *)
