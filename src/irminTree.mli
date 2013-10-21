(*
 * Copyright (c) 2013 Louis Gesbert     <louis.gesbert@ocamlpro.com>
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

(** Tree-like structures of values. *)

module type S = sig

  type revision
  (** Type of revisions. *)

  type value
  (** Type of valuse. *)

  include IrminBase.STORE with type value := revision

  val create: ?value:value -> revision list -> revision
  (** Create a new revision. *)

  val write: t -> revision -> key -> unit Lwt.t
      (** *)

  val read: t -> key -> revision option Lwt.t

end


(** Create an implementation for trees using [K] as keys and [S] as
    store.. *)
module Make
    (S: IrminBase.STORE with type value = IrminBuffer.S)
    (V: IrminValue.Store):
  S with type key = S.key

module Simple: S with type key = IrminKey.SHA1.t
(** Simple tree implementation where keys are SHA1 of values. *)

(** {2 Store} *)
