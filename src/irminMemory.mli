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

(** In-memory store *)

open IrminTypes

(** Functor to build an in-memory low-level store *)
module Key_store (C: CORE): sig

  include KEY_STORE with module C = C

  (** Create a fresh store *)
  val create: unit -> t
end

(** Functor to build an in-memory low-level store *)
module Value_store (C: CORE): sig

  include VALUE_STORE with module C = C

  (** Create a fresh store *)
  val create: unit -> t
end

(** Functor to build an in-memory tag store. *)
module Tag_store (C: CORE): sig

  include TAG_STORE with module C = C

  (** Create a fresh store *)
  val create: unit -> t
end

module type S = sig

  (** In-memory store *)
  include STORE

  (** Create a fresh store *)
  val create: unit -> t

end


(** Functor to build an in-memory global store. *)
module Make (C: CORE): S with module C = C
