(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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

module type S = sig
  type t
  type elt

  val add : t -> elt -> [ `Ok | `Duplicate ]
  (** [add t elt] adds [elt] to the set [t] and returns [`Ok] if [elt] is not
      already a member of [t], otherwise returns [`Duplicate] and leaves the
      hashset unchanged. *)

  val add_exn : t -> elt -> unit
  (** [add_exn t elt] adds [elt] to the set [t].

      @raise Invalid_argument if [elt] is already a member of [t]. *)

  val mem : t -> elt -> bool
  (** [mem t elt] is [true] iff [elt] has been added to the hashset. *)
end
