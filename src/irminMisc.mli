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

(** Miscellaneous functions *)

(** Encode a binary string to hexa *)
val hex_encode: string -> string

(** Decode an hexa string to binary *)
val hex_decode: string -> string

(** Compute the sha1 of a binary string *)
val sha1: string -> string

(** Debug *)
val debug: string -> ('a, out_channel, unit) format -> 'a

(** Info *)
val info: string ->  ('a, out_channel, unit) format -> 'a

(** Error *)
val error: string -> ('a, unit, string, unit) format4 -> 'a

(** Is debug enabled ? *)
val debug_enabled: unit -> bool

(** Set the debug mode (default is the value of [IRMIN_DEBUG] env variable. *)
val set_debug_mode: bool -> unit

(** Overwrite stdlib's [OrderedType] *)
module type SetOrderedType = sig
  include Set.OrderedType
  val pretty: t -> string
end

(** Overwrite stdlib's [Set.Make] *)
module SetMake (B: SetOrderedType): IrminTypes.SET with type elt = B.t
