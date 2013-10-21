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

(** Bigarrays *)

type ba = Cstruct.buffer

(** Create a bigarray. *)
val create_ba: int -> ba

(** Dump a bigarray. *)
val dump_ba: ba -> unit

(** Bound-mutable bigarrays. *)
type t = {
  mutable buffer: Cstruct.t;
}

(** Create a new buffer. *)
val create: int -> t

(** Create a new buffer. *)
val of_ba: ba -> t

(** Accessor. Return the underlying big array. *)
val to_ba: t -> ba

(** {2 Errors} *)

(** Parse error *)
exception Parse_error of string

(** Eventualy raise a exception *)
val parse_error_buf: t -> ('a, unit, string, 'b) format4 -> 'a

(** Raise an exception *)
val parse_error: ('a, unit, string, 'b) format4 -> 'a

(** Dump the buffer *)
val dump: t -> unit

(** {Basic IO operations} *)

(** Get/set big-endian integers of various sizes. *)

(** [get_char buf] return the character stored in [buf]. *)
val get_char: t -> char

(** [get_uint8 buf] is the 8 bit unsigned integer stored in [buf]. *)
val get_uint8: t -> int

(** [get_uint16 buf] is the 16 bit long big-endian unsigned integer
    stored in [buf]. *)
val get_uint16: t -> int

(** [get_uint32 buf] is the 32 bit long big-endian unsigned integer
    stored in [buf]. *)
val get_uint32: t -> int32

(** [get_uint64 buf] is the 64 bit long big-endian unsigned integer
    stored in [buf]. *)
val get_uint64: t -> int64

(** [get_string buf len] is the string of size [len] stored in [buf]. *)
val get_string: t -> int -> string

(** [set_char buf off c] write the character [c] in [buf] at offset
    [off]. *)
val set_char: t -> char -> unit

(** [set_uint8 buf] write the 8 bit long integer stored in [buf]. *)
val set_uint8: t -> int -> unit

(** [set_uint16 buf i] writes the 16 bit long big-endian unsigned
    integer [i] in [buf]. *)
val set_uint16: t -> int -> unit

(** [set_uint32 buf i] writes the 32 bit long big-endian unsigned
    integer [i] in [buf]. *)
val set_uint32: t -> int32 -> unit

(** [set_uint64 buf i] writes the 64 bit long big-endian unsigned
    integer [i] in [buf]. *)
val set_uint64: t -> int64 -> unit

(** [set_string buf str] write the string [str] into [buf]. *)
val set_string: t -> string -> unit
