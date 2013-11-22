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

type t
(** Type for mutable windows on top of bigarrays, with bounds
    automatically updated when values are read/write in the buffer. *)

val create: int -> t
(** Create a new buffer. *)

(** Get the buffer length. *)
val length: t -> int

(** {2 Errors} *)

exception Parse_error of string
(** Parse error *)

val parse_error_buf: t -> ('a, unit, string, 'b) format4 -> 'a
(** Eventualy raise a exception *)

val parse_error: ('a, unit, string, 'b) format4 -> 'a
(** Raise an exception *)

val dump: ?msg:string -> t -> unit
(** Dump the buffer *)

(** {Basic IO operations} *)

(** Get/set big-endian integers of various sizes. *)

val get_char: t -> char
(** [get_char buf] return the character stored in [buf]. *)

val get_uint8: t -> int
(** [get_uint8 buf] is the 8 bit unsigned integer stored in [buf]. *)

val get_uint16: t -> int
(** [get_uint16 buf] is the 16 bit long big-endian unsigned integer
    stored in [buf]. *)

val get_uint32: t -> int32
(** [get_uint32 buf] is the 32 bit long big-endian unsigned integer
    stored in [buf]. *)

val get_uint64: t -> int64
(** [get_uint64 buf] is the 64 bit long big-endian unsigned integer
    stored in [buf]. *)

val get_string: t -> int -> string
(** [get_string buf len] is the string of size [len] stored in [buf]. *)

val pick_string: t -> int -> string option
(** [pick_string buf len] looks for the string of size [len] in the
    buffer, without consuming it. Return [None] if the buffer is
    bigger than [len]. *)

val set_char: t -> char -> unit
(** [set_char buf off c] write the character [c] in [buf] at offset
    [off]. *)

val set_uint8: t -> int -> unit
(** [set_uint8 buf] write the 8 bit long integer stored in [buf]. *)

val set_uint16: t -> int -> unit
(** [set_uint16 buf i] writes the 16 bit long big-endian unsigned
    integer [i] in [buf]. *)

val set_uint32: t -> int32 -> unit
(** [set_uint32 buf i] writes the 32 bit long big-endian unsigned
    integer [i] in [buf]. *)

val set_uint64: t -> int64 -> unit
(** [set_uint64 buf i] writes the 64 bit long big-endian unsigned
    integer [i] in [buf]. *)

val set_string: t -> string -> unit
(** [set_string buf str] write the string [str] into [buf]. *)

(** {2 Bigarrays} *)

type ba = Cstruct.buffer
(** ocaml-cstruct's buffers are windows on top of bigarrays. *)

(** Create a bigarray. *)
val create_ba: int -> ba

val dump_ba: ?msg:string -> ba -> unit
(** Dump a bigarray. *)

val pretty_ba: ba -> string
(** Pretty print a big array. *)

(** Length of a bigarray. *)
val length_ba: ba -> int

(** Create a new buffer. *)
val of_ba: ba -> t

(** Accessor. Return the underlying big array. *)
val to_ba: t -> ba
