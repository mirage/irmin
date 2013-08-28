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

(** Compose IO operations *)

open IrminTypes

(** Create a new buffer. *)
val create: int -> (int -> unit Lwt.t) -> bufIO

(** Wait for all array elements in the range to be available *)
val poll: bufIO -> int -> unit Lwt.t

(** {2 Errors} *)

(** Parse error *)
exception Parse_error of string

(** Eventualy raise a exception *)
val parse_error_buf: bufIO -> ('a, unit, string, 'b Lwt.t) format4 -> 'a

(** Raise an exception *)
val parse_error: ('a, unit, string, 'b) format4 -> 'a

(** Dump the buffer *)
val dump_buffer: all:bool -> bufIO -> unit

(** {Basic IO operations} *)

(** Get/set big-endian integers of various sizes. *)

(** [get_char buf] return the character stored in [buf]. *)
val get_char: bufIO -> char Lwt.t

(** [get_uint8 buf] is the 8 bit unsigned integer stored in [buf]. *)
val get_uint8: bufIO -> int Lwt.t

(** [get_uint16 buf] is the 16 bit long big-endian unsigned integer
    stored in [buf]. *)
val get_uint16: bufIO -> int Lwt.t

(** [get_uint32 buf] is the 32 bit long big-endian unsigned integer
    stored in [buf]. *)
val get_uint32: bufIO -> int32 Lwt.t

(** [get_uint64 buf] is the 64 bit long big-endian unsigned integer
    stored in [buf]. *)
val get_uint64: bufIO -> int64 Lwt.t

(** [get_string buf len] is the string of size [len] stored in [buf]. *)
val get_string: bufIO -> int -> string Lwt.t

(** [set_char buf off c] write the character [c] in [buf] at offset
    [off]. *)
val set_char: bufIO -> char -> unit Lwt.t

(** [set_uint8 buf] write the 8 bit long integer stored in [buf]. *)
val set_uint8: bufIO -> int -> unit Lwt.t

(** [set_uint16 buf i] writes the 16 bit long big-endian unsigned
    integer [i] in [buf]. *)
val set_uint16: bufIO -> int -> unit Lwt.t

(** [set_uint32 buf i] writes the 32 bit long big-endian unsigned
    integer [i] in [buf]. *)
val set_uint32: bufIO -> int32 -> unit Lwt.t

(** [set_uint64 buf i] writes the 64 bit long big-endian unsigned
    integer [i] in [buf]. *)
val set_uint64: bufIO -> int64 -> unit Lwt.t

(** [set_string buf str] write the string [str] into [buf]. *)
val set_string: bufIO -> string -> unit Lwt.t

(** {2 Lifts} *)

(** Lift IO operation to lists. *)
module List (E: BASE): BASE with type t = E.t list

(** Lift IO operation to options. *)
module Option (E: BASE): BASE with type t = E.t option

(** Lift IO operations to pairs. *)
module Pair (K: BASE) (V: BASE): BASE with type t = K.t * V.t

(** List IO operations to sets. *)
module Set (E: BASE): BASE with type t = E.Set.t

(** serialization to strings *)
module type STRINGABLE = sig
  type t
  val to_string: t -> string
  val of_string: string -> t
end

(** Lift IO operations to strings *)
module String (S: STRINGABLE): BASE with type t = S.t

(** {2 Lwt channels} *)

module Lwt_channel: sig

  type t

  val create: Lwt_unix.file_descr -> string -> t

  val name: t -> string

  val close: t -> unit Lwt.t

  val read_string: t -> int -> string Lwt.t

  val read_buf: t -> int -> bufIO Lwt.t

  val write_string: t -> string -> unit Lwt.t

  val write_buf: t -> bufIO -> int -> unit Lwt.t

  val read_length: t -> int Lwt.t

  val write_length: t -> int -> unit Lwt.t

  val ready: t -> int -> unit Lwt.t

end

(** Extend [BASE] with channel operations *)
module type CHANNEL = sig

  include BASE

  type channel = Lwt_channel.t

   (** Read on a channel *)
  val read_fd: channel -> t Lwt.t

  (** Write on a channel *)
  val write_fd: channel -> t -> unit Lwt.t

end

(** Wire operation *)
module Wire (B: BASE): CHANNEL with type t = B.t

(** File operations *)
module File (B: BASE): CHANNEL with type t = B.t
