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

(** JSON utilities *)

(** Abstract type *)
type t =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of t list
  | `O of (string * t) list ]

(** {2 Serializer} *)

val of_buffer: Buffer.t -> t
val to_buffer: Buffer.t -> t -> unit
val pretty: t -> string

(** {2 of JSON} *)

val of_string: string -> t
val of_strings: string list -> t
val of_int: int -> t
val of_list: ('a -> t) -> 'a list -> t
val of_option: ('a -> t) -> 'a option -> t
val of_pair: ('a -> t) -> ('b -> t) -> ('a * 'b) -> t

(** {2 to JSON} *)

val to_string: t -> string
val to_strings: t -> string list
val to_int: t -> int
val to_list: (t -> 'a) -> t -> 'a list
val to_option:(t -> 'a) -> t -> 'a option
val to_pair:(t -> 'a) -> (t -> 'b) -> t -> ('a * 'b)
