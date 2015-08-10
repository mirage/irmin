(*
 * Copyright (c) 2015 Daniel C. Bünzli
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type 'a parser = string -> [ `Error of string | `Ok of 'a ]
type 'a printer = Format.formatter -> 'a -> unit
type 'a converter = 'a parser * 'a printer

val parser : 'a converter -> 'a parser
val printer : 'a converter -> 'a printer
val bool : bool converter
val int : int converter
val string : string converter
val some : 'a converter -> 'a option converter
val uri: Uri.t converter

type 'a key
val key : ?docs:string -> ?docv:string -> ?doc:string ->
  string -> 'a converter -> 'a -> 'a key

val name: 'a key -> string
val docs: 'a key -> string option
val docv: 'a key -> string option
val doc: 'a key -> string option
val conv: 'a key -> 'a converter
val default: 'a key -> 'a

val root: string option key

type t
val empty : t
val singleton: 'a key -> 'a -> t
val is_empty : t -> bool
val mem : t -> 'a key -> bool
val add : t -> 'a key -> 'a -> t
val rem : t -> 'a key -> t
val union : t -> t -> t
val find : t -> 'a key -> 'a option
val get : t -> 'a key -> 'a
