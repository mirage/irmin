(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Provenance tracking. *)

type t with bin_io, compare, sexp
(** Provenace values. *)

val to_json: t -> Ezjsonm.t
(** Create a JSON object from a value of type [t]. *)

val of_json: Ezjsonm.t -> t
(** Create a value of type [t] from a JSON object. *)

val set_date: (unit -> int64) -> unit
(** How to compute the commit dates. By default, increment a counter. *)

val set_id: (unit -> string) -> unit
(** How to compute the commit origins. By default, return a random number. *)

val create: ?date:int64 -> ?id:string -> ('a, unit, string, t) format4 -> 'a
(** Create a new provenance message. *)

val date: t -> int64
(** Get the origin date. *)

val id: t -> string
(** Get the origin ID. *)

val message: t -> string
(** Get the origin message. *)
