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

module type S = sig
  include Tc.I0
  val create: ?date:int64 -> ?id:string -> ('a, unit, string, t) format4 -> 'a
  val date: t -> int64
  val id: t -> string
  val message: t -> string
end

module type P = sig

  (** Origin parameters. *)

  (* XXX: add a way to access private/public keys and thread it to the
     read/write functions. *)

  val date: unit -> int64
  (** How to compute the commit dates. *)

  val id: unit -> string
  (** How to compute the commit origins. *)

  val string_of_date: int64 -> string
  (** Pretty-print dates. *)
end

module Default: S
(** The default origin, where [date] is an incremented counter and
    [id] is a random number. *)

module Make (M: P): S
(** Default origin. *)
