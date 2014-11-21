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

(** Merge operators. *)

(** {1 The result monad} *)

type 'a result = [ `Ok of 'a | `Conflict of string ]

module Result: Tc.I1 with type 'a t = 'a result

val bind: 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
val iter: ('a -> unit result Lwt.t) -> 'a list -> unit result Lwt.t

exception Conflict of string
val exn: 'a result -> 'a Lwt.t

module OP: sig
  val ok: 'a -> 'a result Lwt.t
  val conflict: ('a, unit, string, 'b result Lwt.t) format4 -> 'a
  val (>>|): 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
end

(** {1 Merge functions} *)

type ('a, 'o) t = 'o -> old:'a -> 'a -> 'a -> 'a result Lwt.t

module type S = Tc.I0

type 'a elt = (module S with type t = 'a)

val default: 'a elt -> ('a, 'o) t
val string: (string, 'o) t
val counter: (int, 'o) t
val seq: ('a, 'o) t list -> ('a, 'o) t
val some: 'a elt -> ('a, 'o) t -> ('a option, 'o) t
val alist: 'a elt -> 'b elt -> ('b, 'o) t -> ( ('a * 'b) list, 'o) t
module Map (X: S): sig
  val merge: ('a elt) -> ('a, 'o) t -> ('a Map.Make(X).t, 'o) t
end
val pair: 'a elt -> 'b elt -> ('a, 'o) t -> ('b, 'o) t -> ('a * 'b, 'o) t
val biject: 'a elt -> 'b elt ->
  ('a, 'o) t -> ('a -> 'b) -> ('b -> 'a) -> ('b, 'o) t
val biject': 'a elt -> 'b elt ->
  ('a, 'o) t -> ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> ('b, 'o) t
val apply: ('a -> ('b, 'o) t) -> 'a -> ('b, 'o) t
