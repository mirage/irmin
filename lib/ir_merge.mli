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

module Result: Tc.S1 with type 'a t = 'a result

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

type 'a t = old:'a -> 'a -> 'a -> 'a result Lwt.t

val default: 'a Tc.t -> 'a t
val string: string t
val counter: int t
val seq: 'a t list -> 'a t
val some: 'a Tc.t -> 'a t -> 'a option t
val alist: 'a Tc.t -> 'b Tc.t -> 'b t -> ('a * 'b) list t
module Map (M: Map.S) (K: Tc.S0 with type t = M.key): sig
  val merge: 'a Tc.t -> 'a t -> 'a M.t t
end
val pair: 'a Tc.t -> 'b Tc.t -> 'a t -> 'b t -> ('a * 'b) t
val biject: 'a Tc.t -> 'b Tc.t -> 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t
val biject': 'a Tc.t -> 'b Tc.t -> 'a t -> ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> 'b t
val apply: ('a -> 'b t) -> 'a -> 'b t
