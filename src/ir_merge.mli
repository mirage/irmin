(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** {1 Merge functions} *)

type 'a promise = unit -> 'a option result Lwt.t

type 'a t = old:'a promise -> 'a -> 'a -> 'a result Lwt.t

val promise: 'a -> 'a promise
val promise_map: ('a -> 'b) -> 'a promise -> 'b promise
val promise_bind: 'a promise -> ('a -> 'b promise) -> 'b promise


val seq: 'a t list -> 'a t
val apply: ('a -> 'b t) -> 'a -> 'b t

val default: 'a Tc.t -> 'a t

val string: string t

type counter = int
val counter: counter t
module MSet (M: Map.S): sig
  val merge: counter M.t t
end

val option: 'a Tc.t -> 'a t -> 'a option t
val set: (module Set.S with type t = 'a) -> 'a t

val alist: 'a Tc.t -> 'b Tc.t -> ('a -> 'b option t) -> ('a * 'b) list t
module Map (M: Map.S) (K: Tc.S0 with type t = M.key): sig
  val merge: 'a Tc.t -> (M.key -> 'a option t) -> 'a M.t t
end

val pair: 'a Tc.t -> 'b Tc.t -> 'a t -> 'b t -> ('a * 'b) t
val triple: 'a Tc.t -> 'b Tc.t -> 'c Tc.t -> 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val biject:  'a Tc.t -> 'b t -> ('a -> 'b) -> ('b -> 'a) -> 'a t
val biject': 'a Tc.t -> 'b t -> ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> 'a t

val with_conflict: (string -> string) -> 'a t -> 'a t

module OP: sig
  val ok: 'a -> 'a result Lwt.t
  val conflict: ('a, unit, string, 'b result Lwt.t) format4 -> 'a
  val (>>|): 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
  val (>?|): 'a promise -> ('a -> 'b promise) -> 'b promise
end
