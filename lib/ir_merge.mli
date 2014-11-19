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

(** {2 The result monad} *)

type 'a result =
  [ `Ok of 'a
  | `Conflict of string ]
(** Merge results. *)

val bind: 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
(** Monadic bind over Result. *)

val iter: ('a -> unit result Lwt.t) -> 'a list -> unit result Lwt.t
(** Monadic iterations over results. *)

module Result (A: Tc.I0): Tc.I0 with type t = A.t result
(** Base function over [A.t result]s.. *)

module UnitResult: Tc.I0 with type t = unit result
(** Base functions overs [unit result]s. *)

exception Conflict of string
(** Exception which might be raised when merging.  *)

val exn: 'a result -> 'a Lwt.t
(** Convert [Conflict] values to [Conflict] exceptions. *)

module OP: sig

  val ok: 'a -> 'a result Lwt.t
  (** Return [`Ok x]. *)

  val conflict: ('a, unit, string, 'b result Lwt.t) format4 -> 'a
  (** Return [`Conflict str]. *)

  val (>>|): 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
  (** Same as [bind]. *)

end

(** {2 Merge functions} *)

type ('a, 'o) t = 'o -> old:'a -> 'a -> 'a -> 'a result Lwt.t
(** Signature of a merge function.

            /----> t1 ----\
    ----> old              |--> result
            \----> t2 ----/
*)

module type S = Tc.I0
(** Consider only [Tc.I0] as mergeable. *)

type 'a elt = (module S with type t = 'a)
(** The type for mergeable contents of type ['a]. *)

val default: 'a elt -> ('a, 'o) t
(** Create a default merge function. This is a simple merge
    functions which support changes in one branch at the time:

    - if t1=t2  then return t1
    - if t1=old then return t2
    - if t2=old then return t1
    - otherwise raise [Conflict].
*)

val string: (string, 'o) t
(** The default string merge function. Do not anything clever, just
    compare the strings using the [default] merge function. *)

val counter: (int, 'o) t
(** Mergeable counters. *)

(** {2 Combinator} *)

val seq: ('a, 'o) t list -> ('a, 'o) t
(** Try the merge operations in sequence. *)

val some: 'a elt -> ('a, 'o) t -> ('a option, 'o) t
(** Lift a merge function to optional values of the same type. If all
    the provided values are inhabited, then call the provided merge
    function, otherwise use the same behavior as [create]. *)

module Map (X: S): sig
  val merge: ('a elt) -> ('a, 'o) t -> ('a Map.Make(X).t, 'o) t
end
(** Lift to string maps. *)

val pair: 'a elt -> 'b elt -> ('a, 'o) t -> ('b, 'o) t -> ('a * 'b, 'o) t
(** Lift to pairs. *)

val biject: 'a elt -> 'b elt ->
  ('a, 'o) t -> ('a -> 'b) -> ('b -> 'a) -> ('b, 'o) t
(** Use the merge function defined in another domain. If the
    functions given in argument are partial (ie. returning
    [Not_found] on some entries), the exception is catched and
    [Conflict] is returned instead. *)

val biject': 'a elt -> 'b elt ->
  ('a, 'o) t -> ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> ('b, 'o) t
(** Same as [map] but with potentially blocking converting
    functions. *)

val apply: ('a -> ('b, 'o) t) -> 'a -> ('b, 'o) t
(** [apply] combinator. Usefull to untie recursive loops. *)
