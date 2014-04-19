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

open Core_kernel.Std

exception Conflict
(** Exception raised during merge conflicts. *)

module type S = sig

  (** Mergeable contents. *)

  type t

  val to_string: t -> string
  (** Pretty-print. *)

  val equal: t -> t -> bool
  (** Equality. *)

end

type 'a t
(** Abstract merge functions. *)

val merge: 'a t -> old:'a -> 'a -> 'a -> 'a Lwt.t
(** Get the merge functions.

            /----> t1 ----\
    ----> old              |--> result
            \----> t2 ----/
*)

(** {2 Combinators} *)

val create: (module S with type t = 'a) -> (old:'a -> 'a -> 'a-> 'a) -> 'a t
(** Create a custom merge operator. *)

val create': (module S with type t = 'a) -> (old:'a -> 'a -> 'a-> 'a Lwt.t) -> 'a t
(** Create a custom merge operator which might block. *)

val default: (module S with type t = 'a) -> 'a t
(** Create a default merge function. This is a simple merge
    functions which support changes in one branch at the time:

    - if t1=t2  then return t1
    - if t1=old then return t2
    - if t2=old then return t1
    - otherwise raise [Conflict].
*)

val default': (module S with type t = 'a) -> ('a -> 'a -> bool Lwt.t) -> 'a t
(** Same as [default] but for blocking equality functions. *)

val string: string t
(** The default string merge function. Do not anything clever, just
    compare the strings using the [default] merge function. *)

val counter: int t
(** Mergeable counters. *)

val some: 'a t -> 'a option t
(** Lift a merge function to optional values of the same type. If all
    the provided values are inhabited, then call the provided merge
    function, otherwise use the same behavior as [create]. *)

val map: 'a t -> 'a String.Map.t t
(** Lift to hash-tables. *)

val pair: 'a t -> 'b t -> ('a * 'b) t
(** Lift to pairs. *)

val apply: ('a -> 'b t ) -> 'a -> 'b t
(** Apply operator. Use this operator to break recursive loops. *)

val biject: (module S with type t = 'b) -> 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t
(** Use the merge function defined in another domain. If the
    functions given in argument are partial (ie. returning
    [Not_found] on some entries), the exception is catched and
    [Conflict] is returned instead. *)

val biject': (module S with type t = 'b) -> 'a t -> ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> 'b t
(** Same as [map] but with potentially blocking converting
    functions. *)
