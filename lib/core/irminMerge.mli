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

exception Conflict
(** Exception raised during merge conflicts. *)

type 'a t
(** Abstract merge functions. *)

val merge: 'a t -> old:'a -> 'a -> 'a -> 'a Lwt.t
(** Get the merge functions.

            /----> t1 ----\
    ----> old              |--> result
            \----> t2 ----/
*)

(** {2 Combinators} *)

val default: ('a -> 'a -> bool) -> 'a t
(** Create a default merge function. This is a simple merge functions
    which support changes in one branch at the time:

    - if t1=t2  then return t1
    - if t1=old then return t2
    - if t2=old then return t1
    - otherwise raise [Conflict].
*)

val default': ('a -> 'a -> bool Lwt.t) -> 'a t
(** Same as [default] but for non-blocking equality functions. *)

val string: string t
(** The default string merge function. Do not anything clever, just
    compare the strings using the [default] merge function. *)

val some: 'a t -> 'a option t
(** Lift a merge function to optional values of the same type. If all
    the provided values are inhabited, then call the provided merge
    function, otherwise use the same behavior as [create]. *)

val assoc: 'a t -> (string * 'a) list t
(** Lift to assoc lists. *)

val pair: 'a t -> 'b t -> ('a * 'b) t
(** Lift to pairs. *)

val apply: ('a -> 'b t ) -> 'a -> 'b t
(** Apply operator. Use this operator to break recursive loops. *)

val map: 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t
(** Use the merge function defined in another domain. If the functions
    given in argument are partial (ie. returning [Not_found] on some
    entries), the exception is catched and [Conflict] is returned
    instead. *)

val map': 'a t -> ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> 'b t
(** Same as [map] but with potentially non-blocking converting
    functions. *)
