(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type conflict = [ `Conflict of string ] [@@deriving irmin]
(** The type for merge errors. *)

val ok : 'a -> ('a, conflict) result Lwt.t
(** Return [Ok x]. *)

val conflict : ('a, unit, string, ('b, conflict) result Lwt.t) format4 -> 'a
(** Return [Error (Conflict str)]. *)

val bind :
  ('a, 'b) result Lwt.t ->
  ('a -> ('c, 'b) result Lwt.t) ->
  ('c, 'b) result Lwt.t
(** [bind r f] is the merge result which behaves as of the application of the
    function [f] to the return value of [r]. If [r] fails, [bind r f] also
    fails, with the same conflict. *)

val map : ('a -> 'c) -> ('a, 'b) result Lwt.t -> ('c, 'b) result Lwt.t
(** [map f m] maps the result of a merge. This is the same as
    [bind m (fun x -> ok (f x))]. *)

(** {1 Merge Combinators} *)

type 'a promise = unit -> ('a option, conflict) result Lwt.t
(** An ['a] promise is a function which, when called, will eventually return a
    value type of ['a]. A promise is an optional, lazy and non-blocking value. *)

val promise : 'a -> 'a promise
(** [promise a] is the promise containing [a]. *)

val map_promise : ('a -> 'b) -> 'a promise -> 'b promise
(** [map_promise f a] is the promise containing [f] applied to what is promised
    by [a]. *)

val bind_promise : 'a promise -> ('a -> 'b promise) -> 'b promise
(** [bind_promise a f] is the promise returned by [f] applied to what is
    promised by [a]. *)

type 'a f = old:'a promise -> 'a -> 'a -> ('a, conflict) result Lwt.t
(** Signature of a merge function. [old] is the value of the least-common
    ancestor.

    {v
            /----> t1 ----\
    ----> old              |--> result
            \----> t2 ----/
    v} *)

type 'a t
(** The type for merge combinators. *)

val v : 'a Type.t -> 'a f -> 'a t
(** [v dt f] create a merge combinator. *)

val f : 'a t -> 'a f
(** [f m] is [m]'s merge function. *)

val seq : 'a t list -> 'a t
(** Call the merge functions in sequence. Stop as soon as one is {e not}
    returning a conflict. *)

val like : 'a Type.t -> 'b t -> ('a -> 'b) -> ('b -> 'a) -> 'a t
(** Use the merge function defined in another domain. If the converting
    functions raise any exception the merge is a conflict. *)

val with_conflict : (string -> string) -> 'a t -> 'a t
(** [with_conflict f m] is [m] with the conflict error message modified by [f]. *)

val like_lwt : 'a Type.t -> 'b t -> ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> 'a t
(** Same as {{!Merge.biject} biject} but with blocking domain converting
    functions. *)

(** {1 Basic Merges} *)

val default : 'a Type.t -> 'a t
(** [default t] is the default merge function for values of type [t]. This is a
    simple merge function which supports changes in one branch at a time:

    - if [t1=old] then the result of the merge is [OK t2];
    - if [t2=old] then return [OK t1];
    - otherwise the result is [Conflict]. *)

val idempotent : 'a Type.t -> 'a t
(** [idempotent t] is the default merge function for values of type [t] using
    idempotent operations. It follows the same rules as the {!default} merge
    function but also adds:

    - if [t1=t2] then the result of the merge is [OK t1]. *)

val unit : unit t
(** [unit] is the default merge function for unit values. *)

val bool : bool t
(** [bool] is the default merge function for booleans. *)

val char : char t
(** [char] is the default merge function for characters. *)

val int32 : int32 t
(** [int32] is the default merge function for 32-bits integers. *)

val int64 : int64 t
(** [int64] the default merge function for 64-bit integers. *)

val float : float t
(** [float] is the default merge function for floating point numbers. *)

val string : string t
(** The default string merge function. Do not do anything clever, just compare
    the strings using the [default] merge function. *)

val option : 'a t -> 'a option t
(** Lift a merge function to optional values of the same type. If all the
    provided values are inhabited, then call the provided merge function,
    otherwise use the same behavior as {!default}. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** Lift merge functions to pairs of elements. *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** Lift merge functions to triples of elements. *)

(** {1 Counters and Multisets} *)

type counter = int64
(** The type for counter values. It is expected that the only valid operations
    on counters are {e increment} and {e decrement}. The following merge
    functions ensure that the counter semantics are preserved: {e i.e.} it
    ensures that the number of increments and decrements is preserved. *)

val counter : counter t
(** The merge function for mergeable counters. *)

(** Multi-sets. *)
module MultiSet (K : sig
  include Set.OrderedType

  val t : t Type.t
end) : sig
  val merge : counter Map.Make(K).t t
end

(** {1 Maps and Association Lists} *)

(** We consider the only valid operations for maps and association lists to be:

    - Adding a new bindings to the map.
    - Removing a binding from the map.
    - Replacing an existing binding with a different value.
    - {e Trying to add an already existing binding is a no-op}.

    We thus assume that no operation on maps is modifying the {e key} names. So
    the following merge functions ensures that {e (i)} new bindings are
    preserved {e (ii)} removed bindings stay removed and {e (iii)} modified
    bindings are merged using the merge function of values.

    {b Note:} We only consider sets of bindings, instead of multisets.
    Application developers should take care of concurrent addition and removal
    of similar bindings themselves, by using the appropriate {{!Merge.MSet}
    multi-sets}. *)

(** Lift merge functions to sets. *)
module Set (E : sig
  include Set.OrderedType

  val t : t Type.t
end) : sig
  val merge : Set.Make(E).t t
end

val alist : 'a Type.t -> 'b Type.t -> ('a -> 'b option t) -> ('a * 'b) list t
(** Lift the merge functions to association lists. *)

(** Lift the merge functions to maps. *)

module Map (K : sig
  include Map.OrderedType

  val t : t Type.t
end) : sig
  val merge : 'a Type.t -> (K.t -> 'a option t) -> 'a Map.Make(K).t t
end

(** Infix operators for manipulating merge results and {!promise}s.

    [open Irmin.Merge.Infix] at the top of your file to use them. *)
module Infix : sig
  (** {1 Merge Result Combinators} *)

  val ( >>=* ) :
    ('a, conflict) result Lwt.t ->
    ('a -> ('b, conflict) result Lwt.t) ->
    ('b, conflict) result Lwt.t
  (** [>>=*] is {!bind}. *)

  val ( >|=* ) :
    ('a, conflict) result Lwt.t -> ('a -> 'b) -> ('b, conflict) result Lwt.t
  (** [>|=*] is {!map}. *)

  (** {1 Promise Combinators}

      This is useful to manipulate lca results. *)

  val ( >>=? ) : 'a promise -> ('a -> 'b promise) -> 'b promise
  (** [>>=?] is {!bind_promise}. *)

  val ( >|=? ) : 'a promise -> ('a -> 'b) -> 'b promise
  (** [>|=?] is {!map_promise}. *)
end
