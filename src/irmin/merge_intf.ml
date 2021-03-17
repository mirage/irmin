(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type H = sig
  type (+'a, 's) io
  (** The type for higher-kinded IO effects, resolving to values of type ['a]. *)

  type nonrec conflict = conflict
  (** The type for merge errors. *)

  type nonrec 'a result = ('a, conflict) result [@@deriving irmin]
  (** The type for merge results. *)

  val ok : 'a -> ('a result, _) io
  (** Return [Ok x]. *)

  val conflict : ('a, unit, string, ('b result, _) io) format4 -> 'a
  (** Return [Error (Conflict str)]. *)

  val bind :
    ('a result, 's) io -> ('a -> ('b result, 's) io) -> ('b result, 's) io
  (** [bind r f] is the merge result which behaves as of the application of the
      function [f] to the return value of [r]. If [r] fails, [bind r f] also
      fails, with the same conflict. *)

  val map : ('a -> 'b) -> ('a result, 's) io -> ('b result, 's) io
  (** [map f m] maps the result of a merge. This is the same as
      [bind m (fun x -> ok (f x))]. *)

  (** {1 Merge Combinators} *)

  type ('a, 's) promise = unit -> ('a option result, 's) io
  (** An ['a] promise is a function which, when called, will eventually return a
      value type of ['a]. A promise is an optional, lazy and non-blocking value. *)

  val promise : 'a -> ('a, _) promise
  (** [promise a] is the promise containing [a]. *)

  val map_promise : ('a -> 'b) -> ('a, 's) promise -> ('b, 's) promise
  (** [map_promise f a] is the promise containing [f] applied to what is
      promised by [a]. *)

  val bind_promise :
    ('a, 's) promise -> ('a -> ('b, 's) promise) -> ('b, 's) promise
  (** [bind_promise a f] is the promise returned by [f] applied to what is
      promised by [a]. *)

  type ('a, 's) f = old:('a, 's) promise -> 'a -> 'a -> ('a result, 's) io
  (** Signature of a merge function. [old] is the value of the least-common
      ancestor.

      {v
            /----> t1 ----\
    ----> old              |--> result
            \----> t2 ----/
      v} *)

  type ('a, 'io) t
  (** The type for merge combinators. *)

  val v : 'a Type.t -> ('a, 'io) f -> ('a, 'io) t
  (** [v dt f] create a merge combinator. *)

  val f : ('a, 'io) t -> ('a, 'io) f
  (** [f m] is [m]'s merge function. *)

  val seq : ('a, 'io) t list -> ('a, 'io) t
  (** Call the merge functions in sequence. Stop as soon as one is {e not}
      returning a conflict. *)

  val like : 'a Type.t -> ('b, 'io) t -> ('a -> 'b) -> ('b -> 'a) -> ('a, 'io) t
  (** Use the merge function defined in another domain. If the converting
      functions raise any exception the merge is a conflict. *)

  val with_conflict : (string -> string) -> ('a, 'io) t -> ('a, 'io) t
  (** [with_conflict f m] is [m] with the conflict error message modified by
      [f]. *)

  val like_io :
    'a Type.t ->
    ('b, 'io) t ->
    ('a -> ('b, 'io) io) ->
    ('b -> ('a, 'io) io) ->
    ('a, 'io) t
  (** Same as {{!Merge.biject} biject} but with blocking domain converting
      functions. *)

  (** {1 Basic Merges} *)

  val default : 'a Type.t -> ('a, _) t
  (** [default t] is the default merge function for values of type [t]. This is
      a simple merge function which supports changes in one branch at a time:

      - if [t1=old] then the result of the merge is [OK t2];
      - if [t2=old] then return [OK t1];
      - otherwise the result is [Conflict]. *)

  val idempotent : 'a Type.t -> ('a, _) t
  (** [idempotent t] is the default merge function for values of type [t] using
      idempotent operations. It follows the same rules as the {!default} merge
      function but also adds:

      - if [t1=t2] then the result of the merge is [OK t1]. *)

  val unit : unit -> (unit, 'io) t
  (** [unit] is the default merge function for unit values. *)

  val bool : unit -> (bool, 'io) t
  (** [bool] is the default merge function for booleans. *)

  val char : unit -> (char, 'io) t
  (** [char] is the default merge function for characters. *)

  val int32 : unit -> (int32, 'io) t
  (** [int32] is the default merge function for 32-bits integers. *)

  val int64 : unit -> (int64, 'io) t
  (** [int64] the default merge function for 64-bit integers. *)

  val float : unit -> (float, 'io) t
  (** [float] is the default merge function for floating point numbers. *)

  val string : unit -> (string, 'io) t
  (** The default string merge function. Do not do anything clever, just compare
      the strings using the [default] merge function. *)

  val option : ('a, 'io) t -> ('a option, 'io) t
  (** Lift a merge function to optional values of the same type. If all the
      provided values are inhabited, then call the provided merge function,
      otherwise use the same behavior as {!default}. *)

  val pair : ('a, 'io) t -> ('b, 'io) t -> ('a * 'b, 'io) t
  (** Lift merge functions to pairs of elements. *)

  val triple :
    ('a, 'io) t -> ('b, 'io) t -> ('c, 'io) t -> ('a * 'b * 'c, 'io) t
  (** Lift merge functions to triples of elements. *)

  (** {1 Counters and Multisets} *)

  type counter = int64
  (** The type for counter values. It is expected that the only valid operations
      on counters are {e increment} and {e decrement}. The following merge
      functions ensure that the counter semantics are preserved: {e i.e.} it
      ensures that the number of increments and decrements is preserved. *)

  val counter : unit -> (counter, _) t
  (** The merge function for mergeable counters. *)

  (** Multi-sets. *)
  module MultiSet (K : sig
    include Set.OrderedType

    val t : t Type.t
  end) : sig
    val merge : unit -> (counter Map.Make(K).t, _) t
  end

  (** {1 Maps and Association Lists} *)

  (** We consider the only valid operations for maps and association lists to
      be:

      - Adding a new bindings to the map.
      - Removing a binding from the map.
      - Replacing an existing binding with a different value.
      - {e Trying to add an already existing binding is a no-op}.

      We thus assume that no operation on maps is modifying the {e key} names.
      So the following merge functions ensures that {e (i)} new bindings are
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
    val merge : unit -> (Set.Make(E).t, _) t
  end

  val alist :
    'a Type.t ->
    'b Type.t ->
    ('a -> ('b option, 'io) t) ->
    (('a * 'b) list, 'io) t
  (** Lift the merge functions to association lists. *)

  (** Lift the merge functions to maps. *)

  module Map (K : sig
    include Map.OrderedType

    val t : t Type.t
  end) : sig
    val merge :
      'a Type.t -> (K.t -> ('a option, 'io) t) -> ('a Map.Make(K).t, 'io) t
  end

  (** Infix operators for manipulating merge results and {!promise}s.

      [open Irmin.Merge.Infix] at the top of your file to use them. *)
  module Infix : sig
    (** {1 Merge Result Combinators} *)

    val ( >>=* ) :
      ('a result, 'io) io -> ('a -> ('b result, 'io) io) -> ('b result, 'io) io
    (** [>>=*] is {!bind}. *)

    val ( >|=* ) : ('a result, 'io) io -> ('a -> 'b) -> ('b result, 'io) io
    (** [>|=*] is {!map}. *)

    (** {1 Promise Combinators}

        This is useful to manipulate lca results. *)

    val ( >>=? ) :
      ('a, 'io) promise -> ('a -> ('b, 'io) promise) -> ('b, 'io) promise
    (** [>>=?] is {!bind_promise}. *)

    val ( >|=? ) : ('a, 'io) promise -> ('a -> 'b) -> ('b, 'io) promise
    (** [>|=?] is {!map_promise}. *)
  end
end

module type S = sig
  type +'a io
  (** The type for IO effects. *)

  type conflict = [ `Conflict of string ]
  (** The type for merge errors. *)

  type nonrec 'a result = ('a, conflict) result
  (** The type for merge results. *)

  type 'a promise = unit -> 'a option result io
  (** An ['a] promise is a function which, when called, will eventually return a
      value type of ['a]. A promise is an optional, lazy and non-blocking value. *)

  type 'a f = old:'a promise -> 'a -> 'a -> 'a result io
  (** Signature of a merge function. [old] is the value of the least-common
      ancestor.

      {v
            /----> t1 ----\
    ----> old              |--> result
            \----> t2 ----/
      v} *)

  type 'a t
  (** The type for merge combinators. *)

  include
    H
      with type ('a, _) io := 'a io
       and type conflict := conflict
       and type 'a result := 'a result
       and type ('a, _) promise := 'a promise
       and type ('a, _) f := 'a f
       and type ('a, _) t := 'a t

  module IO : IO.S with type 'a t = 'a io
end

module type Merge = sig
  type nonrec conflict = conflict

  module type H = H

  module DSL : H

  type ('a, 'io) t = ('a, 'io) DSL.t
  type ('a, 'io) abstract_merge := ('a, 'io) t

  module type S = sig
    include S

    type s

    val run : ('a, s) abstract_merge -> 'a t
  end

  module Make (IO : IO.S) : S with type 'a io = 'a IO.t and module IO = IO
end
