(*
 * Copyright (c) 2019-2020 Craig Ferguson <me@craigfe.io>
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

open Higher

val ( >>> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Left-to-right composition operator. *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

val some : 'a -> 'a option

module Pair : sig
  include module type of Pair
end

module Either : sig
  include module type of Either
end

type ('a, 'b) either = ('a, 'b) Either.t = Left of 'a | Right of 'b

module Natural : sig
  type ('m, 'n) t = { nat : 'a. ('a, 'm) app -> ('a, 'n) app } [@@unboxed]
  (** The type of natural transformations from operator ['m] to operator ['n]. *)
end

(** Functor typeclass. *)
class virtual ['f] functor_ :
  object
    method virtual pure : 'a. 'a -> ('a, 'f) app

    method virtual fmap : 'a 'b. ('a -> 'b) -> ('a, 'f) app -> ('b, 'f) app
  end

(** Monad typeclass. *)
class virtual ['m] monad :
  object
    inherit ['m] functor_

    method virtual return : 'a. 'a -> ('a, 'm) app

    method virtual bind :
      'a 'b. ('a -> ('b, 'm) app) -> ('a, 'm) app -> ('b, 'm) app
  end

module Identity : sig
  (** The identity container. *)

  include module type of Newtype1 (struct
    type 'a t = 'a
  end)

  val v : t monad
end

module Monad : sig
  val sequence : 'm monad -> ('a, 'm) app list -> ('a list, 'm) app

  val kliesli :
    'm monad ->
    ('a -> ('b, 'm) app) ->
    ('b -> ('c, 'm) app) ->
    'a ->
    ('c, 'm) app
end

module Const : sig
  (** The constant container. *)

  include module type of Newtype2 (struct
    type ('a, 'b) t = 'a
  end)

  (* val v :
   *   < bind :
   *       'a 'b 'c. ('a -> ('b, ('c, t) app) app) -> ('a, ('c, t) app) app ->
   *       ('b, ('c, t) app) app
   *   ; fmap : 'a 'b. ('a -> 'b) -> ('a, t) app -> ('b, t) app
   *   ; return : 'a. 'a -> ('a, t) app
   *   ; get_const : 'a 'c. ('a, ('c, t) app) app -> 'c > *)
end

module State (S : sig
  type t
end) : sig
  type state

  include module type of Newtype1 (struct
    type 'a t
  end)

  val t :
    < t monad
    ; get : (state, t) app
    ; put : state -> (unit, t) app
    ; run : 'a. ('a, t) app -> state -> 'a * state >
end
with type state = S.t
