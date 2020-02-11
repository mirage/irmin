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

module Either : sig
  type ('a, 'b) t = Left of 'a | Right of 'b

  val map : ('a, 'b) t -> l:('a -> 'c) -> r:('b -> 'c) -> 'c

  val left : 'a -> ('a, _) t

  val right : 'b -> (_, 'b) t
end

type ('a, 'b) either = ('a, 'b) Either.t = Left of 'a | Right of 'b

(** Functor typeclass. *)
class virtual ['f] functor_ :
  object
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

  val v :
    < bind : 'a 'b. ('a -> ('b, t) app) -> ('a, t) app -> ('b, t) app
    ; fmap : 'a 'b. ('a -> 'b) -> ('a, t) app -> ('b, t) app
    ; return : 'a. 'a -> ('a, t) app >
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
