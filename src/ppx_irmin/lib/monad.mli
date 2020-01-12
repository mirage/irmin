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

module type FUNCTOR = sig
  type 'a t

  val return : 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type S = sig
  include FUNCTOR

  val bind : ('a -> 'b t) -> 'a t -> 'b t

  val sequence : 'a t list -> 'a list t

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module Reader (E : sig
  type t
end) : sig
  (** Computations that read values from a shared environment. *)

  include S

  val run : 'a t -> E.t -> 'a
  (** Runs a {!'a t} and extracts the final value ['a] from it. *)

  val ask : E.t t
  (** Retrieves the monad environment. *)

  val asks : (E.t -> 'a) -> 'a t
  (** Retrieves a projection of the current monad environment. *)

  val local : (E.t -> E.t) -> 'a t -> 'a t
  (** [local f m] executes a computation in [m] in an environment modified by
      [f]. *)
end
