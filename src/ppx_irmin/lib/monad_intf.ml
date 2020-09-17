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

module type S = sig
  type ('a, 'p) t

  val return : 'a -> ('a, 'p) t

  val map : ('a -> 'b) -> ('a, 'p) t -> ('b, 'p) t

  val bind : ('a -> ('b, 'p) t) -> ('a, 'p) t -> ('b, 'p) t

  val sequence : ('a, 'p) t list -> ('a list, 'p) t

  module Syntax : sig
    val ( let+ ) : ('a, 'p) t -> ('a -> 'b) -> ('b, 'p) t

    val ( let* ) : ('a, 'p) t -> ('a -> ('b, 'p) t) -> ('b, 'p) t
  end
end

module type Monad = sig
  module type S = S

  module Reader : sig
    (** Computations that read values from a shared environment. *)

    include S
    (** @inline *)

    val run : ('a, 'e) t -> 'e -> 'a
    (** Runs a {!('a, 'e) t} and extracts the final value ['a] from it. *)

    val ask : ('e, 'e) t
    (** Retrieves the monad environment. *)

    val asks : ('e -> 'a) -> ('a, 'e) t
    (** Retrieves a projection of the current monad environment. *)

    val local : ('e -> 'e) -> ('a, 'e) t -> ('a, 'e) t
    (** [local f m] executes a computation in [m] in an environment modified by
        [f]. *)
  end
end
