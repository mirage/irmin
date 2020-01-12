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
end) =
struct
  type 'a t = Reader of (E.t -> 'a)

  let run (Reader r) = r

  let map f m = Reader (fun env -> f (run m env))

  let bind f m = Reader (fun env -> run (f (run m env)) env)

  let return x = Reader (fun _ -> x)

  let sequence ms =
    List.fold_right
      (fun (aM : 'a t) (bM : 'a list t) ->
        bind (fun a -> map (fun b -> a :: b) bM) aM)
      ms (return [])

  let asks f = Reader (fun env -> f env)

  let ask = asks (fun x -> x)

  let local f m = Reader (fun env -> run m (f env))

  module Syntax = struct
    let ( let+ ) x f = map f x

    let ( let* ) x f = bind f x
  end
end
