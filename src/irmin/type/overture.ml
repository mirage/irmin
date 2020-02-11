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

let ( >>> ) f g x = g (f x)

let flip f a b = f b a

let some x = Some x

module Either = struct
  type ('a, 'b) t = Left of 'a | Right of 'b

  let map t ~l ~r = match t with Left x -> l x | Right x -> r x

  let left x = Left x

  let right x = Right x
end

type ('a, 'b) either = ('a, 'b) Either.t = Left of 'a | Right of 'b

class virtual ['f] functor_ =
  object
    method virtual fmap : 'a 'b. ('a -> 'b) -> ('a, 'f) app -> ('b, 'f) app
  end

class virtual ['m] monad =
  object
    inherit ['m] functor_

    method virtual return : 'a. 'a -> ('a, 'm) app

    method virtual bind
        : 'a 'b. ('a -> ('b, 'm) app) -> ('a, 'm) app -> ('b, 'm) app
  end

module Identity = struct
  include Newtype1 (struct
    type 'a t = 'a
  end)

  let v =
    object
      (* Functor instance *)
      inherit [t] functor_

      method fmap f = prj >>> f >>> inj

      (* Monad instance *)
      inherit [t] monad

      method return = inj

      method bind f = prj >>> f
    end
end

module Const = struct
  include Newtype2 (struct
    type ('a, 'b) t = 'a
  end)

  (* let v =
   *   object
   *     inherit [('a, t) app] functor_
   * 
   *     method fmap f = prj >>> f >>> inj
   * 
   *     method return = prj
   * 
   *     method bind f = prj >>> f >>> inj
   * 
   *     method get_const = prj
   *   end *)
end
