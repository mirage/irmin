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

module Pair = Pair
module Either = Either

type ('a, 'b) either = ('a, 'b) Either.t = Left of 'a | Right of 'b

module Natural = struct
  type ('m, 'n) t = { nat : 'a. ('a, 'm) app -> ('a, 'n) app } [@@unboxed]
end

class virtual ['f] functor_ =
  object
    method virtual pure : 'a. 'a -> ('a, 'f) app

    method virtual fmap : 'a 'b. ('a -> 'b) -> ('a, 'f) app -> ('b, 'f) app
  end

class virtual ['m] monad =
  object
    inherit ['m] functor_

    method virtual return : 'a. 'a -> ('a, 'm) app

    method virtual bind
        : 'a 'b. ('a -> ('b, 'm) app) -> ('a, 'm) app -> ('b, 'm) app
  end

module Monad = struct
  let sequence : type a m. m monad -> (a, m) app list -> (a list, m) app =
   fun monad ms ->
    let ( let* ) x f = monad#bind f x in
    let ( let+ ) x f = monad#fmap f x in
    let+ reversed =
      List.fold_right
        (fun (next : (a, m) app) (acc : (a list, m) app) ->
          let* next = next in
          let+ acc = acc in
          next :: acc)
        ms (monad#return [])
    in
    List.rev reversed

  let kliesli :
      type a b c m.
      m monad -> (a -> (b, m) app) -> (b -> (c, m) app) -> a -> (c, m) app =
   fun monad f g x -> monad#bind g (f x)
end

module Identity = struct
  include Newtype1 (struct
    type 'a t = 'a
  end)

  let v =
    object
      (* Functor instance *)
      inherit [t] functor_

      method pure = inj

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

module State (S : sig
  type t
end) =
struct
  type state = S.t

  include Newtype1 (struct
    type 'a t = state -> 'a * state
  end)

  let t =
    object
      inherit [t] monad

      method pure : 'a. 'a -> ('a, t) app = fun a -> inj (fun s -> (a, s))

      method fmap f get =
        inj (fun s ->
            let a, s = (prj get) s in
            (f a, s))

      method return a = inj (fun s -> (a, s))

      method bind f get =
        inj (fun s ->
            let a, s' = (prj get) s in
            (f a |> prj) s')

      method get = inj (fun s -> (s, s))

      method put : state -> (unit, t) app = fun s -> inj (fun _ -> ((), s))

      method run : 'a. ('a, t) app -> state -> 'a * state =
        fun m s -> (m |> prj) s
    end
end
