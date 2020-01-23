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

let undefined _ =
  let exception Undefined in
  raise Undefined

module Lens (F : S.MONAD) = struct
  open F
  open F.Infix
  open F.Syntax

  type (-'s, +'t, +'a, -'b) t = {
    op :
      'r. ('a F.t -> ('b -> 'r F.t) -> 'r F.t) -> 's -> ('t -> 'r F.t) -> 'r F.t;
  }
  (** We use an optic representation very similar to the standard 'van
      Laarhoven' encoding (based on CPS):

      {[
        type (-'s, +'t, +'a, -'b) t = {
          op : 'r. ('a -> ('b -> 'r) -> 'r) -> 's -> ('t -> 'r) -> 'r;
        }
      ]}

      However, the above quickly breaks down when used in practice, because of
      the value restriction. In particular, the corresponding definition of
      [lens] is:

      {[
        let lens : type s t a b. (s -> a) -> (s -> b -> t) -> (s, t, a, b) lens
            =
         fun get set ->
          let op k this read = k (get this) (fun b -> read (set this b)) in
          { op }
      ]}

      See
      <https://stackoverflow.com/questions/29187287/sneaking-lenses-and-cps-past-the-value-restriction#comment4663665529187287> *)

  type ('s, 'a) mono = ('s, 's, 'a, 'a) t

  let v get set =
    let op k this read = k (get this) (fun b -> set this b >>= read) in
    { op }

  let view { op } s = op (fun a _ -> a) s undefined

  let modify { op } f s =
    op (fun aM rf -> aM >>= fun a -> rf (f a)) s return

  let update l b = modify l (fun _ -> b)

  let ( >> ) { op = f } { op = g } =
    let op z = f (g z) in
    { op }

  (* Provided lenses *)
  let id = { op = (fun _ s rf -> rf s) }

  let fst = { op = (fun k (a, x) read -> k a (fun b -> read (b, x))) }

  let snd = { op = (fun k (x, b) read -> k b (fun a -> read (x, a))) }

  let head =
    {
      op =
        (fun k list read ->
          let x, xs = (List.hd list, List.tl list) in
          k x (fun b -> read (b :: xs)));
    }

  type _ t_list =
    | ( :: ) :
        ('s, 't, 'a, 'b) t * 'l t_list
        -> (('s, 't, 'a, 'b) t * 'l) t_list
    | [] : unit t_list
end

module Prism = struct
  type (-'s, +'t, +'a, -'b) t = {
    review : 'b -> 't;
    preview : 's -> ('a, 't) result;
  }

  type ('s, 'a) mono = ('s, 's, 'a, 'a) t

  let v review preview = { review; preview }

  let ( >> ) = undefined

  (* f g = {
   *     review = (fun x -> f.review (g.review x));
   *     preview = (fun s -> g.preview (f.preview s)); *)
  (* } *)

  (* Provided prisms *)

  type _ t_list =
    | ( :: ) :
        ('s, 't, 'a, 'b) t * 'l t_list
        -> (('s, 't, 'a, 'b) t * 'l) t_list
    | [] : unit t_list
end

let ( >> ) f g x = g (f x)

module Getter (In : S.APPLICATIVE) (Out : S.MONAD) = struct
  type ('s, 'a) t = 's In.t -> 'a Out.t

  let ( >> ) f g = f >> Out.bind (In.return >> g)

  let v f = f

  let ( ^. ) = ( |> )
end

module Traversal (F : S.APPLICATIVE) = struct
  type ('s, 't, 'a, 'b) t = {
    traverse : ('a -> 'b F.t) -> 's -> 't F.t;
    get : 's -> 'a F.t;
  }

  let ( >> ) f g x = g (f x)
end
