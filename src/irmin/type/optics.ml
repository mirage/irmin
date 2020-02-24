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

open Overture
open Higher

let kliesli :
    < bind : 'a 'b. ('a -> ('b, 'm) app) -> ('a, 'm) app -> ('b, 'm) app ; .. > ->
    ('a -> ('b, 'm) app) ->
    ('b -> ('c, 'm) app) ->
    'a ->
    ('c, 'm) app =
 fun monad f g x -> monad#bind g (f x)

module Effectful = struct
  module Lens = struct
    type ('s, 't, 'a, 'b, 'm) t = {
      monad : 'm monad;
      view : 's -> ('a, 'm) app;
      modify : ('a -> ('b, 'm) app) -> 's -> ('t, 'm) app;
    }

    type ('s, 't, 'a, 'b, 'm) ty = ('s, 't, 'a, 'b, 'm) t

    type ('s, 'a, 'm) mono = ('s, 's, 'a, 'a, 'm) t

    let v monad view modify = { monad; view; modify }

    let modify { modify; _ } = modify

    let update { modify; _ } b = modify (fun _ -> b)

    let view { view; _ } = view

    let ( >> ) l1 l2 =
      let ( >=> ) = kliesli l1.monad in
      {
        monad = l1.monad;
        modify = l2.modify >>> l1.modify;
        view = l1.view >=> l2.view;
      }

    type (_, 'm) t_list =
      | ( :: ) :
          ('s, 't, 'a, 'b, 'm) ty * ('l, 'm) t_list
          -> (('s, 't, 'a, 'b, 'm) ty * 'l, 'm) t_list
      | [] : (unit, 'm) t_list
  end

  module Prism = struct
    type ('s, 't, 'a, 'b, 'm) t = {
      monad : 'm monad;
      review : 'b -> ('t, 'm) app;
      preview : 's -> ('a option, 'm) app;
    }

    type ('s, 'a, 'm) mono = ('s, 's, 'a, 'a, 'm) t

    let v monad review preview = { monad; review; preview }

    let review { review; _ } = review

    let preview { preview; _ } = preview

    let natural_compose :
        type m n a b c d e f.
        (d -> ((d, n) app, m) app) ->
        ((c, n) app -> (c, m) app) ->
        (a, b, (c, n) app, (d, n) app, m) t ->
        (c, d, (e, n) app, (f, n) app, m) t ->
        (a, b, (e, n) app, (f, n) app, m) t =
     fun pure nat p1 p2 ->
      let ( >>= ) x fn = p1.monad#bind fn x in
      let review f =
        p2.review f >>= fun d ->
        pure d >>= fun d -> p1.review d
      in
      let preview a =
        p1.preview a >>= function
        | Some c_addr -> nat c_addr >>= fun c -> p2.preview c
        | None -> p1.monad#return None
      in
      { monad = p1.monad; review; preview }

    let ( >> ) :
        type m a b c d e f.
        (a, b, c, d, m) t -> (c, d, e, f, m) t -> (a, b, e, f, m) t =
     fun f g ->
      let ( >=> ) = kliesli f.monad in
      let review = g.review >=> f.review in
      let preview x =
        f.preview x
        |> f.monad#bind (function
             | Some a -> g.preview a
             | None -> f.monad#return None)
      in
      { monad = f.monad; review; preview }

    type (_, 'm) t_list =
      | ( :: ) :
          ('s, 't, 'a, 'b, 'm) t * ('l, 'm) t_list
          -> (('s, 't, 'a, 'b, 'm) t * 'l, 'm) t_list
      | [] : (unit, 'm) t_list

    let id monad =
      {
        monad;
        review = monad#return;
        preview = (fun s -> Some s |> monad#return);
      }
  end

  module Getter (In : S.APPLICATIVE) (Out : S.MONAD) = struct
    type ('s, 'a) t = 's In.t -> 'a Out.t

    let ( >> ) f g = f >>> Out.bind (In.return >>> g)

    let v f = f

    let ( ^. ) = ( |> )
  end

  module Optional = struct
    type ('s, 't, 'a, 'b, 'm) t = {
      monad : 'm monad;
      modify : ('a -> ('b option, 'm) app) -> 's -> ('t option, 'm) app;
      preview : 's -> ('a option, 'm) app;
    }

    type ('s, 't, 'a, 'b, 'm) ty = ('s, 't, 'a, 'b, 'm) t

    let of_lens : type s t a b m. (s, t, a, b, m) Lens.t -> (s, t, a, b, m) ty =
     fun ({ monad; view; modify } as lens) ->
      let ( >>= ) x f = monad#bind f x in
      let ( >>| ) x f = monad#fmap f x in
      let modify f s =
        (* Inefficient implementation because we can't have a partial modify inside the lens. *)
        view s >>= fun a ->
        f a >>= function
        | Some b -> modify (fun _ -> monad#return b) s >>| some
        | None -> monad#return None
      in
      let preview : s -> (a option, m) app = fun s -> lens.view s >>| some in
      { monad; modify; preview }

    let of_prism : type s t a b m. (s, t, a, b, m) Prism.t -> (s, t, a, b, m) ty
        =
     fun { monad; review; preview } ->
      let ( >>= ) x f = monad#bind f x in
      let ( >>| ) x f = monad#fmap f x in
      let modify f x =
        preview x >>= function
        | Some a -> (
            f a >>= function
            | Some b -> review b >>| some
            | None -> monad#return None )
        | None -> monad#return None
      in
      { monad; modify; preview }

    let get_opt { preview; _ } = preview

    let modify { modify; _ } = modify

    let ( >> ) :
        type a b c d e f m.
        (a, b, c, d, m) t -> (c, d, e, f, m) t -> (a, b, e, f, m) t =
     fun f g ->
      let ( >=> ) = kliesli f.monad in
      {
        monad = f.monad;
        modify = g.modify >>> f.modify;
        preview =
          (f.preview >=> function
           | Some a -> g.preview a
           | None -> f.monad#return None);
      }
  end
end

(* Pre-supplied instantiations of the above without effects *)

module Lens = struct
  module L = Effectful.Lens

  let inj, prj = Identity.(inj, prj)

  type ('s, 't, 'a, 'b) t = ('s, 't, 'a, 'b, Identity.t) L.t

  type ('s, 't, 'a, 'b) ty = ('s, 't, 'a, 'b) t

  type ('s, 'a) mono = ('s, 's, 'a, 'a) t

  let id = L.{ monad = Identity.v; view = inj; modify = (fun f x -> f x) }

  let fst : type a1 a2 b. (a1 * b, a2 * b, a1, a2) ty =
    let view (a, _) = inj a in
    let modify f (a, b) = (f a |> prj, b) |> inj in
    { L.monad = Identity.v; view; modify }

  let snd : type a b1 b2. (a * b1, a * b2, b1, b2) ty =
    let view (_, b) = inj b in
    let modify f (a, b) = (a, f b |> prj) |> inj in
    { L.monad = Identity.v; view; modify }

  let v : type s t a b. (s -> a) -> ((a -> b) -> s -> t) -> (s, t, a, b) ty =
   fun view modify ->
    let view = view >>> inj in
    let modify f = modify (f >>> prj) >>> inj in
    L.v Identity.v view modify

  let ( >> ) = L.( >> )

  let view l s = L.view l s |> prj

  let modify l f = L.modify l (f >>> inj) >>> prj

  let update l b = modify l (fun _ -> b)

  let prj x = x
end

module Prism = struct
  module P = Effectful.Prism

  let inj, prj = Identity.(inj, prj)

  type ('s, 't, 'a, 'b) t = ('s, 't, 'a, 'b, Identity.t) P.t

  type ('s, 'a) mono = ('s, 's, 'a, 'a) t

  (* Provided prisms *)

  let some : type a b. (a option, b option, a, b) t =
    let review x = inj (Some x) in
    let preview s = inj s in
    { P.monad = Identity.v; review; preview }

  let none : type a. (a option, a option, unit, unit) t =
    let review () = inj None in
    let preview = function None -> inj (Some ()) | Some _ -> inj None in
    { P.monad = Identity.v; review; preview }

  let ok : type a b c. ((a, c) result, (b, c) result, a, b) t =
    let review b = inj (Ok b) in
    let preview = function Ok a -> inj (Some a) | Error _ -> inj None in
    { P.monad = Identity.v; review; preview }

  let error : type a b c. ((a, b) result, (a, c) result, b, c) t =
    let review b = inj (Error b) in
    let preview = function Error a -> inj (Some a) | Ok _ -> inj None in
    { P.monad = Identity.v; review; preview }

  let head : type a. (a list, a list, a, a) t =
    let review b = inj [ b ] in
    let preview = function a :: _ -> inj (Some a) | [] -> inj None in
    { P.monad = Identity.v; review; preview }

  let tail : type a. (a list, a list, a list, a list) t =
    let review x = inj x in
    let preview = function _ :: tl -> inj (Some tl) | [] -> inj None in
    { P.monad = Identity.v; review; preview }

  let nil : type a. (a list, a list, unit, unit) t =
    let review () = inj [] in
    let preview = function [] -> inj (Some ()) | _ :: _ -> inj None in
    { P.monad = Identity.v; review; preview }

  let v review preview =
    let review b = review b |> inj in
    let preview s = preview s |> inj in
    { P.monad = Identity.v; review; preview }

  let ( >> ) = P.( >> )

  let review p b = P.review p b |> prj

  let preview p s = P.preview p s |> prj

  let prj x = x
end
