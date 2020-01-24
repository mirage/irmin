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

let ( >> ) f g x = g (f x)

module Lens (F : S.MONAD) = struct
  open F
  open F.Infix

  type (-'s, +'t, +'a, -'b) t = {
    op :
      'r. ('a F.t -> ('b -> 'r F.t) -> 'r F.t) -> 's -> ('t -> 'r F.t) -> 'r F.t;
        (** Internally, we allow modification to run inside the {!F} monad, in
            order to be able to compose lenses; this isn't exposed to the user.*)
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

  type (-'s, +'t, +'a, -'b) ty = ('s, 't, 'a, 'b) t

  type ('s, 'a) mono = ('s, 's, 'a, 'a) t

  let v get set =
    let op k this read = k (get this) (fun b -> set this b >>= read) in
    { op }

  let view { op } s = op (fun a _ -> a) s undefined

  let modify : type s t a b. (s, t, a, b) ty -> (a -> b) -> s -> t F.t =
   fun { op } f s -> op (fun aM rf -> aM >>= (f >> return >> bind rf)) s return

  let update l b = modify l (fun _ -> b)

  let ( >> ) :
      type a b c d e f. (a, b, c, d) ty -> (c, d, e, f) ty -> (a, b, e, f) ty =
   fun { op = f } { op = g } ->
    let op z = f (fun cM rf -> cM >>= fun c -> (g z) c rf) in
    { op }

  (* Provided lenses *)
  let id = { op = (fun _ s rf -> rf s) }

  let fst = { op = (fun k (a, x) read -> k (return a) (fun b -> read (b, x))) }

  let snd = { op = (fun k (x, b) read -> k (return b) (fun a -> read (x, a))) }

  let head =
    {
      op =
        (fun k list read ->
          let x, xs = (List.hd list, List.tl list) in
          k (return x) (fun b -> read (b :: xs)));
    }

  type _ t_list =
    | ( :: ) :
        ('s, 't, 'a, 'b) t * 'l t_list
        -> (('s, 't, 'a, 'b) t * 'l) t_list
    | [] : unit t_list
end

module Prism (F : S.MONAD) = struct
  open F
  open F.Infix

  type (-'s, +'t, +'a, -'b) t = {
    review : 'b -> 't F.t;
    preview : 's -> 'a option F.t;
  }

  type ('s, 'a) mono = ('s, 's, 'a, 'a) t

  let v review preview = { review; preview }

  let ( >> ) :
      type a b c d e f. (a, b, c, d) t -> (c, d, e, f) t -> (a, b, e, f) t =
   fun f g ->
    {
      review = g.review >=> f.review;
      preview =
        (f.preview >=> function Some a -> g.preview a | None -> return None);
    }

  (* Provided prisms *)

  let some =
    { review = (fun b -> return (Some b)); preview = (fun s -> return s) }

  let none =
    let review () = return None in
    let preview = function None -> return (Some ()) | Some _ -> return None in
    { review; preview }

  let ok =
    let review b = return (Ok b) in
    let preview = function Ok a -> return (Some a) | Error _ -> return None in
    { review; preview }

  let error =
    let review b = return (Error b) in
    let preview = function Error a -> return (Some a) | Ok _ -> return None in
    { review; preview }

  let head =
    let review b = return [ b ] in
    let preview = function a :: _ -> return (Some a) | [] -> return None in
    { review; preview }

  let nil =
    let review () = return [] in
    let preview = function [] -> return (Some ()) | _ :: _ -> return None in
    { review; preview }

  type _ t_list =
    | ( :: ) :
        ('s, 't, 'a, 'b) t * 'l t_list
        -> (('s, 't, 'a, 'b) t * 'l) t_list
    | [] : unit t_list
end

module Getter (In : S.APPLICATIVE) (Out : S.MONAD) = struct
  type ('s, 'a) t = 's In.t -> 'a Out.t

  let ( >> ) f g = f >> Out.bind (In.return >> g)

  let v f = f

  let ( ^. ) = ( |> )
end

module Optional (F : S.MONAD) = struct
  open F
  open F.Infix
  module Lens = Lens (F)
  module Prism = Prism (F)

  type ('s, 't, 'a, 'b) t = {
    modify : ('a -> 'b option F.t) -> 's -> 't option F.t;
        (** We allow the modification function to be partial in order to ease
            composition of Optionals. This is not exposed to the user. *)
    preview : 's -> 'a option F.t;
  }

  type ('s, 't, 'a, 'b) ty = ('s, 't, 'a, 'b) t

  let modify { modify; _ } f = modify (fun x -> f x >|= (fun a -> Some a))

  let of_lens : type s t a b. (s, t, a, b) Lens.t -> (s, t, a, b) ty =
   fun { Lens.op } ->
    let modify : (a -> b option F.t) -> s -> t option F.t =
     fun f s ->
      op
        (fun aM rf ->
          aM >>= (f >=> function Some b -> rf b | None -> return None))
        s
        (fun x -> return (Some x))
    in
    let preview s = op (fun a _ -> a) s undefined >|= fun a -> Some a in
    { modify; preview }

  let of_prism : type s t a b. (s, t, a, b) Prism.t -> (s, t, a, b) ty =
   fun { review; preview } ->
    let modify f =
      preview >=> function
      | Some a -> (
          f a >>= function
          | Some b -> review b >|= fun t -> Some t
          | None -> return None )
      | None -> return None
    in
    { modify; preview }

  let get_opt { preview; _ } = preview

  let ( >> ) :
      type a b c d e f. (a, b, c, d) t -> (c, d, e, f) t -> (a, b, e, f) t =
   fun f g ->
    {
      modify = g.modify >> f.modify;
      preview =
        (f.preview >=> function Some a -> g.preview a | None -> return None);
    }
end
