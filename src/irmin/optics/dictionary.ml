(*
 * Copyright (c) 2020 Craig Ferguson <me@craigfe.io>
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

open Irmin_root
open Brands
open Subtyping

type ('c, 'w) t

type ('s, 't, 'a, 'b, 'p) dimapper = ('a, 'b, 'p) app2 -> ('s, 't, 'p) app2

type ('s, 't, 'a, 'b, 'p) lens_builder =
  view:('s -> 'a) -> update:('s -> 'b -> 't) -> ('s, 't, 'a, 'b, 'p) dimapper

type ('s, 't, 'a, 'b, 'p) prism_builder =
  match_:('s -> ('a, 't) Either.t) ->
  build:('b -> 't) ->
  ('s, 't, 'a, 'b, 'p) dimapper

type ('s, 't, 'a, 'b, 'p) optional_builder =
  ('s -> ('a * ('b -> 't), 't) Either.t) -> ('s, 't, 'a, 'b, 'p) dimapper

type ('s, 't, 'a, 'b, 'p) mapper_builder =
  ('s -> ('a -> 'b) -> 't) -> ('s, 't, 'a, 'b, 'p) dimapper

module Create = struct
  module Lens = struct
    type 'w t = { f : 's 't 'a 'b. ('s, 't, 'a, 'b, 'w) lens_builder }
    [@@unboxed]
  end

  module Prism = struct
    type 'w t = { f : 's 't 'a 'b. ('s, 't, 'a, 'b, 'w) prism_builder }
    [@@unboxed]
  end

  module Optional = struct
    type 'w t = { f : 's 't 'a 'b. ('s, 't, 'a, 'b, 'w) optional_builder }
    [@@unboxed]
  end

  module Mapper = struct
    type 'w t = { f : 's 't 'a 'b. ('s, 't, 'a, 'b, 'w) mapper_builder }
    [@@unboxed]
  end

  (* The type classes above are related with this GADT, using a phantom polymorphic
     variant type index. *)
  type ('c, 'w) t =
    | Lens : 'w Lens.t -> ([> lens ], 'w) t
    | Prism : 'w Prism.t -> ([> prism ], 'w) t
    | Optional : 'w Optional.t -> ([> optional ], 'w) t
    | Mapper : 'w Mapper.t -> ([> map ], 'w) t

  let lens lens = Lens lens

  let prism prism = Prism prism

  let optional optional = Optional optional

  let mapper map = Mapper map
end

type nonrec ('c, 'w) t = ('c, 'w) Create.t

module Coerce = struct
  (** This module implements coercions from the dictionary of each optic to the
      dictionaries of its sub-types as defined in {!Subtyping}. *)

  type 'w lens_hack = Lens_hack : ([< lens ], 'w) t -> 'w lens_hack
  [@@unboxed]

  let lens opt ~view ~update =
    let (Lens_hack opt) = Lens_hack opt in
    match opt with
    | Lens { f } -> f ~view ~update
    | Mapper { f } ->
        f (fun s ab ->
            let a = view s in
            let b = ab a in
            update s b)
    | Optional { f } -> f (fun s -> Left (view s, update s))

  type 'w prism_hack = Prism_hack : ([< prism ], 'w) t -> 'w prism_hack
  [@@unboxed]

  let prism :
      type s ty a b p.
      ([< prism ], p) t ->
      match_:(s -> (a, ty) Either.t) ->
      build:(b -> ty) ->
      (s, ty, a, b, p) dimapper =
   fun opt ~match_ ~build ->
    let (Prism_hack opt) = Prism_hack opt in
    match opt with
    | Prism { f } -> f ~match_ ~build
    | Optional { f } ->
        f (fun s ->
            match match_ s with Left a -> Left (a, build) | Right _ as t -> t)
    | Mapper { f } ->
        f (fun s ab ->
            match match_ s with Left a -> build (ab a) | Right t -> t)

  type 'w optional_hack =
    | Optional_hack : ([< optional ], 'w) t -> 'w optional_hack
  [@@unboxed]

  let optional opt optional_f =
    let (Optional_hack opt) = Optional_hack opt in
    match opt with
    | Optional { f } -> f optional_f
    | Mapper { f } ->
        f (fun s ab ->
            match optional_f s with
            | Left (a, build) -> build (ab a)
            | Right t -> t)

  type 'w mapper_hack = Mapper_hack : ([< map ], 'w) t -> 'w mapper_hack
  [@@unboxed]

  let mapper opt =
    let (Mapper_hack (Mapper { f })) = Mapper_hack opt in
    f
end
