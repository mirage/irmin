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

module Create : sig
  module Lens : sig
    type 'w t = { f : 's 't 'a 'b. ('s, 't, 'a, 'b, 'w) lens_builder }
    [@@unboxed]
  end

  module Prism : sig
    type 'w t = { f : 's 't 'a 'b. ('s, 't, 'a, 'b, 'w) prism_builder }
    [@@unboxed]
  end

  module Optional : sig
    type 'w t = { f : 's 't 'a 'b. ('s, 't, 'a, 'b, 'w) optional_builder }
    [@@unboxed]
  end

  module Mapper : sig
    type 'w t = { f : 's 't 'a 'b. ('s, 't, 'a, 'b, 'w) mapper_builder }
    [@@unboxed]
  end

  val lens : 'w Lens.t -> ([> lens ], 'w) t

  val prism : 'w Prism.t -> ([> prism ], 'w) t

  val optional : 'w Optional.t -> ([> optional ], 'w) t

  val mapper : 'w Mapper.t -> ([> map ], 'w) t
end

module Coerce : sig
  val lens : ([< lens ], 'w) t -> ('s, 't, 'a, 'b, 'w) lens_builder

  val mapper : ([< map ], 'w) t -> ('s, 't, 'a, 'b, 'w) mapper_builder

  val optional : ([< optional ], 'w) t -> ('s, 't, 'a, 'b, 'w) optional_builder

  val prism : ([< prism ], 'w) t -> ('s, 't, 'a, 'b, 'w) prism_builder
end
