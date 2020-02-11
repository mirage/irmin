open Overture
open Higher
module Lens = Optics.Effectful.Lens
module Prism = Optics.Effectful.Prism

module Record (M : sig
  type t

  val v : < t monad ; t functor_ >
end) : sig
  open Type_core.Make(M)

  type ('record, 'cons, 'remaning, 'lenses, 'lens_nil) open_record

  val record :
    string -> 'cons -> ('record, 'cons, 'cons, 'lens_nil, 'lens_nil) open_record

  type ('a, 'b) field

  val field :
    string ->
    'a t ->
    ?set:('b -> 'a -> ('b, M.t) app) ->
    ('b -> ('a, M.t) app) ->
    ('b, 'a) field

  val ( |+ ) :
    ( 'record,
      'cons,
      'field -> 'remaining_fields,
      'lenses,
      ('record, 'field, M.t) Lens.mono * 'lens_nil )
    open_record ->
    ('record, 'field) field ->
    ('record, 'cons, 'remaining_fields, 'lenses, 'lens_nil) open_record

  val sealr : ('record, 'cons, 'record, 'lenses, unit) open_record -> 'record t

  val sealr_with_optics :
    ('record, 'cons, 'record, 'lenses, unit) open_record ->
    'record t * 'lenses Lens.t_list
end

module Variant (M : sig
  type t

  val v : < t monad ; t functor_ >
end) : sig
  open Type_core.Make(M)

  type ( 'variant,
         'pat,
         'remaning,
         'remaining_nil,
         'prisms,
         'prism_nil )
       open_variant

  val variant :
    string ->
    'pat ->
    ('variant, 'pat, 'pat, 'pat, 'prism_nil, 'prism_nil) open_variant

  type ('a, 'b, 'c) case

  type 'a case_p

  val case0 : string -> ('a, M.t) app -> ('a, unit, 'a case_p) case

  val case1 :
    string -> 'b t -> ('b -> ('a, M.t) app) -> ('a, 'b, 'b -> 'a case_p) case

  val ( |~ ) :
    ( 'variant,
      'pat,
      'rem,
      'constr -> 'rem_nil,
      'prisms,
      ('variant, 'case, M.t) Prism.mono * 'prism_nil )
    open_variant ->
    ('variant, 'case, 'constr) case ->
    ('variant, 'pat, 'rem, 'rem_nil, 'prisms, 'prism_nil) open_variant

  type ('variant, 'inj) sealer = {
    sealer :
      'pat 'prisms.
      ('variant, 'pat, 'pat, 'variant -> 'inj, 'prisms, unit) open_variant ->
      'variant t * 'prisms Prism.t_list;
  }

  val sealv : ('inj -> ('variant case_p, M.t) app) -> ('variant, 'inj) sealer
end
