open Overture
open Type_core

module Record : sig
  type ('record, 'cons, 'remaning, 'lenses, 'lens_nil, 'monad) open_record

  val record :
    string ->
    'cons ->
    ('record, 'cons, 'cons, 'lens_nil, 'lens_nil, 'monad) open_record

  type ('a, 'b) field

  val field :
    string -> 'a t -> ?set:('b -> 'a -> 'b) -> ('b -> 'a) -> ('b, 'a) field

  val ( |+ ) :
    ( 'record,
      'cons,
      'field -> 'remaining_fields,
      'lenses,
      ('record, 'field, 'monad) Optics.Effectful.Lens.mono * 'lens_nil,
      'monad )
    open_record ->
    ('record, 'field) field ->
    ('record, 'cons, 'remaining_fields, 'lenses, 'lens_nil, 'monad) open_record

  val sealr :
    ('record, 'cons, 'record, 'lenses, unit, 'monad) open_record -> 'record t

  val sealr_with_optics :
    ('record, 'cons, 'record, 'lenses, unit, 'm) open_record ->
    'record t * ('m monad -> ('lenses, 'm) Optics.Effectful.Lens.t_list)
end

module Variant : sig
  type ( 'variant,
         'pat,
         'remaning,
         'remaining_nil,
         'prisms,
         'prism_nil,
         'monad )
       open_variant

  val variant :
    string ->
    'pat ->
    ('variant, 'pat, 'pat, 'pat, 'prism_nil, 'prism_nil, 'monad) open_variant

  type ('a, 'b, 'c) case

  type 'a case_p

  val case0 : string -> 'a -> ('a, unit, 'a case_p) case

  val case1 : string -> 'b t -> ('b -> 'a) -> ('a, 'b, 'b -> 'a case_p) case

  val ( |~ ) :
    ( 'variant,
      'pat,
      'rem,
      'constr -> 'rem_nil,
      'prisms,
      ('variant, 'case, 'm) Optics.Effectful.Prism.mono * 'prism_nil,
      'm )
    open_variant ->
    ('variant, 'case, 'constr) case ->
    ('variant, 'pat, 'rem, 'rem_nil, 'prisms, 'prism_nil, 'm) open_variant

  val sealv :
    ( 'variant,
      'pat,
      'pat,
      'variant -> 'variant case_p,
      'prisms,
      unit,
      'm )
    open_variant ->
    'variant t * ('m monad -> ('prisms, 'm) Optics.Effectful.Prism.t_list)
end
