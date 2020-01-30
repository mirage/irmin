module Record (M : S.MONAD) : sig
  open Type_core.Make(M)

  type ('record, 'cons, 'remaning, 'lenses, 'lens_nil) open_record

  module Lens : module type of Optics.Lens (M)

  val record :
    string -> 'cons -> ('record, 'cons, 'cons, 'lens_nil, 'lens_nil) open_record

  type ('a, 'b) field

  val field : string -> 'a t -> ('b -> 'a M.t) -> ('b, 'a) field

  val ( |+ ) :
    ( 'record,
      'cons,
      'field -> 'remaining_fields,
      'lenses,
      ('record, 'field) Lens.mono * 'lens_nil )
    open_record ->
    ('record, 'field) field ->
    ('record, 'cons, 'remaining_fields, 'lenses, 'lens_nil) open_record

  val sealr : ('record, 'cons, 'record, 'lenses, unit) open_record -> 'record t

  val sealr_with_optics :
    ('record, 'cons, 'record, 'lenses, unit) open_record ->
    'record t * 'lenses Lens.t_list
end

module Variant (M : S.MONAD) : sig
  open Type_core.Make(M)

  module Prism : module type of Optics.Prism (M)

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

  val case0 : string -> 'a M.t -> ('a, unit, 'a case_p) case

  val case1 : string -> 'b t -> ('b -> 'a M.t) -> ('a, 'b, 'b -> 'a case_p) case

  val ( |~ ) :
    ( 'variant,
      'pat,
      'rem,
      'constr -> 'rem_nil,
      'prisms,
      ('variant, 'case) Prism.mono * 'prism_nil )
    open_variant ->
    ('variant, 'case, 'constr) case ->
    ('variant, 'pat, 'rem, 'rem_nil, 'prisms, 'prism_nil) open_variant

  val sealv :
    ( 'variant,
      'pat,
      'pat,
      'variant -> 'variant case_p M.t,
      'prisms,
      unit )
    open_variant ->
    'variant t

  val sealv_with_optics :
    ( 'variant,
      'pat,
      'pat,
      'variant -> 'variant case_p M.t,
      'prisms,
      unit )
    open_variant ->
    'variant t * 'prisms Prism.t_list
end
