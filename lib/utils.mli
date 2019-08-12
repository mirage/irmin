open Ppxlib

module type S = sig
  val ( >|= ) : 'a list -> ('a -> 'b) -> 'b list

  val unlabelled : 'a -> arg_label * 'a

  val compose_all : ('a -> 'a) list -> 'a -> 'a

  val variant_case :
    polymorphic:bool ->
    cons_name:label ->
    ?component_type:expression * int ->
    expression ->
    expression

  val variant_pattern : string -> (pattern option -> pattern) -> int -> case

  val record_field :
    name:label -> field_type:expression -> expression -> expression

  val record_constructor : label_declaration list -> expression

  val variant_constructor : constructor_declaration list -> expression

  val polyvariant_constructor : row_field list -> expression

  val function_encode :
    constructor:('a list -> expression) ->
    accessor:('a -> expression -> expression) ->
    combinator_name:string ->
    sealer_name:string ->
    type_name:string ->
    'a list ->
    expression
end

module Located (S : Ast_builder.S) : S
