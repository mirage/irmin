open Ppxlib

module type S = sig
  val ( >|= ) : 'a list -> ('a -> 'b) -> 'b list

  val unlabelled : 'a -> arg_label * 'a

  val compose_all : ('a -> 'a) list -> 'a -> 'a

  val variant_case :
    cons_name:label ->
    ?component_type:expression * int ->
    expression ->
    expression

  val variant_pattern : constructor_declaration -> case

  val record_field :
    name:label -> field_type:expression -> expression -> expression

  val record_accessor : fields:label list -> expression
end

module Located (S : Ast_builder.S) : S
