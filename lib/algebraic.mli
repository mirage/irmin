open Ppxlib

module type S = sig
  val variant_case :
    polymorphic:bool ->
    cons_name:label ->
    ?component_type:expression * int ->
    expression ->
    expression

  val variant_pattern : string -> (pattern option -> pattern) -> int -> case

  val record_field :
    name:label -> field_type:expression -> expression -> expression

  (** Record composites are encoded as a constructor function:

      {[ fun field1 field2 ... fieldN -> { field1; field2; ...; fieldN }) ]} *)

  (** Variant composites are encoded as a destructor function:

      {[
        fun case1 case2 ... caseN -> function
           | Case1 x -> case1 c
           | Case2 (x1, x2) -> case2 x1 x2
            ...
           | CaseN -> casen
      ]} *)

  (** Same as {!variant_composite} but gives the destructor for a
      {i polymorphic} variant *)

  type _ typ =
    | Record : label_declaration typ
    | Variant : constructor_declaration typ
    | Polyvariant : row_field typ

  val function_encode :
    typ:'a typ ->
    accessor:('a -> expression -> expression) ->
    type_name:string ->
    'a list ->
    expression
  (** Combine the various elements necessary for a functional encoding of a
      composite type. *)
end

module Located (S : Ast_builder.S) : S
