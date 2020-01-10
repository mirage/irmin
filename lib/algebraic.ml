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
end

module Located (A : Ast_builder.S) : S = struct
  open A

  let ( >|= ) x f = List.map f x

  let unlabelled x = (Nolabel, x)

  let compose_all : type a. (a -> a) list -> a -> a =
   fun l x -> List.fold_left ( |> ) x (List.rev l)

  let generate_identifiers n =
    List.init n (fun i -> Printf.sprintf "x%d" (i + 1))

  (* [lambda "x" e] is [fun x -> e] *)
  let lambda fparam = pvar fparam |> pexp_fun Nolabel None

  (* |~ case0 "cons_name" [`]Cons_name *)
  let variant_case0 ~polymorphic ~cons_name e =
    let fnbody =
      if polymorphic then pexp_variant cons_name None
      else pexp_construct (Located.lident cons_name) None
    in
    pexp_apply (evar "|~")
      ( [
          e;
          pexp_apply (evar "case0")
            ( [ pexp_constant @@ Pconst_string (cons_name, None); fnbody ]
            >|= unlabelled );
        ]
      >|= unlabelled )

  (* |~ case1 "cons_name" component_type (fun (x1, ..., xN) -> [`]Cons_name (x1, ..., xN)) *)
  let variant_case1 ~polymorphic ~cons_name ~component_type ~idents e =
    let constructor =
      let tuple_pat = idents >|= Located.mk >|= ppat_var |> ppat_tuple in
      let tuple_exp = idents >|= evar |> pexp_tuple in
      let fnbody =
        if polymorphic then pexp_variant cons_name (Some tuple_exp)
        else pexp_construct (Located.lident cons_name) (Some tuple_exp)
      in
      pexp_fun Nolabel None tuple_pat fnbody
    in
    pexp_apply (evar "|~")
      ( [
          e;
          pexp_apply (evar "case1")
            ( [
                pexp_constant @@ Pconst_string (cons_name, None);
                component_type;
                constructor;
              ]
            >|= unlabelled );
        ]
      >|= unlabelled )

  let variant_case ~polymorphic ~cons_name ?component_type =
    match component_type with
    | None -> variant_case0 ~polymorphic ~cons_name
    | Some (component_type, n) ->
        let idents = generate_identifiers n in
        variant_case1 ~cons_name ~polymorphic ~component_type ~idents

  (* | Cons_name (x1, x2, x3) -> cons_name x1 x2 x3 *)
  let variant_pattern cons_name pattern n =
    let fparam_of_name name = String.lowercase_ascii name in
    match n with
    | 0 ->
        let lhs = pattern None in
        let rhs = evar (fparam_of_name cons_name) in
        case ~lhs ~guard:None ~rhs
    | n ->
        let idents = generate_identifiers n in
        let lhs = idents >|= pvar |> ppat_tuple |> fun x -> pattern (Some x) in
        let rhs =
          idents >|= evar |> pexp_tuple |> fun x ->
          pexp_apply (evar (fparam_of_name cons_name)) [ (Nolabel, x) ]
        in
        case ~lhs ~guard:None ~rhs

  (* |+ field "field_name" (field_type) (fun t -> t.field_name) *)
  let record_field ~name ~field_type e =
    pexp_apply (evar "|+")
      ( [
          e;
          pexp_apply (evar "field")
            ( [
                pexp_constant @@ Pconst_string (name, None);
                field_type;
                lambda "t" (pexp_field (evar "t") (Located.lident name));
              ]
            >|= unlabelled );
        ]
      >|= unlabelled )

  type _ typ =
    | Record : label_declaration typ
    | Variant : constructor_declaration typ
    | Polyvariant : row_field typ

  let combinator_of_typ : type a. a typ -> string = function
    | Record -> "record"
    | Variant -> "variant"
    | Polyvariant -> "variant"

  let sealer_of_typ : type a. a typ -> expression -> expression =
    let seal name e =
      pexp_apply (evar "|>") ([ e; evar name ] >|= unlabelled)
    in
    function Record -> seal "sealr" | Variant | Polyvariant -> seal "sealv"

  let record_composite fields =
    let fields = fields >|= fun l -> l.pld_name.txt in
    let lambda_wrapper = compose_all (fields >|= lambda) in
    let record =
      let rfields = fields >|= fun s -> (Located.lident s, evar s) in
      pexp_record rfields None
    in
    lambda_wrapper record

  let variant_composite cs =
    let fparam_of_cdecl c = c.pcd_name.txt |> String.lowercase_ascii in
    let pattern_of_cdecl c =
      let pattern = ppat_construct (Located.map_lident c.pcd_name) in
      let n =
        match c.pcd_args with
        | Pcstr_tuple args -> List.length args
        | Pcstr_record _ -> invalid_arg "Inline record types unsupported"
      in
      variant_pattern c.pcd_name.txt pattern n
    in
    cs
    >|= pattern_of_cdecl
    |> pexp_function
    |> compose_all (cs >|= fparam_of_cdecl >|= lambda)

  let polyvariant_composite fs =
    let fparam_of_rowfield f =
      match f.prf_desc with
      | Rtag (label, _, _) -> String.lowercase_ascii label.txt
      | Rinherit _ -> assert false
    in
    let pattern_case_of_rowfield f =
      match f.prf_desc with
      | Rtag ({ txt; _ }, _, typs) ->
          let pattern = ppat_variant txt in
          let n = List.length typs in
          variant_pattern txt pattern n
      | Rinherit _ -> assert false
    in
    fs
    >|= pattern_case_of_rowfield
    |> pexp_function
    |> compose_all (fs >|= fparam_of_rowfield >|= lambda)

  let function_encode :
      type a.
      typ:a typ ->
      accessor:(a -> expression -> expression) ->
      type_name:string ->
      a list ->
      expression =
   fun ~typ ~accessor ~type_name ts ->
    let composite =
      match typ with
      | Record -> record_composite ts
      | Variant -> variant_composite ts
      | Polyvariant -> polyvariant_composite ts
    in
    let cases = ts >|= accessor |> List.rev |> compose_all in
    pexp_apply
      (evar (combinator_of_typ typ))
      ([ estring type_name; composite ] >|= unlabelled)
    |> cases
    |> sealer_of_typ typ
end
