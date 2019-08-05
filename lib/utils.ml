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

module Located (A : Ast_builder.S) : S = struct
  open A

  let ( >|= ) x f = List.map f x

  let unlabelled x = (Nolabel, x)

  let compose_all : type a. (a -> a) list -> a -> a =
   fun l x -> List.fold_left (fun x f -> f x) x (List.rev l)

  let generate_identifiers =
    let rec aux acc = function
      | 0 -> acc
      | i ->
          let id = Printf.sprintf "x%d" i in
          aux (id :: acc) (i - 1)
    in
    aux []

  (* [lambda "x" e] is [fun x -> e] *)
  let lambda fparam = Located.mk fparam |> ppat_var |> pexp_fun Nolabel None

  (* |~ case0 "cons_name" Cons_name *)
  let variant_case0 ~cons_name e =
    pexp_apply
      (pexp_ident @@ Located.lident "|~")
      ( [ e;
          pexp_apply
            (pexp_ident @@ Located.lident "case0")
            ( [ pexp_constant @@ Pconst_string (cons_name, None);
                pexp_construct (Located.lident cons_name) None
              ]
            >|= unlabelled )
        ]
      >|= unlabelled )

  (* |~ case1 "cons_name" component_type (fun (x1, ..., xN) -> Cons_name (x1, ..., xN)) *)
  let variant_case1 ~cons_name ~component_type ~idents e =
    let constructor =
      let tuple_pat = idents >|= Located.mk >|= ppat_var |> ppat_tuple in
      let tuple_exp = idents >|= Located.lident >|= pexp_ident |> pexp_tuple in
      let fnbody =
        pexp_construct (Located.lident cons_name) (Some tuple_exp)
      in
      pexp_fun Nolabel None tuple_pat fnbody
    in
    pexp_apply
      (pexp_ident @@ Located.lident "|~")
      ( [ e;
          pexp_apply
            (pexp_ident @@ Located.lident "case1")
            ( [ pexp_constant @@ Pconst_string (cons_name, None);
                component_type;
                constructor
              ]
            >|= unlabelled )
        ]
      >|= unlabelled )

  let variant_case ~cons_name ?component_type =
    match component_type with
    | None -> variant_case0 ~cons_name
    | Some (component_type, n) ->
        let idents = generate_identifiers n in
        variant_case1 ~cons_name ~component_type ~idents

  (* | Cons_name (x1, x2, x3) -> cons_name x1 x2 x3 *)
  let variant_pattern constructor_decl =
    let fparam_of_cdecl c = c.pcd_name.txt |> String.lowercase_ascii in
    let constructor =
      ppat_construct (Located.map_lident constructor_decl.pcd_name)
    in
    match constructor_decl.pcd_args with
    | Pcstr_tuple [] ->
        let lhs = constructor None in
        let rhs =
          pexp_ident @@ Located.lident (fparam_of_cdecl constructor_decl)
        in
        case ~lhs ~guard:None ~rhs
    | Pcstr_tuple components ->
        let idents = generate_identifiers (List.length components) in
        let lhs =
          idents >|= Located.mk >|= ppat_var |> ppat_tuple |> fun x ->
          constructor (Some x)
        in
        let rhs =
          idents >|= Located.lident >|= pexp_ident |> pexp_tuple |> fun x ->
          pexp_apply
            (pexp_ident @@ Located.lident (fparam_of_cdecl constructor_decl))
            [ (Nolabel, x) ]
        in
        case ~lhs ~guard:None ~rhs
    | Pcstr_record _ -> invalid_arg "Inline record types unsupported"

  (* |+ field "field_name" (field_type) (fun t -> t.field_name) *)
  let record_field ~name ~field_type e =
    pexp_apply
      (pexp_ident @@ Located.lident "|+")
      ( [ e;
          pexp_apply
            (pexp_ident @@ Located.lident "field")
            ( [ pexp_constant @@ Pconst_string (name, None);
                field_type;
                pexp_fun Nolabel None
                  (ppat_var @@ Located.mk "t")
                  (pexp_field
                     (pexp_ident @@ Located.lident "t")
                     (Located.lident name))
              ]
            >|= unlabelled )
        ]
      >|= unlabelled )

  (* fun field1 field2 ... fieldN -> { field1; field2; ...; fieldN }) *)
  let record_accessor ~fields =
    let lambda_wrapper = compose_all (fields >|= lambda) in
    let record =
      let rfields =
        fields >|= fun s ->
        (Located.lident s, pexp_ident @@ Located.lident s)
      in
      pexp_record rfields None
    in
    lambda_wrapper record
end
