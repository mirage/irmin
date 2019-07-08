open Ppxlib

let name = "irmin"

let rec derive_core: location -> core_type -> expression = fun loc typ ->
  let (module S) = Ast_builder.make loc in
  let open S in

  match typ.ptyp_desc with
  | Ptyp_constr ({txt = Lident const_name; _}, args) ->
    List.map (fun t -> (Nolabel, derive_core loc t)) args
    |> pexp_apply (pexp_ident @@ Located.lident const_name)

  | Ptyp_tuple args -> derive_tuple loc args

  | Ptyp_arrow _ -> invalid_arg "Arrow types unsupported"
  | _ -> invalid_arg "Unsupported type"


and derive_tuple: location -> core_type list -> expression  = fun loc args ->
  let (module S) = Ast_builder.make loc in
  let open S in

  let tuple_type = match List.length args with
    | 2 -> "pair"
    | 3 -> "triple"
    | l -> invalid_arg (Printf.sprintf "Tuples must have 2 or 3 components. Found %d." l)
  in
  List.map (fun t -> (Nolabel, derive_core loc t)) args
  |> pexp_apply (pexp_ident @@ Located.lident tuple_type)


and derive_variant: location -> string -> constructor_declaration list -> expression = fun loc name cs ->
  let (module S) = Ast_builder.make loc in
  let open S in


  let fparam_of_cdecl c =
    c.pcd_name.txt
    |> String.lowercase_ascii
  in

  let function_of_cdecl c =
    fparam_of_cdecl c
    |> Located.mk
    |> ppat_var
    |> pexp_fun Nolabel None
  in

  (* | Cons_name (x1, x2, x3) -> cons_name x1 x2 x3 *)
  let pcase_of_cdecl c =
    match c.pcd_args with
    | Pcstr_tuple [] ->
      case
        ~lhs:(ppat_construct (Located.map_lident c.pcd_name) None)
        ~guard:None
        ~rhs:(pexp_ident @@ Located.lident (fparam_of_cdecl c))

    | Pcstr_tuple components ->
      let idents =
        (* TODO: refactor out magic identifier 'x' ? *)
        List.mapi (fun i _ -> Printf.sprintf "x%d" (i+1)) components
      in
      case
        ~lhs:(ppat_construct
                (Located.map_lident c.pcd_name)
                (idents
                 |> List.map Located.mk
                 |> List.map ppat_var
                 |> ppat_tuple
                 |> fun x -> Some x))
        ~guard:None
        ~rhs:(pexp_apply
                (pexp_ident @@ Located.lident (fparam_of_cdecl c))
                [(Nolabel, (pexp_tuple
                (idents
                 |> List.map Located.lident
                 |> List.map pexp_ident)))]
             )

    | Pcstr_record _ -> invalid_arg "Record types unsupported"
  in

  let vcase_of_cdecl c = fun e ->
    let cons_name = c.pcd_name.txt in
    match c.pcd_args with
    | Pcstr_record _ -> invalid_arg "Record types unsupported"
    | Pcstr_tuple [] -> (
        (* |~ case0 "cons_name" Cons_name *)
        pexp_apply (pexp_ident @@ Located.lident "|~") [
          (Nolabel, e);
          (Nolabel, pexp_apply (pexp_ident @@ Located.lident "case0") [
              (Nolabel, pexp_constant @@ Pconst_string (cons_name, None));
              (Nolabel, pexp_construct (Located.lident cons_name) None)
            ])
        ])
    | Pcstr_tuple components -> (
        (* |~ case1 "cons_name" component_type (fun (x1, ..., xN) -> Cons_name (x1, ..., xN)) *)

        let idents =
          (* TODO: refactor out magic identifier 'x' ? *)
          List.mapi (fun i _ -> Printf.sprintf "x%d" (i+1)) components
        in

        pexp_apply (pexp_ident @@ Located.lident "|~") [
          (Nolabel, e);
          (Nolabel, pexp_apply (pexp_ident @@ Located.lident "case1") [
              (Nolabel, pexp_constant @@ Pconst_string (cons_name, None));
              (Nolabel,
               match components with
               | [t] -> derive_core loc t
               | c   -> derive_tuple loc c);
              (Nolabel, pexp_fun
                 Nolabel
                 None
                 (idents
                  |> List.map Located.mk
                  |> List.map ppat_var
                  |> ppat_tuple)
                 (pexp_construct
                    (Located.lident cons_name)
                    (Some (pexp_tuple
                              (idents
                               |> List.map Located.lident
                               |> List.map pexp_ident))))
              )
            ])
        ])
  in

  let compose_all = List.fold_left (fun f g x -> f (g x)) (fun x -> x) in

  let nested = List.map function_of_cdecl cs |> compose_all in
  let cases = List.rev cs |> List.map vcase_of_cdecl |> compose_all in

  let pattern =
    List.map pcase_of_cdecl cs
    |> pexp_function
  in

  pexp_apply (pexp_ident @@ Located.lident "|>")
    [
      (Nolabel, cases @@ pexp_apply (pexp_ident @@ Located.lident "variant") [
          (Nolabel, estring name);
          (Nolabel, nested pattern)
        ]);
      (Nolabel, pexp_ident @@ Located.lident "sealv")
    ]

let expand_str ~loc ~path:_ (input_ast: rec_flag * type_declaration list) =
  let (module S) = Ast_builder.make loc in
  let open S in

  let open_type = pexp_open Fresh (Located.lident "Irmin.Type") in

  match input_ast with
  | (_, [typ]) -> (
      let name = typ.ptype_name.txt in
      let kind = typ.ptype_kind in

      let expr = (match kind with
          | Ptype_abstract -> (
              match typ.ptype_manifest with
              | None -> invalid_arg "No manifest"
              | Some c -> (match c.ptyp_desc with

                  (* Basic type: unit, int, string etc. *)
                  | Ptyp_constr ({txt = Lident type_name; loc = _}, []) ->

                    (* TODO: don't string concatenate here*)
                    evar ("Irmin.Type." ^ type_name)

                  (* Type constructor: list, tuple,  etc. *)
                  | _ -> derive_core loc c |> open_type
                ))

          | Ptype_variant cs -> derive_variant loc name cs |> open_type
          | Ptype_record _ -> invalid_arg "Record types unsupported"
          | Ptype_open -> invalid_arg "Open types unsupported") in

      [ pstr_value Nonrecursive [ value_binding ~pat:(ppat_var @@ S.Located.mk name) ~expr]]
    )

  | _ -> invalid_arg "Multiple type declarations not supported"

let str_type_decl_generator =
  Deriving.Generator.make_noarg expand_str

let irmin =
  Deriving.add
    ~str_type_decl:str_type_decl_generator
    name

