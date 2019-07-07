open Ppxlib

let name = "irmin"

let rec derive_core: location -> core_type -> expression = fun loc typ ->
  let (module S) = Ast_builder.make loc in
  let open S in

  match typ.ptyp_desc with
  | Ptyp_constr ({txt = Lident const_name; _}, args) ->
    List.map (fun t -> (Nolabel, derive_core loc t)) args
    |> pexp_apply (pexp_ident @@ Located.lident const_name)

  | Ptyp_tuple args ->
    let tuple_type = match List.length args with
    | 2 -> "pair"
    | 3 -> "triple"
    | l -> invalid_arg (Printf.sprintf "Tuples must have 2 or 3 components. Found %d" l)
    in
    List.map (fun t -> (Nolabel, derive_core loc t)) args
    |> pexp_apply (pexp_ident @@ Located.lident tuple_type)

  | Ptyp_arrow _ -> invalid_arg "Arrow types unsupported"
  | _ -> invalid_arg "Unsupported type"

and derive_variant: location -> string -> constructor_declaration list -> expression = fun loc name _cs ->
  let (module S) = Ast_builder.make loc in
  let open S in

  pexp_apply (pexp_ident @@ Located.lident "variant") (Nolabel, estring name)


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
