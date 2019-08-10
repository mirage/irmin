open Ppxlib
module SSet = Set.Make (String)

(* TODO: get this list dynamically? *)
let irmin_types =
  SSet.of_list
    [ "unit";
      "bool";
      "char";
      "int";
      "int32";
      "int64";
      "float";
      "string";
      "bytes";
      "list";
      "array";
      "option";
      "pair";
      "triple";
      "result"
    ]

module type S = sig
  val derive_str :
    ?name:string -> rec_flag * type_declaration list -> structure_item list

  val derive_sig :
    ?name:string -> rec_flag * type_declaration list -> signature_item list
end

module Located (A : Ast_builder.S) : S = struct
  module Utils = Utils.Located (A)
  open A

  let unlabelled x = (Nolabel, x)

  let ( >|= ) x f = List.map f x

  let seal sealer e =
    pexp_apply
      (pexp_ident @@ Located.lident "|>")
      ([ e; pexp_ident @@ Located.lident sealer ] >|= unlabelled)

  let sealv = seal "sealv"

  let sealr = seal "sealr"

  let open_module = pexp_open Fresh (Located.lident "Irmin.Type")

  let lambda fparam = Located.mk fparam |> ppat_var |> pexp_fun Nolabel None

  let recursive fparam e =
    pexp_apply
      (pexp_ident @@ Located.lident "Irmin.Type.mu")
      ( [ pexp_fun Nolabel None (ppat_var @@ Located.mk fparam) e ]
      >|= unlabelled )

  let witness_name_of_type_name = function "t" -> "t" | x -> x ^ "_t"

  let rec derive_core ~rec_flag ~type_name ~witness_name ~rec_detected typ =
    match typ.ptyp_desc with
    | Ptyp_constr ({ txt = const_name; _ }, args) -> (
        match Attribute.get Attributes.witness typ with
        | Some e -> e
        | None ->
            let lident =
              match const_name with
              | Lident const_name ->
                  let name =
                    (* If this type is the one we are deriving and the 'nonrec' keyword hasn't been used,
                  replace with the witness name *)
                    if
                      rec_flag <> Nonrecursive
                      && String.equal const_name type_name
                    then (
                      rec_detected := true;
                      witness_name
                      (* If not a base type, assume a composite witness with the same naming convention *)
                      )
                    else if not @@ SSet.mem const_name irmin_types then
                      witness_name_of_type_name const_name
                    else const_name
                  in
                  Located.lident name
              | Ldot (lident, name) ->
                  let name = witness_name_of_type_name name in
                  Located.mk @@ Ldot (lident, name)
              | Lapply _ -> invalid_arg "Lident.Lapply not supported"
            in
            let cons_args =
              args
              >|= derive_core ~rec_flag ~type_name ~witness_name ~rec_detected
              >|= unlabelled
            in
            pexp_apply (pexp_ident lident) cons_args )
    | Ptyp_variant (_, Open, _) -> Raise.unsupported_type_polyvar ~loc typ
    (* | Ptyp_variant (rowfields, Closed, labellist) -> *)
    | Ptyp_poly _ -> Raise.unsupported_type_poly ~loc typ
    | Ptyp_tuple args ->
        derive_tuple ~rec_flag ~type_name ~witness_name ~rec_detected args
    | Ptyp_arrow _ -> Raise.unsupported_type_arrow ~loc typ
    | Ptyp_var v -> Raise.unsupported_type_var ~loc v
    | Ptyp_package _ -> Raise.unsupported_type_package ~loc typ
    | Ptyp_extension _ -> Raise.unsupported_type_extension ~loc typ
    | Ptyp_alias _ -> Raise.unsupported_type_alias ~loc typ
    | _ -> invalid_arg "unsupported"

  and derive_tuple ~rec_flag ~type_name ~witness_name ~rec_detected args =
    match args with
    | [ t ] ->
        (* This case can occur when the tuple type is nested inside a variant *)
        derive_core ~rec_flag ~type_name ~witness_name ~rec_detected t
    | _ ->
        let tuple_type =
          match List.length args with
          | 2 -> "pair"
          | 3 -> "triple"
          | n -> Raise.unsupported_tuple_size ~loc n
        in
        args
        >|= derive_core ~rec_flag ~type_name ~witness_name ~rec_detected
        >|= unlabelled
        |> pexp_apply (pexp_ident @@ Located.lident tuple_type)

  and derive_record ~rec_flag ~type_name ~witness_name ~rec_detected ls =
    let rfield_of_ldecl label_decl e =
      let name = label_decl.pld_name.txt in
      let field_type =
        derive_core ~rec_flag ~type_name ~witness_name ~rec_detected
          label_decl.pld_type
      in
      Utils.record_field ~name ~field_type e
    in
    let accessor =
      let fields =
        ls >|= fun l ->
        l.pld_name.txt
      in
      Utils.record_accessor ~fields
    in
    let cases = Utils.compose_all (List.rev ls >|= rfield_of_ldecl) in
    pexp_apply
      (pexp_ident @@ Located.lident "record")
      ([ estring type_name; accessor ] >|= unlabelled)
    |> cases |> sealr

  and derive_variant ~rec_flag ~type_name ~witness_name ~rec_detected name cs =
    let fparam_of_cdecl c = c.pcd_name.txt |> String.lowercase_ascii in
    let variant_case_of_cdecl c =
      let cons_name = c.pcd_name.txt in
      let component_type =
        match c.pcd_args with
        | Pcstr_record _ -> invalid_arg "inline record types unsupported"
        | Pcstr_tuple [] -> None
        | Pcstr_tuple cs ->
            Some
              ( derive_tuple ~rec_flag ~type_name ~witness_name ~rec_detected cs,
                List.length cs )
      in
      (cons_name, component_type)
    in
    let cases =
      let case_wrappers =
        cs >|= variant_case_of_cdecl >|= fun (cons_name, component_type) ->
        Utils.variant_case ~cons_name ?component_type
      in
      Utils.compose_all (List.rev case_wrappers)
    in
    let variant_accessor =
      cs >|= Utils.variant_pattern |> pexp_function
      |> Utils.compose_all (cs >|= fparam_of_cdecl >|= lambda)
    in
    pexp_apply
      (pexp_ident @@ Located.lident "variant")
      ([ estring name; variant_accessor ] >|= unlabelled)
    |> cases |> sealv

  let derive_sig ?name input_ast =
    match input_ast with
    | _, [ typ ] ->
        let type_name = typ.ptype_name.txt in
        let name =
          Located.mk
            ( match name with
            | Some n -> n
            | None -> witness_name_of_type_name type_name )
        in
        let type_ =
          ptyp_constr
            (Located.lident "Irmin.Type.t")
            [ ptyp_constr (Located.lident type_name) [] ]
        in
        [ psig_value (value_description ~name ~type_ ~prim:[]) ]
    | _ -> invalid_arg "Multiple type declarations not supported"

  let derive_str ?name input_ast =
    match input_ast with
    | rec_flag, [ typ ] ->
        let type_name = typ.ptype_name.txt in
        let witness_name =
          match name with
          | Some s -> s
          | None -> witness_name_of_type_name type_name
        in
        let kind = typ.ptype_kind in
        let rec_detected = ref false in
        let expr =
          match kind with
          | Ptype_abstract -> (
              match typ.ptype_manifest with
              | None -> invalid_arg "No manifest"
              | Some c -> (
                  match c.ptyp_desc with
                  (* No need to open Irmin.Type module *)
                  | Ptyp_constr ({ txt; loc = _ }, []) -> (
                      match Attribute.get Attributes.witness c with
                      | Some e -> e
                      | None -> (
                          match txt with
                          | Lident cons_name ->
                              if SSet.mem cons_name irmin_types then
                                pexp_ident
                                  (Located.lident @@ "Irmin.Type." ^ cons_name)
                              else
                                (* If not a basic type, assume a composite witness /w same naming convention *)
                                pexp_ident
                                  ( Located.lident
                                  @@ witness_name_of_type_name cons_name )
                          | Ldot (lident, cons_name) ->
                              pexp_ident
                                ( Located.mk
                                @@ Ldot
                                     ( lident,
                                       witness_name_of_type_name cons_name ) )
                          | Lapply _ ->
                              invalid_arg "Lident.Lapply not supported" ) )
                  (* Type constructor: list, tuple, etc. *)
                  | _ ->
                      derive_core ~rec_flag ~type_name ~witness_name
                        ~rec_detected c
                      |> open_module ) )
          | Ptype_variant cs ->
              derive_variant ~rec_flag ~type_name ~witness_name ~rec_detected
                type_name cs
              |> open_module
          | Ptype_record ls ->
              derive_record ~rec_flag ~type_name ~witness_name ~rec_detected ls
              |> open_module
          | Ptype_open -> Raise.unsupported_type_open ~loc
        in
        (* If the type is syntactically self-referential and the user has not asserted 'nonrec' in the type
           declaration, wrap in a 'mu' combinator *)
        let expr =
          if !rec_detected && not (rec_flag == Nonrecursive) then
            recursive witness_name expr
          else expr
        in
        let pat = ppat_var @@ Located.mk @@ witness_name in
        [ pstr_value Nonrecursive [ value_binding ~pat ~expr ] ]
    | _ -> invalid_arg "Multiple type declarations not supported"
end
