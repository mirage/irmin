open Ppxlib

module Utils = struct
  let (>|=) x f = List.map f x

  let unlabelled x = (Nolabel, x)
  let compose_all = List.fold_left (fun f g x -> f (g x)) (fun x -> x)
end

module SSet = Set.Make(String)

(* TODO: get this list dynamically? *)
let irmin_types = SSet.of_list
    [ "unit"; "bool"; "char"; "int"; "int32"; "int64"; "float"; "string"; "bytes";
      "list"; "array"; "option"; "pair"; "triple"; "result" ]

module type S = sig
  val derive_str: ?name:string -> (rec_flag * type_declaration list) -> structure_item list
  val derive_sig: ?name:string -> (rec_flag * type_declaration list) -> signature_item list
end

module Located (S: Ast_builder.S): S = struct
  open S
  open Utils

  let seal sealer e =
    pexp_apply
      (pexp_ident @@ Located.lident "|>")
      ([e; pexp_ident @@ Located.lident sealer] >|= unlabelled)

  let sealv = seal "sealv"
  let sealr = seal "sealr"

  let open_module =
    pexp_open Fresh (Located.lident "Irmin.Type")

  let lambda fparam =
    Located.mk fparam
    |> ppat_var
    |> pexp_fun Nolabel None

  let recursive fparam e =
    pexp_apply (pexp_ident @@ Located.lident "Irmin.Type.mu") ([
        pexp_fun Nolabel None (ppat_var @@ Located.mk fparam)
          e;
      ] >|= unlabelled)

  let witness_name_of_type_name = function
    | "t" -> "t"
    | x   -> (x ^ "_t")

  let rec derive_core ~rec_flag ~type_name ~witness_name ~rec_detected typ =
    match typ.ptyp_desc with
    | Ptyp_constr ({txt=const_name; _}, args) -> (

        match Attribute.get Attributes.witness typ with
        | Some e -> e
        | None -> (
            let cons_args =
              args
              >|= derive_core ~rec_flag ~type_name ~witness_name ~rec_detected
              >|= unlabelled
            in
            let lident = match const_name with
            | Lident const_name ->
              let name =
                (* If this type is the one we are deriving and the 'nonrec' keyword hasn't been used,
                  replace with the witness name *)
                if (rec_flag <> Nonrecursive) && String.equal const_name type_name then
                  (rec_detected := true; witness_name)

                (* If not a base type, assume a composite witness with the same naming convention *)
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
          pexp_apply (pexp_ident lident) cons_args
        )
      )

    | Ptyp_tuple args -> derive_tuple ~rec_flag ~type_name ~witness_name ~rec_detected args

    | Ptyp_arrow _     -> Raise.unsupported_type_arrow ~loc typ
    | Ptyp_var v       -> Raise.unsupported_type_var ~loc v
    | Ptyp_poly _      -> Raise.unsupported_type_poly ~loc typ
    | Ptyp_variant _   -> Raise.unsupported_type_polyvar ~loc typ
    | Ptyp_package _   -> Raise.unsupported_type_package ~loc typ
    | Ptyp_extension _ -> Raise.unsupported_type_extension ~loc typ
    | _ -> invalid_arg "unsupported"

  and derive_tuple ~rec_flag ~type_name ~witness_name ~rec_detected args =
    let tuple_type = match List.length args with
      | 2 -> "pair"
      | 3 -> "triple"
      | n -> Raise.unsupported_tuple_size ~loc n
    in
    args
    >|= derive_core ~rec_flag ~type_name ~witness_name ~rec_detected
    >|= unlabelled
    |> pexp_apply (pexp_ident @@ Located.lident tuple_type)


  and derive_record ~rec_flag ~type_name ~witness_name ~rec_detected ls =
    let rcase_of_ldecl l = fun e ->
      let label_name = l.pld_name.txt in

      (* |+ field "field_name" (field_type) (fun t -> t.field_name) *)
      pexp_apply (pexp_ident @@ Located.lident "|+") ([
          e;
          pexp_apply (pexp_ident @@ Located.lident "field") ([
              pexp_constant @@ Pconst_string (label_name, None);
              derive_core ~rec_flag ~type_name ~witness_name ~rec_detected l.pld_type;
              pexp_fun Nolabel None
                (ppat_var @@ Located.mk "t")
                (pexp_field
                   (pexp_ident @@ Located.lident "t")
                   (Located.map_lident l.pld_name))
            ] >|= unlabelled)
        ] >|= unlabelled)
    in

    let record_accessor =
      ls
      >|= (fun l -> l.pld_name.txt)
      >|= (fun s -> (Located.lident s, pexp_ident @@ Located.lident s))
      |> (fun fields -> pexp_record fields None)
      |> compose_all (ls >|= (fun l -> lambda l.pld_name.txt))
    in

    let cases =
      compose_all (List.rev ls >|= rcase_of_ldecl)
    in

    pexp_apply (pexp_ident @@ Located.lident "record")
      ([estring type_name; record_accessor] >|= unlabelled)
    |> cases
    |> sealr

  and derive_variant ~rec_flag ~type_name ~witness_name ~rec_detected name cs =
    let fparam_of_cdecl c =
      c.pcd_name.txt
      |> String.lowercase_ascii
    in

    let generate_identifiers =
      List.mapi (fun i _ -> Printf.sprintf "x%d" (i+1))
    in

    (* | Cons_name (x1, x2, x3) -> cons_name x1 x2 x3 *)
    let pcase_of_cdecl c =
      let constructor = ppat_construct (Located.map_lident c.pcd_name) in
      match c.pcd_args with
      | Pcstr_tuple [] ->
        let lhs = constructor None in
        let rhs = pexp_ident @@ Located.lident (fparam_of_cdecl c) in
        case ~lhs ~guard:None ~rhs

      | Pcstr_tuple components ->
        let idents = generate_identifiers components in
        let lhs =
          idents
          >|= Located.mk
          >|= ppat_var
          |> ppat_tuple
          |> fun x -> constructor (Some x)
        in
        let rhs =
          idents
          >|= Located.lident
          >|= pexp_ident
          |> pexp_tuple
          |> fun x -> pexp_apply (pexp_ident @@ Located.lident (fparam_of_cdecl c)) [(Nolabel, x)]
        in
        case ~lhs ~guard:None ~rhs

      | Pcstr_record _ -> invalid_arg "Record types unsupported"
    in

    let vcase_of_cdecl c = fun e ->
      let cons_name = c.pcd_name.txt in
      match c.pcd_args with
      | Pcstr_record _ -> invalid_arg "Record types unsupported"
      | Pcstr_tuple [] -> (

          (* |~ case0 "cons_name" Cons_name *)
          pexp_apply (pexp_ident @@ Located.lident "|~") ([
              e;
              pexp_apply (pexp_ident @@ Located.lident "case0") ([
                  pexp_constant @@ Pconst_string (cons_name, None);
                  pexp_construct (Located.lident cons_name) None
                ] >|= unlabelled)
            ] >|= unlabelled))

      | Pcstr_tuple components ->

        (* |~ case1 "cons_name" component_type (fun (x1, ..., xN) -> Cons_name (x1, ..., xN)) *)
        let idents = generate_identifiers components in
        let component_type = match components with
          | [t] -> derive_core  ~rec_flag ~type_name ~witness_name ~rec_detected t
          | c   -> derive_tuple ~rec_flag ~type_name ~witness_name ~rec_detected c
        in
        let constructor =
          let tuple_pat = idents >|= Located.mk     >|= ppat_var   |> ppat_tuple in
          let tuple_exp = idents >|= Located.lident >|= pexp_ident |> pexp_tuple in
          let fnbody = pexp_construct (Located.lident cons_name) (Some tuple_exp) in
          pexp_fun Nolabel None tuple_pat fnbody
        in

        pexp_apply (pexp_ident @@ Located.lident "|~") ([
            e;
            pexp_apply (pexp_ident @@ Located.lident "case1") ([
                pexp_constant @@ Pconst_string (cons_name, None);
                component_type;
                constructor
              ] >|= unlabelled)
          ] >|= unlabelled)

    in
    let cases = compose_all (List.rev cs >|= vcase_of_cdecl) in
    let variant_accessor =
      cs
      >|= pcase_of_cdecl
      |> pexp_function
      |> compose_all (cs >|= fparam_of_cdecl >|= lambda)
    in
    pexp_apply (pexp_ident @@ Located.lident "variant")
      ([estring name; variant_accessor] >|= unlabelled)
    |> cases
    |> sealv

  let derive_sig ?name input_ast =
    match input_ast with
    | (_, [typ]) -> (
        let type_name = typ.ptype_name.txt in
        let name = Located.mk (match name with
            | Some n -> n
            | None -> witness_name_of_type_name type_name)
        in
        let type_ =
          ptyp_constr
            (Located.lident ("Irmin.Type.t"))
            [ptyp_constr (Located.lident type_name) []]
        in
        [ psig_value (value_description ~name ~type_ ~prim:[]) ]
      )
    | _ -> invalid_arg "Multiple type declarations not supported"

  let derive_str ?name input_ast =
    match input_ast with
    | (rec_flag, [typ]) -> (
        let type_name = typ.ptype_name.txt in
        let witness_name = match name with
          | Some s -> s
          | None -> witness_name_of_type_name type_name in
        let kind = typ.ptype_kind in
        let rec_detected = ref false in

        let expr = match kind with
          | Ptype_abstract -> (
              match typ.ptype_manifest with
              | None -> invalid_arg "No manifest"
              | Some c -> match c.ptyp_desc with

                (* No need to open Irmin.Type module *)
                | Ptyp_constr ({txt; loc = _}, []) -> (
                    match Attribute.get Attributes.witness c with
                    | Some e -> e
                    | None -> (
                      match txt with
                      | Lident cons_name -> (
                        if SSet.mem cons_name irmin_types then
                          pexp_ident (Located.lident @@ "Irmin.Type." ^ cons_name)
                        else
                          (* If not a basic type, assume a composite witness /w same naming convention *)
                          pexp_ident (Located.lident @@ witness_name_of_type_name cons_name)
                      )
                      | Ldot (lident, cons_name) ->
                          pexp_ident (Located.mk @@ Ldot (lident, witness_name_of_type_name cons_name))

                      | Lapply _ -> invalid_arg "Lident.Lapply not supported"

                    )
                  )

                (* Type constructor: list, tuple, etc. *)
                | _ -> derive_core ~rec_flag ~type_name ~witness_name ~rec_detected c |> open_module)

          | Ptype_variant cs ->
            derive_variant ~rec_flag ~type_name ~witness_name ~rec_detected type_name cs |> open_module
          | Ptype_record ls ->
            derive_record  ~rec_flag ~type_name ~witness_name ~rec_detected ls |> open_module
          | Ptype_open -> Raise.unsupported_type_open ~loc
        in

        (* If the type is syntactically self-referential and the user has not asserted 'nonrec' in the type
           declaration, wrap in a 'mu' combinator *)
        let expr = if !rec_detected && not (rec_flag == Nonrecursive) then
            recursive witness_name expr
          else
            expr
        in
        let pat = ppat_var @@ Located.mk @@ witness_name in
        [ pstr_value Nonrecursive [ value_binding ~pat ~expr ]]
      )

    | _ -> invalid_arg "Multiple type declarations not supported"

end
