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

  let open_type =
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

  let derive_sig ?name input_ast =
    match input_ast with
    | (_, [typ]) -> (
        let type_name = typ.ptype_name.txt in
        let witness_name = match name with
          | Some n -> n
          | None -> witness_name_of_type_name typ.ptype_name.txt
        in
        [
          psig_value @@ value_description
            ~name:(Located.mk @@ witness_name)
            ~type_:(ptyp_constr
                      (Located.lident ("Irmin.Type.t"))
                      [ptyp_constr (Located.lident type_name) []]
                   )
            ~prim:[]
        ]
      )
    | _ -> invalid_arg "Multiple type declarations not supported"


  let rec derive_str ?name input_ast =
    match input_ast with
    | (_, [typ]) -> (

        let type_name = typ.ptype_name.txt in
        let witness_name = match name with
          | Some s -> s
          | None -> witness_name_of_type_name type_name in
        let kind = typ.ptype_kind in
        let rec_flag = ref false in

        let expr = (
          match kind with
          | Ptype_abstract -> (
              match typ.ptype_manifest with
              | None -> invalid_arg "No manifest"
              | Some c -> (match c.ptyp_desc with
                  (* No need to open Irmin.Type *)
                  | Ptyp_constr ({txt = Lident cons_name; loc = _}, []) -> (

                      match Attribute.get Attributes.witness c with
                      | Some e -> e
                      | None -> (

                          if SSet.mem cons_name irmin_types then
                            pexp_ident (Located.lident @@ "Irmin.Type." ^ cons_name)
                          else
                            (* If not a basic type, assume a composite witness /w same naming convention *)
                            pexp_ident (Located.lident @@ witness_name_of_type_name cons_name)

                        )
                    )

                  (* Type constructor: list, tuple, etc. *)
                  | _ -> derive_core ~type_name ~witness_name ~rec_flag c |> open_type
                ))

          | Ptype_variant cs -> derive_variant ~type_name ~witness_name ~rec_flag type_name cs |> open_type
          | Ptype_record ls ->  derive_record  ~type_name ~witness_name ~rec_flag ls |> open_type
          | Ptype_open -> invalid_arg "Open types unsupported"
        )
        in

        let expr = expr |> if !rec_flag then recursive witness_name else (fun x -> x) in
        [ pstr_value Nonrecursive [ value_binding ~pat:(ppat_var @@ Located.mk @@ witness_name) ~expr]]
      )

    | _ -> invalid_arg "Multiple type declarations not supported"

  and derive_core ~type_name ~witness_name ~rec_flag typ =
    match typ.ptyp_desc with
    | Ptyp_constr ({txt = Lident const_name; _}, args) -> (

        match Attribute.get Attributes.witness typ with
        | Some e -> e
        | None -> (

            let name =
              (* If this type is the one we are deriving, replace with the witness name *)
              if String.equal const_name type_name then
                (rec_flag := true; witness_name)

              (* If not a base type, assume a composite witness with the same naming convention *)
              else if not @@ SSet.mem const_name irmin_types then
                witness_name_of_type_name const_name

              else const_name
            in

            args
            >|= derive_core ~type_name ~witness_name ~rec_flag
            >|= unlabelled
            |> pexp_apply (pexp_ident @@ Located.lident name)
          )
      )

    | Ptyp_tuple args -> derive_tuple ~type_name ~witness_name ~rec_flag args

    | Ptyp_arrow _ -> invalid_arg "Arrow types unsupported"
    | _ -> invalid_arg "Unsupported type"


  and derive_tuple ~type_name ~witness_name ~rec_flag args =
    let tuple_type = match List.length args with
      | 2 -> "pair"
      | 3 -> "triple"
      | l -> invalid_arg (Printf.sprintf "Tuples must have 2 or 3 components. Found %d." l)
    in
    args
    >|= derive_core ~type_name ~witness_name ~rec_flag
    >|= unlabelled
    |> pexp_apply (pexp_ident @@ Located.lident tuple_type)

  and derive_record ~type_name ~witness_name ~rec_flag ls =
    let rcase_of_ldecl l = fun e ->
      let label_name = l.pld_name.txt in

      (* |+ field "field_name" (field_type) (fun t -> t.field_name) *)
      pexp_apply (pexp_ident @@ Located.lident "|+") ([
          e;
          pexp_apply (pexp_ident @@ Located.lident "field") ([
              pexp_constant @@ Pconst_string (label_name, None);
              derive_core ~type_name ~witness_name ~rec_flag l.pld_type;
              pexp_fun
                Nolabel
                None
                (ppat_var @@ Located.mk "t")
                (pexp_field
                   (pexp_ident @@ Located.lident "t")
                   (Located.map_lident l.pld_name))
            ] >|= unlabelled)
        ] >|= unlabelled)
    in

    let nested =
      ls
      >|= (fun l -> l.pld_name.txt)
      >|= lambda
      |> compose_all
    in

    let record =
      (ls
       >|= (fun l -> l.pld_name.txt)
       >|= (fun s -> (Located.lident s, pexp_ident @@ Located.lident s))
       |> pexp_record)
        None
    in

    let cases =
      List.rev ls
      >|= rcase_of_ldecl
      |> compose_all
    in

    cases @@ pexp_apply (pexp_ident @@ Located.lident "record")
      ([estring type_name; nested record] >|= unlabelled)
    |> sealr

  and derive_variant: type_name:string -> witness_name:string -> rec_flag:bool ref
    -> string -> constructor_declaration list -> expression
    = fun ~type_name ~witness_name ~rec_flag name cs ->

      let fparam_of_cdecl c =
        c.pcd_name.txt
        |> String.lowercase_ascii
      in

      let generate_identifiers =
        List.mapi (fun i _ -> Printf.sprintf "x%d" (i+1))
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
          let idents = generate_identifiers components in
          case
            ~lhs:(ppat_construct
                    (Located.map_lident c.pcd_name)
                    (
                      idents
                      >|= Located.mk
                      >|= ppat_var
                      |> ppat_tuple
                      |> fun x -> Some x
                    )
                 )
            ~guard:None
            ~rhs:(pexp_apply
                    (pexp_ident @@ Located.lident (fparam_of_cdecl c))
                    [
                      idents
                      >|= Located.lident
                      >|= pexp_ident
                      |> pexp_tuple
                      |> unlabelled
                    ]
                 )

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

        | Pcstr_tuple components -> (

            (* |~ case1 "cons_name" component_type (fun (x1, ..., xN) -> Cons_name (x1, ..., xN)) *)
            let idents = generate_identifiers components in

            pexp_apply (pexp_ident @@ Located.lident "|~") ([
                e;
                pexp_apply (pexp_ident @@ Located.lident "case1") ([
                    pexp_constant @@ Pconst_string (cons_name, None);

                    (match components with
                     | [t] -> derive_core ~type_name ~witness_name ~rec_flag t
                     | c   -> derive_tuple ~type_name ~witness_name ~rec_flag c);

                    (pexp_fun Nolabel None
                       (idents >|= Located.mk >|= ppat_var |> ppat_tuple)
                       (pexp_construct
                          (Located.lident cons_name)
                          (Some (pexp_tuple (idents >|= Located.lident >|= pexp_ident)))))

                  ] >|= unlabelled)
              ] >|= unlabelled)
          )
      in

      let nested =
        cs
        >|= fparam_of_cdecl
        >|= lambda
        |> compose_all
      in

      let cases =
        List.rev cs
        >|= vcase_of_cdecl
        |> compose_all
      in

      let pattern =
        cs
        >|= pcase_of_cdecl
        |> pexp_function
      in

      cases @@ pexp_apply (pexp_ident @@ Located.lident "variant")
        ([estring name; nested pattern] >|= unlabelled)
      |> sealv
end
