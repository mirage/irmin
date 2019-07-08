open Ppxlib

let name = "irmin"

module Utils = struct
  let unlabelled x = (Nolabel, x)
  let compose_all = List.fold_left (fun f g x -> f (g x)) (fun x -> x)
end

module type S = sig
  val derive: rec_flag * type_declaration list -> structure_item list
end

module Located (S: Ast_builder.S): S = struct
  open S
  open Utils

  let (>|=) x f = List.map f x

  let seal sealer e =
    pexp_apply
      (pexp_ident @@ Located.lident "|>")
      ([e; pexp_ident @@ Located.lident sealer] >|= unlabelled)

  let sealv = seal "sealv"
  let sealr = seal "sealr"

  let lambda fparam =
    Located.mk fparam
    |> ppat_var
    |> pexp_fun Nolabel None


  let rec derive input_ast =

    let open_type = pexp_open Fresh (Located.lident "Irmin.Type") in
    match input_ast with
    | (_, [typ]) -> (
        let name = typ.ptype_name.txt in
        let kind = typ.ptype_kind in

        let expr = match kind with
          | Ptype_abstract -> (
              match typ.ptype_manifest with
              | None -> invalid_arg "No manifest"
              | Some c -> (match c.ptyp_desc with

                  (* Basic type: unit, int, string etc. *)
                  | Ptyp_constr ({txt = Lident type_name; loc = _}, []) ->

                    (* TODO: don't string concatenate here *)
                    evar ("Irmin.Type." ^ type_name)

                  (* Type constructor: list, tuple, etc. *)
                  | _ -> derive_core c |> open_type
                ))

          | Ptype_variant cs -> derive_variant name cs |> open_type
          | Ptype_record ls -> derive_record ls |> open_type
          | Ptype_open -> invalid_arg "Open types unsupported" in

        [ pstr_value Nonrecursive [ value_binding ~pat:(ppat_var @@ Located.mk name) ~expr]]
      )

    | _ -> invalid_arg "Multiple type declarations not supported"

  and derive_core: core_type -> expression = fun typ ->
    match typ.ptyp_desc with
    | Ptyp_constr ({txt = Lident const_name; _}, args) ->
      args
      >|= derive_core
      >|= unlabelled
      |> pexp_apply (pexp_ident @@ Located.lident const_name)

    | Ptyp_tuple args -> derive_tuple args

    | Ptyp_arrow _ -> invalid_arg "Arrow types unsupported"
    | _ -> invalid_arg "Unsupported type"


  and derive_tuple: core_type list -> expression = fun args ->
    let tuple_type = match List.length args with
      | 2 -> "pair"
      | 3 -> "triple"
      | l -> invalid_arg (Printf.sprintf "Tuples must have 2 or 3 components. Found %d." l)
    in
    args
    >|= derive_core
    >|= unlabelled
    |> pexp_apply (pexp_ident @@ Located.lident tuple_type)

  and derive_record: label_declaration list -> expression = fun ls ->

    let rcase_of_ldecl l = fun e ->
      let label_name = l.pld_name.txt in

      (* |~ case0 "cons_name" Cons_name *)
      pexp_apply (pexp_ident @@ Located.lident "|+") ([
          e;
          pexp_apply (pexp_ident @@ Located.lident "field") ([
              pexp_constant @@ Pconst_string (label_name, None);
              derive_core l.pld_type;
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
      ([estring name; nested record] >|= unlabelled)
    |> sealr

    (* pexp_apply (pexp_ident @@ Located.lident "|>")
     *   [
     *     (Nolabel, cases @@ pexp_apply (pexp_ident @@ Located.lident "variant") [
     *         unlabelled @@ estring name;
     *         unlabelled @@ nested pattern
     *       ]);
     *     (Nolabel, pexp_ident @@ Located.lident "sealr")
     *   ] *)


  and derive_variant: string -> constructor_declaration list -> expression = fun name cs ->
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
                 | [t] -> derive_core t
                 | c   -> derive_tuple c);

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


    (* TODO: if the type is recursive, wrap everything in a mu (fun cons_name -> ... )*)
    cases @@ pexp_apply (pexp_ident @@ Located.lident "variant")
      ([estring name; nested pattern] >|= unlabelled)
    |> sealv


end

let expand_str ~loc ~path:_ (input_ast: rec_flag * type_declaration list) =
  let (module S) = Ast_builder.make loc in
  let (module L) = (module Located(S): S) in
  L.derive input_ast

let str_type_decl_generator =
  Deriving.Generator.make_noarg expand_str

let irmin =
  Deriving.add
    ~str_type_decl:str_type_decl_generator
    name

