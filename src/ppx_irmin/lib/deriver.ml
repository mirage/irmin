(*
 * Copyright (c) 2019-2020 Craig Ferguson <me@craigfe.io>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Ppxlib
module SSet = Set.Make (String)

let irmin_types =
  SSet.of_list
    [
      "unit";
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
      "result";
    ]

module type S = sig
  val derive_str :
    ?name:string ->
    ?lib:expression ->
    rec_flag * type_declaration list ->
    structure_item list

  val derive_sig :
    ?name:string ->
    ?lib:expression ->
    rec_flag * type_declaration list ->
    signature_item list
end

module Located (A : Ast_builder.S) : S = struct
  module State = struct
    type t = {
      rec_flag : rec_flag;
      type_name : string;
      lib : string option;
      generic_name : string;
      rec_detected : bool ref;
    }
  end

  module Reader = Monad.Reader (State)

  let ( >>= ) x f = Reader.bind f x

  module Algebraic = Algebraic.Located (A) (Reader)
  open A
  open Reader.Syntax
  open Reader

  let unlabelled x = (Nolabel, x)

  let ( >|= ) x f = List.map f x

  let lambda fparam = pvar fparam |> pexp_fun Nolabel None

  let open_lib expr =
    let+ { lib; _ } = ask in
    match lib with
    | Some lib ->
        pexp_open
          {
            popen_expr = pmod_ident (Located.lident lib);
            popen_override = Fresh;
            popen_loc = A.loc;
            popen_attributes = [];
          }
          expr
    | None -> expr

  let recursive ~lib fparam e =
    let lib = match lib with Some s -> s | None -> "" in
    pexp_apply
      (evar (String.concat "." [ lib; "mu" ]))
      ([ lambda fparam e ] >|= unlabelled)

  let generic_name_of_type_name = function "t" -> "t" | x -> x ^ "_t"

  let rec derive_core typ =
    let* { rec_flag; type_name; generic_name; rec_detected; _ } = ask in
    match typ.ptyp_desc with
    | Ptyp_constr ({ txt = const_name; _ }, args) -> (
        match Attribute.get Attributes.generic typ with
        | Some e -> return e
        | None ->
            let lident =
              match const_name with
              | Lident const_name ->
                  let name =
                    (* If this type is the one we are deriving and the 'nonrec'
                       keyword hasn't been used, replace with the generic
                       name *)
                    if
                      rec_flag <> Nonrecursive
                      && String.equal const_name type_name
                    then (
                      rec_detected := true;
                      generic_name
                      (* If not a base type, assume a composite generic with the
                         same naming convention *) )
                    else
                      let nobuiltin =
                        match Attribute.get Attributes.nobuiltin typ with
                        | Some () -> true
                        | None -> false
                      in
                      if nobuiltin || not (SSet.mem const_name irmin_types) then
                        generic_name_of_type_name const_name
                      else const_name
                  in
                  Located.lident name
              | Ldot (lident, name) ->
                  let name = generic_name_of_type_name name in
                  Located.mk @@ Ldot (lident, name)
              | Lapply _ -> invalid_arg "Lident.Lapply not supported"
            in
            let+ cons_args =
              args >|= derive_core |> sequence |> map (List.map unlabelled)
            in
            pexp_apply (pexp_ident lident) cons_args )
    | Ptyp_variant (_, Open, _) -> Raise.Unsupported.type_open_polyvar ~loc typ
    | Ptyp_variant (rowfields, Closed, _labellist) ->
        derive_polyvariant type_name rowfields
    | Ptyp_poly _ -> Raise.Unsupported.type_poly ~loc typ
    | Ptyp_tuple args -> derive_tuple args
    | Ptyp_arrow _ -> Raise.Unsupported.type_arrow ~loc typ
    | Ptyp_var v -> Raise.Unsupported.type_var ~loc v
    | Ptyp_package _ -> Raise.Unsupported.type_package ~loc typ
    | Ptyp_extension _ -> Raise.Unsupported.type_extension ~loc typ
    | Ptyp_alias _ -> Raise.Unsupported.type_alias ~loc typ
    | _ -> invalid_arg "unsupported"

  and derive_tuple args =
    match args with
    | [ t ] ->
        (* This case can occur when the tuple type is nested inside a variant *)
        derive_core t
    | _ ->
        let tuple_type =
          match List.length args with
          | 2 -> "pair"
          | 3 -> "triple"
          | n -> Raise.Unsupported.tuple_size ~loc n
        in
        args
        >|= derive_core
        |> sequence
        |> map (List.map unlabelled)
        |> map (pexp_apply (evar tuple_type))

  and derive_record ls =
    let* State.{ type_name; _ } = ask in
    let subderive label_decl =
      let field_name = label_decl.pld_name.txt in
      let+ field_generic = derive_core label_decl.pld_type in
      Algebraic.{ field_name; field_generic }
    in
    Algebraic.(encode Record) ~subderive ~type_name ls

  and derive_variant cs =
    let* { type_name; _ } = ask in
    let subderive c =
      let case_name = c.pcd_name.txt in
      let+ case_cons =
        match c.pcd_args with
        | Pcstr_record _ -> invalid_arg "Inline record types unsupported"
        | Pcstr_tuple [] -> return None
        | Pcstr_tuple cs ->
            let+ tuple_typ = derive_tuple cs in
            Some (tuple_typ, List.length cs)
      in
      Algebraic.{ case_name; case_cons }
    in
    Algebraic.(encode Variant) ~subderive ~type_name cs

  and derive_polyvariant name rowfields =
    let subderive f =
      let+ case_name, case_cons =
        match f.prf_desc with
        | Rtag (label, _, []) -> return (label.txt, None)
        | Rtag (label, _, typs) ->
            let+ tuple_typ = derive_tuple typs in
            (label.txt, Some (tuple_typ, List.length typs))
        | Rinherit _ -> assert false
      in
      Algebraic.{ case_name; case_cons }
    in
    Algebraic.(encode Polyvariant) ~subderive ~type_name:name rowfields

  let derive_lident :
      ?generic:expression -> ?nobuiltin:unit -> longident -> expression Reader.t
      =
   fun ?generic ?nobuiltin txt ->
    let+ { lib; _ } = ask in
    let nobuiltin = match nobuiltin with Some () -> true | None -> false in
    match generic with
    | Some e -> e
    | None -> (
        match txt with
        | Lident cons_name ->
            if (not nobuiltin) && SSet.mem cons_name irmin_types then
              match lib with
              | Some lib -> evar (String.concat "." [ lib; cons_name ])
              | None -> evar cons_name
            else
              (* If not a basic type, assume a composite
                 generic /w same naming convention *)
              evar (generic_name_of_type_name cons_name)
        | Ldot (lident, cons_name) ->
            pexp_ident
              (Located.mk @@ Ldot (lident, generic_name_of_type_name cons_name))
        | Lapply _ -> invalid_arg "Lident.Lapply not supported" )

  let derive_type_decl : type_declaration -> expression Reader.t =
   fun typ ->
    match typ.ptype_kind with
    | Ptype_abstract -> (
        match typ.ptype_manifest with
        | None -> invalid_arg "No manifest"
        | Some c -> (
            match c.ptyp_desc with
            (* No need to open library module *)
            | Ptyp_constr ({ txt; loc = _ }, []) ->
                let generic = Attribute.get Attributes.generic c
                and nobuiltin = Attribute.get Attributes.nobuiltin c in
                derive_lident ?generic ?nobuiltin txt
            (* Type constructor: list, tuple, etc. *)
            | _ -> derive_core c >>= open_lib ) )
    | Ptype_variant cs -> derive_variant cs >>= open_lib
    | Ptype_record ls -> derive_record ls >>= open_lib
    | Ptype_open -> Raise.Unsupported.type_open ~loc

  let parse_lib expr =
    match expr with
    | { pexp_desc = Pexp_construct ({ txt = Lident "None"; _ }, None); _ } ->
        None
    | {
     pexp_desc =
       Pexp_construct
         ( { txt = Lident "Some"; _ },
           Some { pexp_desc = Pexp_constant (Pconst_string (lib, None)); _ } );
     _;
    } ->
        Some lib
    | { pexp_loc = loc; _ } ->
        Location.raise_errorf ~loc
          "Could not process `lib' argument: must be either `Some \"Lib\"' or \
           `None'"

  let lib_default = "Irmin.Type"

  let derive_sig ?name ?lib input_ast =
    match input_ast with
    | _, [ typ ] ->
        let type_name = typ.ptype_name.txt in
        let name =
          Located.mk
            ( match name with
            | Some n -> n
            | None -> generic_name_of_type_name type_name )
        in
        let lib =
          match lib with Some l -> parse_lib l | None -> Some lib_default
        in
        let ty_lident =
          match lib with
          | Some l -> Located.lident (String.concat "." [ l; "t" ])
          | None -> (
              (* This type decl may shadow the repr type ['a t] *)
              match name.txt with
              | "t" -> Located.lident "ty"
              | _ -> Located.lident "t" )
        in
        let type_ =
          ptyp_constr ty_lident [ ptyp_constr (Located.lident type_name) [] ]
        in
        [ psig_value (value_description ~name ~type_ ~prim:[]) ]
    | _ -> invalid_arg "Multiple type declarations not supported"

  let derive_str ?name ?lib input_ast =
    match input_ast with
    | rec_flag, [ typ ] ->
        let env =
          let type_name = typ.ptype_name.txt in
          let generic_name =
            match name with
            | Some s -> s
            | None -> generic_name_of_type_name type_name
          in
          let rec_detected = ref false in
          let lib =
            match lib with Some l -> parse_lib l | None -> Some lib_default
          in
          State.{ rec_flag; type_name; generic_name; rec_detected; lib }
        in
        let expr = run (derive_type_decl typ) env in
        (* If the type is syntactically self-referential and the user has not
           asserted 'nonrec' in the type declaration, wrap in a 'mu'
           combinator *)
        let expr =
          if !(env.rec_detected) && rec_flag == Recursive then
            recursive ~lib:env.lib env.generic_name expr
          else expr
        in
        let pat = pvar env.generic_name in
        [ pstr_value Nonrecursive [ value_binding ~pat ~expr ] ]
    | _ -> invalid_arg "Multiple type declarations not supported"
end
