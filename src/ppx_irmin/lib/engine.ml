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
  val parse_lib : expression -> string option

  val expand_typ : ?lib:string -> core_type -> expression

  val derive_str :
    ?name:string ->
    ?lib:string ->
    rec_flag * type_declaration list ->
    structure_item list

  val derive_sig :
    ?name:string ->
    ?lib:string ->
    rec_flag * type_declaration list ->
    signature_item list
end

module Located (A : Ast_builder.S) : S = struct
  type state = {
    rec_flag : rec_flag;
    type_name : string;
    lib : string option;
    repr_name : string;
    rec_detected : bool ref;
    var_repr : [ `Any | `Var of string ] -> expression option;
        (** Given a type variable in a type, get its corresponding typerep (if
            the variable is properly bound). *)
  }

  open Utils

  open Utils.Make (A)

  module Reader = Monad.Reader

  module Algebraic = struct
    include Algebraic
    include Algebraic.Located (A) (Reader)
  end

  open A
  open Reader.Syntax
  open Reader

  let all_unlabelled = List.map (fun x -> (Nolabel, x))

  let recursive ~lib fparam e =
    let mu = evar (match lib with Some s -> s ^ ".mu" | None -> "mu") in
    [%expr [%e mu] (fun [%p pvar fparam] -> [%e e])]

  let repr_name_of_type_name = function "t" -> "t" | x -> x ^ "_t"

  let in_lib ~lib x = match lib with Some lib -> lib ^ "." ^ x | None -> x

  let rec derive_core typ =
    let* { rec_flag; type_name; repr_name; rec_detected; lib; var_repr } =
      ask
    in
    let loc = typ.ptyp_loc in
    match typ.ptyp_desc with
    | Ptyp_constr ({ txt = const_name; _ }, args) -> (
        match Attribute.get Attributes.repr typ with
        | Some e -> return e
        | None ->
            let lident =
              match const_name with
              | Lident const_name ->
                  let name =
                    (* If this type is the one we are deriving and the 'nonrec'
                       keyword hasn't been used, replace with the repr
                       name *)
                    if
                      rec_flag <> Nonrecursive
                      && String.equal const_name type_name
                    then (
                      rec_detected := true;
                      repr_name
                      (* If not a base type, assume a composite repr with the
                         same naming convention *))
                    else
                      let nobuiltin =
                        match Attribute.get Attributes.nobuiltin typ with
                        | Some () -> true
                        | None -> false
                      in
                      if nobuiltin || not (SSet.mem const_name irmin_types) then
                        repr_name_of_type_name const_name
                      else in_lib ~lib const_name
                  in
                  Located.lident name
              | Ldot (lident, name) ->
                  let name = repr_name_of_type_name name in
                  Located.mk @@ Ldot (lident, name)
              | Lapply _ -> invalid_arg "Lident.Lapply not supported"
            in
            let+ cons_args =
              args >|= derive_core |> sequence |> map all_unlabelled
            in
            pexp_apply (pexp_ident lident) cons_args)
    | Ptyp_variant (_, Open, _) -> Raise.Unsupported.type_open_polyvar ~loc typ
    | Ptyp_variant (rowfields, Closed, _labellist) ->
        derive_polyvariant type_name rowfields
    | Ptyp_poly _ -> Raise.Unsupported.type_poly ~loc typ
    | Ptyp_tuple args -> derive_tuple args
    | Ptyp_arrow _ -> Raise.Unsupported.type_arrow ~loc typ
    | Ptyp_var v -> (
        match var_repr (`Var v) with
        | Some r -> return r
        | None -> Location.raise_errorf ~loc "Unbound type variable" v)
    | Ptyp_package _ -> Raise.Unsupported.type_package ~loc typ
    | Ptyp_extension _ -> Raise.Unsupported.type_extension ~loc typ
    | Ptyp_alias _ -> Raise.Unsupported.type_alias ~loc typ
    | _ -> invalid_arg "unsupported"

  and derive_tuple args =
    let* { lib; _ } = ask in
    match args with
    | [ t ] ->
        (* This case can occur when the tuple type is nested inside a variant *)
        derive_core t
    | _ ->
        let tuple_type =
          (match List.length args with
          | 2 -> "pair"
          | 3 -> "triple"
          | n -> Raise.Unsupported.tuple_size ~loc n)
          |> in_lib ~lib
          |> evar
        in
        args
        >|= derive_core
        |> sequence
        |> map (all_unlabelled >> pexp_apply tuple_type)

  and derive_record ls =
    let* { type_name; lib; _ } = ask in
    let subderive label_decl =
      let field_name = label_decl.pld_name.txt in
      let+ field_repr = derive_core label_decl.pld_type in
      Algebraic.Typ.{ field_name; field_repr }
    in
    Algebraic.(encode Typ.Record) ~subderive ~lib ~type_name ls

  and derive_variant cs =
    let* { type_name; lib; _ } = ask in
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
      Algebraic.Typ.{ case_name; case_cons }
    in
    Algebraic.(encode Variant) ~subderive ~lib ~type_name cs

  and derive_polyvariant name rowfields =
    let* { lib; _ } = ask in
    let subderive f =
      let+ case_name, case_cons =
        match f.prf_desc with
        | Rtag (label, _, []) -> return (label.txt, None)
        | Rtag (label, _, typs) ->
            let+ tuple_typ = derive_tuple typs in
            (label.txt, Some (tuple_typ, List.length typs))
        | Rinherit _ -> assert false
      in
      Algebraic.Typ.{ case_name; case_cons }
    in
    Algebraic.(encode Polyvariant) ~subderive ~lib ~type_name:name rowfields

  let derive_lident :
      ?repr:expression ->
      ?nobuiltin:unit ->
      longident ->
      (expression, state) Reader.t =
   fun ?repr ?nobuiltin txt ->
    let+ { lib; _ } = ask in
    let nobuiltin = match nobuiltin with Some () -> true | None -> false in
    match repr with
    | Some e -> e
    | None -> (
        match txt with
        | Lident cons_name ->
            if (not nobuiltin) && SSet.mem cons_name irmin_types then
              evar (in_lib ~lib cons_name)
            else
              (* If not a basic type, assume a composite
                 repr /w same naming convention *)
              evar (repr_name_of_type_name cons_name)
        | Ldot (lident, cons_name) ->
            pexp_ident
              (Located.mk @@ Ldot (lident, repr_name_of_type_name cons_name))
        | Lapply _ -> invalid_arg "Lident.Lapply not supported")

  let derive_type_decl : type_declaration -> (expression, state) Reader.t =
   fun typ ->
    match typ.ptype_kind with
    | Ptype_abstract -> (
        match typ.ptype_manifest with
        | None -> invalid_arg "No manifest"
        | Some c -> (
            match c.ptyp_desc with
            (* No need to open library module *)
            | Ptyp_constr ({ txt; loc = _ }, []) ->
                let repr = Attribute.get Attributes.repr c
                and nobuiltin = Attribute.get Attributes.nobuiltin c in
                derive_lident ?repr ?nobuiltin txt
            (* Type constructor: list, tuple, etc. *)
            | _ -> derive_core c))
    | Ptype_variant cs -> derive_variant cs
    | Ptype_record ls -> derive_record ls
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

  (* Remove duplicate elements from a list (preserving the order of the first
     occurrence of each duplicate). *)
  let list_uniq_stable =
    let rec inner ~seen acc = function
      | [] -> List.rev acc
      | x :: xs when not (List.mem x seen) ->
          inner ~seen:(x :: seen) (x :: acc) xs
      | _ :: xs (* seen *) -> inner ~seen acc xs
    in
    inner ~seen:[] []

  let expand_typ ?lib typ =
    let typ, tvars =
      (* Find all type variables, renaming any instances of [Ptyp_any] to a
         fresh variable. *)
      (object
         inherit [string list] Ast_traverse.fold_map as super

         method! core_type_desc t =
           super#core_type_desc t >> fun (t, acc) ->
           match t with
           | Ptyp_var v -> (t, v :: acc)
           | Ptyp_any ->
               let name = gen_symbol () in
               (Ptyp_var name, name :: acc)
           | _ -> (t, acc)
      end)
        #core_type
        typ []
    in
    let tvars = List.rev tvars |> list_uniq_stable in
    let env =
      {
        rec_flag = Nonrecursive;
        type_name = "t";
        repr_name = "t";
        rec_detected = ref false;
        lib;
        var_repr =
          (function
          | `Any ->
              assert false (* We already renamed all instances of [Ptyp_any] *)
          | `Var x -> Some (evar x));
      }
    in
    run (derive_core typ) env |> lambda tvars

  let derive_sig ?name ?lib input_ast =
    match input_ast with
    | _, [ typ ] ->
        let type_name = typ.ptype_name.txt in
        let name =
          Located.mk
            (match name with
            | Some n -> n
            | None -> repr_name_of_type_name type_name)
        in
        let ty_lident =
          (match lib with
          | Some _ -> in_lib ~lib "t"
          | None -> (
              (* This type decl may shadow the repr type ['a t] *)
              match name.txt with
              | "t" -> "ty"
              | _ -> "t"))
          |> Located.lident
        in
        let type_ =
          combinator_type_of_type_declaration typ ~f:(fun ~loc:_ t ->
              ptyp_constr ty_lident [ t ])
        in
        [ psig_value (value_description ~name ~type_ ~prim:[]) ]
    | _ -> invalid_arg "Multiple type declarations not supported"

  let derive_str ?name ?lib input_ast =
    match input_ast with
    | rec_flag, [ typ ] ->
        let tparams =
          typ.ptype_params
          |> List.map (function
               | { ptyp_desc = Ptyp_var v; _ }, _ -> v
               | { ptyp_desc = Ptyp_any; _ }, _ -> "_"
               | _ -> assert false)
        in
        let env =
          let type_name = typ.ptype_name.txt in
          let repr_name =
            match name with
            | Some s -> s
            | None -> repr_name_of_type_name type_name
          in
          let rec_detected = ref false in
          let var_repr = function
            | `Any -> Raise.Unsupported.type_any ~loc
            | `Var v -> if List.mem v tparams then Some (evar v) else None
          in
          { rec_flag; type_name; repr_name; rec_detected; lib; var_repr }
        in
        let expr = run (derive_type_decl typ) env in
        (* If the type is syntactically self-referential and the user has not
           asserted 'nonrec' in the type declaration, wrap in a 'mu'
           combinator *)
        let expr =
          if !(env.rec_detected) && rec_flag == Recursive then
            recursive ~lib:env.lib env.repr_name expr
          else expr
        in
        let expr = lambda tparams expr in
        let pat = pvar env.repr_name in
        [ pstr_value Nonrecursive [ value_binding ~pat ~expr ] ]
    | _ -> invalid_arg "Multiple type declarations not supported"
end
