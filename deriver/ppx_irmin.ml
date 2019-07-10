open Ppxlib

let ppx_name = "irmin"

let expand_str ~loc ~path:_ input_ast name =
  let (module S) = Ast_builder.make loc in
  let (module L) = (module Ppx_irmin_lib.Deriver.Located(S): Ppx_irmin_lib.Deriver.S) in
  L.derive_str ?name input_ast

let expand_sig ~loc ~path:_ input_ast =
  let (module S) = Ast_builder.make loc in
  let (module L) = (module Ppx_irmin_lib.Deriver.Located(S): Ppx_irmin_lib.Deriver.S) in
  L.derive_sig input_ast

let str_type_decl_generator =
  let args = Deriving.Args.(empty +> arg "name" (estring __)) in
  let attributes = Attribute.[T Ppx_irmin_lib.Deriver.attribute_witness] in
  Deriving.Generator.make ~attributes args expand_str

let sig_typ_decl_generator =
  Deriving.Generator.make_noarg expand_sig

let irmin =
  Deriving.add
    ~str_type_decl:str_type_decl_generator
    ~sig_type_decl:sig_typ_decl_generator
    ppx_name
