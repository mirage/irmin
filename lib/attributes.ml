open Ppxlib

let namespace = "ppx_irmin"

let witness =
  Attribute.declare
    (String.concat "." [ namespace; "witness" ])
    Attribute.Context.Core_type
    Ast_pattern.(single_expr_payload __)
    (fun e -> e)

let all = Attribute.[ T witness ]
