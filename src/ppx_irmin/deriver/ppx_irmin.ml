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
open Ppx_irmin_lib

let ppx_name = "irmin"

let expand_str ~loc ~path:_ input_ast name lib =
  let (module S) = Ast_builder.make loc in
  let (module L) = (module Deriver.Located (S) : Deriver.S) in
  L.derive_str ?name ?lib input_ast

let expand_sig ~loc ~path:_ input_ast name lib =
  let (module S) = Ast_builder.make loc in
  let (module L) = (module Deriver.Located (S) : Deriver.S) in
  L.derive_sig ?name ?lib input_ast

let str_type_decl_generator =
  let args = Deriving.Args.(empty +> arg "name" (estring __) +> arg "lib" __) in
  let attributes = Attributes.all in
  Deriving.Generator.make ~attributes args expand_str

let sig_type_decl_generator =
  let args = Deriving.Args.(empty +> arg "name" (estring __) +> arg "lib" __) in
  Deriving.Generator.make args expand_sig

let irmin =
  Deriving.add ~str_type_decl:str_type_decl_generator
    ~sig_type_decl:sig_type_decl_generator ppx_name
