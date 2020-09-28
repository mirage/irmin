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

let default_library = "Irmin.Type"

let library = ref (Some default_library)

let ppx_name = "irmin"

let with_engine f ~loc ~path:_ =
  let (module S) = Ast_builder.make loc in
  f (module Engine.Located (S) : Engine.S)

let deriving_args () =
  Deriving.Args.(empty +> arg "name" (estring __) +> arg "lib" __)

let str_type_decl =
  let attributes = Attributes.all in
  Deriving.Generator.make ~attributes (deriving_args ())
    ( with_engine @@ fun (module L) input_ast name lib ->
      let lib = match lib with Some s -> L.parse_lib s | None -> !library in
      L.derive_str ?name ?lib input_ast )

let sig_type_decl =
  Deriving.Generator.make (deriving_args ())
    ( with_engine @@ fun (module L) input_ast name lib ->
      let lib = match lib with Some s -> L.parse_lib s | None -> !library in
      L.derive_sig ?name ?lib input_ast )

let extension =
  Extension.declare (ppx_name ^ ".typ") Extension.Context.expression
    Ast_pattern.(ptyp __)
    (with_engine @@ fun (module L) -> L.expand_typ ?lib:!library)

let () =
  let doc =
    Format.sprintf
      "<module-path> Set the module path containing the combinators to use \
       (defaults to %s). An empty string is interpreted as the current module."
      default_library
  in
  Ppxlib.Driver.add_arg "--lib"
    (Arg.String (function "" -> library := None | s -> library := Some s))
    ~doc;

  Reserved_namespaces.reserve ppx_name;
  Deriving.add ~str_type_decl ~sig_type_decl ppx_name |> Deriving.ignore;
  Driver.register_transformation ~extensions:[ extension ] ppx_name
