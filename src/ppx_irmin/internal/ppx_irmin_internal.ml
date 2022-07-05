(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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

let rewriter_name = "ppx_irmin.internal"

(* Provides a PPX wrapper around the Logs library that attaches source code
   postitions to log lines via Logs' tags system.

   Input: [%log(s?).<level> <fmt_string> <args...>]
   Output: Log(s?).(<level>) (fun f -> f <fmt_string> <args...> ~tags:(...))

   (The extension node payload can also be written in the standard CPSed form,
   for instance in order to perform computation before constructing the log
   line.)
*)

module Source = struct
  type t =
    | Logs  (** default (source-less) logging functions *)
    | Log  (** referencing a "Log" module, specifying a particular source *)

  let to_string = function Logs -> "logs" | Log -> "log"
end

let level_to_function_name : Logs.level -> string = function
  | App -> "app"
  | Error -> "err"
  | Warning -> "warn"
  | Info -> "info"
  | Debug -> "debug"

let log_function ~loc (source : Source.t) (level : Logs.level) =
  let prefix = match source with Logs -> "Logs." | Log -> "Log." in
  Ast_builder.Default.evar ~loc (prefix ^ level_to_function_name level)

let tags ~loc =
  [%expr
    Logs.Tag.add Ppx_irmin_internal_lib.Source_code_position.tag __POS__
      Logs.Tag.empty]

let expansion_function source level ~loc ~path:_ payload =
  let log_fn = log_function ~loc source level in
  let open Ast_builder.Default in
  match payload with
  | [%expr fun [%p? _] -> [%e? _]] ->
      (* Payload is already in CPS-ed form: we just need to attach the tags. *)
      [%expr
        [%e log_fn] (fun f ->
            ([%e payload] : (?header:string -> (_, _, _, _) format4 -> _) -> _)
              (f ~tags:[%e tags ~loc]))]
  | _ ->
      (* The user hasn't wrapped the payload in [fun f -> ...; f ...], so we
         should attempt to do so. This requires re-interpreting top-level
         [Pexp_apply] nodes in the AST, for example:

         > [%log.debug "fmt_string" ...args]

         This parses ["fmt_string"] as a _function_, but it's going to become
         the first argument of a function [debug]. *)
      let input_args =
        match payload with
        | { pexp_desc = Pexp_constant (Pconst_string _); _ } ->
            [ (Nolabel, payload) ]
        (* Special case for ( @@ ), e.g. [%log.err "%d" @@ 1 + 2] *)
        | [%expr [%e? fmt] @@ [%e? args]] -> [ (Nolabel, fmt); (Nolabel, args) ]
        | { pexp_desc = Pexp_apply (fmt, args); _ } -> (Nolabel, fmt) :: args
        | _ -> Location.raise_errorf ~loc "%s: invalid payload" rewriter_name
      in
      let args = input_args @ [ (Labelled "tags", tags ~loc) ] in
      [%expr [%e log_fn] (fun f -> [%e pexp_apply ~loc [%expr f] args])]

let ( let* ) x f = List.concat_map f x

let rules =
  let* source = [ Source.Logs; Log ] in
  let* level = [ Logs.App; Error; Warning; Info; Debug ] in
  let extension_name =
    Format.sprintf "irmin.%s.%s" (Source.to_string source)
      (level_to_function_name level)
  in
  [
    Extension.declare extension_name Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      (expansion_function source level)
    |> Context_free.Rule.extension;
  ]

let () = Driver.register_transformation ~rules rewriter_name
