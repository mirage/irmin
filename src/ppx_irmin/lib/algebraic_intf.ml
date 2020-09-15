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

module Typ = struct
  type nonrec record_field_repr = {
    field_name : string;
    field_repr : expression;
  }

  and variant_case_repr = {
    case_name : string;
    case_cons : (expression * int) option;
  }

  (** The algebraic datatypes supported by this module, parameterised by:

      - ['a]: the subcomponent type of the algebraic type
      - ['b]: a generic representation of the subcomponent type necessary to
        derive the {i composite} type representation *)
  type (_, _) t =
    | Record : (label_declaration, record_field_repr) t
    | Variant : (constructor_declaration, variant_case_repr) t
    | Polyvariant : (row_field, variant_case_repr) t
end

module type S = sig
  module M : Monad.S

  val encode :
    ('a, 'b) Typ.t ->
    subderive:('a -> ('b, 'e) M.t) ->
    lib:string option ->
    type_name:string ->
    'a list ->
    (expression, 'e) M.t
  (** Build the functional encoding of a composite type. Combine the various
      elements necessary for a functional encoding of a composite type
      [('a, 'b) {!typ}], in terms its components of type ['a list] and the name
      of the composite type [type_name].

      This requires a function [subderive] for deriving the type representation
      of the subcomponents, which may run in a monadic context [M.t]. *)
end

module type Algebraic = sig
  module Typ = Typ

  module type S = S

  module Located (S : Ast_builder.S) (M : Monad.S) : S with module M = M
end
