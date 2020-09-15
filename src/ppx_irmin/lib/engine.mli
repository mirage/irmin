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

module type S = sig
  val parse_lib : expression -> string option
  (** [parse_lib e] is [Some "foo"] or [None] if [e] is a [string option], and
      raises a located exception otherwise. Intended to be used for parsing the
      [lib] argument to the derivers. *)

  val expand_typ : ?lib:string -> core_type -> expression

  val derive_str :
    ?name:string ->
    ?lib:string ->
    rec_flag * type_declaration list ->
    structure_item list
  (** Deriver for Irmin type representations.

      - [?name]: overrides the default name of the generated type
        representation;

      - [?lib]: overrides the default location for the primitive Irmin typereps.
        [~lib:None] will assume that the typereps are available in the same
        namespace. *)

  val derive_sig :
    ?name:string ->
    ?lib:string ->
    rec_flag * type_declaration list ->
    signature_item list
  (** Deriver for Irmin type representation signatures.

      Optional arguments have the same meaning as in {!derive_str}. *)
end

module Located (A : Ast_builder.S) : S
