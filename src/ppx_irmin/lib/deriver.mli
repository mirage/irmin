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
  val derive_str :
    ?name:string ->
    ?lib:expression ->
    rec_flag * type_declaration list ->
    structure_item list
  (** Deriver for Irmin generics.

      - [?name]: overrides the default name of the generated generic;

      - [?lib]: overrides the default location for the primitive Irmin generics.
        [~lib:None] will assume that the generics are available in the same
        namespace. *)

  val derive_sig :
    ?name:string ->
    ?lib:expression ->
    rec_flag * type_declaration list ->
    signature_item list
  (** Deriver for Irmin generic type signatures.

      Optional arguments have the same meaning as in {!derive_str}. *)
end

module Located (A : Ast_builder.S) : S
