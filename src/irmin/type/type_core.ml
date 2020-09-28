(*
 * Copyright (c) 2016-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

include Type_core_intf
include Type_core_intf.Types
open Staging
open Brands

module Json = struct
  type decoder = json_decoder

  let decoder ?encoding src = { lexemes = []; d = Jsonm.decoder ?encoding src }

  let decoder_of_lexemes lexemes = { lexemes; d = Jsonm.decoder (`String "") }

  let rewind e l = e.lexemes <- l :: e.lexemes

  let decode e =
    match e.lexemes with
    | h :: t ->
        e.lexemes <- t;
        `Lexeme h
    | [] -> Jsonm.decode e.d
end

let undefined _ = assert false

let partial ?(pp = undefined) ?(of_string = undefined)
    ?(encode_json = undefined) ?(decode_json = undefined)
    ?(short_hash = fun ?seed:_ -> undefined) ?(pre_hash = stage undefined)
    ?(compare = undefined) ?(equal = undefined) ?(encode_bin = stage undefined)
    ?(decode_bin = stage undefined) ?(size_of = stage undefined)
    ?(unboxed_encode_bin = stage undefined)
    ?(unboxed_decode_bin = stage undefined) ?(unboxed_size_of = stage undefined)
    () =
  Custom
    {
      cwit = `Witness (Witness.make ());
      pp;
      of_string;
      encode_json;
      decode_json;
      short_hash;
      pre_hash;
      compare;
      equal;
      encode_bin;
      decode_bin;
      size_of;
      unboxed_encode_bin;
      unboxed_decode_bin;
      unboxed_size_of;
    }

let rec fields_aux : type a b. (a, b) fields -> a a_field list = function
  | F0 -> []
  | F1 (h, t) -> Field h :: fields_aux t

let fields r = match r.rfields with Fields (f, _) -> fields_aux f

module Dispatch = struct
  type 'a t =
    | Base : 'a staged -> 'a t
    | Arrow : { arg_wit : 'b Witness.t; f : ('b -> 'a) staged } -> 'a t
end

let rec fold_fields :
    type a c acc. (a, acc) Fields_folder.t -> (a, c) fields -> (a, c, acc) app2
    =
 fun folder -> function
  | F0 -> folder.nil
  | F1 (f, fs) -> folder.cons f (fold_fields folder fs)

let fold_variant :
    type a f. (a, f) Case_folder.t -> a variant -> (a -> f) staged =
 fun folder v_typ ->
  let cases =
    Array.map
      (function
        | C0 c0 -> Dispatch.Base (folder.c0 c0)
        | C1 c1 -> Dispatch.Arrow { arg_wit = c1.cwit1; f = folder.c1 c1 })
      v_typ.vcases
  in
  stage (fun v ->
      match v_typ.vget v with
      | CV0 { ctag0; _ } -> (
          match cases.(ctag0) with
          | Dispatch.Base x -> unstage x
          | _ -> assert false)
      | CV1 ({ ctag1; cwit1; _ }, v) -> (
          match cases.(ctag1) with
          | Dispatch.Arrow { f; arg_wit } -> (
              match Witness.cast cwit1 arg_wit v with
              | Some v -> unstage f v
              | None -> assert false)
          | _ -> assert false))
