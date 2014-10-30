(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Sexplib.Std

module type S = sig
  include Tc.I0
  type elt
  val hd: t -> elt option
  val tl: t -> t option
  val cons: elt -> t -> t
  val append: t -> t -> t
end

type t = string list

let hash = Hashtbl.hash
let equal = (=)
let compare = Pervasives.compare

let to_sexp t = Sexplib.Conv.(sexp_of_list sexp_of_string t)
let to_json t = Ezjsonm.(list encode_string t)
let of_json j = Ezjsonm.(get_list decode_string_exn j)


let implode t = "/" ^ (String.concat "/" t)
let explode str =  List.filter ((<>) "") (Stringext.split str ~on:'/')

let size_of t =
  (max 1 (List.length t))
  + List.fold_left (fun acc e -> String.length e + acc) 0 t

let write t buf =
  let str = implode t in
  let len = String.length str in
  Cstruct.blit_from_string str 0 buf 0 len;
  Cstruct.shift buf len

let read buf =
  let str = match Mstruct.get_string_delim buf '\000' with
    | None   -> Mstruct.get_string buf (Mstruct.length buf)
    | Some s -> s
  in
  explode str
