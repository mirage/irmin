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

module type S = Tc.I0

module Path (S: S) = struct

  type t = S.t list

  let hash = Hashtbl.hash
  let equal = (=)

  let rec compare x y = match x, y with
    | [], [] -> 0
    | _::_, [] -> 1
    | [], _::_ -> -1
    | a::b, c::d ->
      match S.compare a c with
      | 0 -> compare b d
      | i -> i

  let to_sexp t = Sexplib.Conv.(sexp_of_list S.to_sexp t)
  let to_json t = Ezjsonm.(list S.to_json t)
  let of_json j = Ezjsonm.(get_list S.of_json j)

  let size_of t =
    (max 1 (List.length t))
    + List.fold_left (fun acc e -> S.size_of e + acc) 0 t

  let write t buf =
    List.fold_left (fun buf step ->
        Cstruct.set_char buf 0 '/';
        let buf = Cstruct.shift buf 1 in
        S.write step buf
      ) buf t

  let read buf =
    let rec aux acc =
      if Mstruct.length buf = 0 then List.rev acc
      else
        let str = match Mstruct.get_string_delim buf '/' with
          | None   -> Mstruct.get_string buf (Mstruct.length buf)
          | Some s -> s
        in
        aux (Tc.read_string (module S) str :: acc)
    in
    match Mstruct.get_char buf with
    | '/' -> aux []
    | c   -> failwith ("Step.read: the step starts by" ^ String.make 1 c)

end

module String = struct

  type t = string
  let hash = Hashtbl.hash
  let equal = (=)
  let compare = String.compare
  let to_sexp t = Sexplib.Type.Atom t
  let to_json t = Ezjsonm.encode_string t
  let of_json j = Ezjsonm.decode_string_exn j
  let size_of t = String.length t

  let write t buf =
    let len = String.length t in
    Cstruct.blit_from_string t 0 buf 0 len;
    Cstruct.shift buf len

  let read buf =
    Mstruct.get_string buf (Mstruct.length buf)

end
