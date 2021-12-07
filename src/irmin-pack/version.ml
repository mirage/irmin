(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

(* For every new version, update the [version] type and [versions]
   headers. *)

type t = [ `V1 ] [@@deriving irmin]

let latest = `V1
let enum = [ (`V1, "00000001") ]
let pp = Fmt.of_to_string (function `V1 -> "v1")
let to_bin v = List.assoc v enum

let invalid_arg v =
  let pp_full_version ppf v = Fmt.pf ppf "%a (%S)" pp v (to_bin v) in
  Fmt.invalid_arg "invalid version: got %S, expecting %a" v
    Fmt.(Dump.list pp_full_version)
    (List.map fst enum)

let of_bin b =
  try Some (List.assoc b (List.map (fun (x, y) -> (y, x)) enum))
  with Not_found -> None

exception Invalid of { expected : t; found : t }

let () =
  Printexc.register_printer (function
    | Invalid { expected; found } ->
        Some
          (Fmt.str "%s.Invalid { expected = %a; found = %a }" __MODULE__ pp
             expected pp found)
    | _ -> None)
