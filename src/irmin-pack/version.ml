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

type t = [ `V1 | `V2 | `V3 ] [@@deriving irmin]

let latest = `V3
let enum = [ (`V1, "00000001"); (`V2, "00000002"); (`V3, "00000003") ]
let pp = Fmt.of_to_string (function `V1 -> "v1" | `V2 -> "v2" | `V3 -> "v3")
let to_bin v = List.assoc v enum
let to_int = function `V1 -> 1 | `V2 -> 2 | `V3 -> 3
let compare a b = Int.compare (to_int a) (to_int b)
let encode_bin t f = to_bin t |> f

let decode_bin s offref =
  let sub = String.sub s !offref (!offref + 8) in
  let res =
    match sub with
    | "00000001" -> `V1
    | "00000002" -> `V2
    | "00000003" -> `V3
    | _ -> failwith "Couldn't decode pack version"
  in
  offref := !offref + 8;
  res

let size_of = Irmin.Type.Size.custom_static 8
let bin = (encode_bin, decode_bin, size_of)
let t = Irmin.Type.like ~bin ~compare ~pp t

let invalid_arg v =
  let pp_full_version ppf v = Fmt.pf ppf "%a (%S)" pp v (to_bin v) in
  Fmt.invalid_arg "invalid version: got %S, expecting %a" v
    Fmt.(Dump.list pp_full_version)
    (List.map fst enum)

let of_bin b = try Some (decode_bin b (ref 0)) with Failure _ -> None

exception Invalid of { expected : t; found : t }

let () =
  Printexc.register_printer (function
    | Invalid { expected; found } ->
        Some
          (Fmt.str "%s.Invalid { expected = %a; found = %a }" __MODULE__ pp
             expected pp found)
    | _ -> None)
