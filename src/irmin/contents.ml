(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

module String = struct
  type t = string
  let t = Type.string
  let merge = Merge.idempotent Type.(option string)
  let pp = Fmt.string
  let of_string s = Ok s
end

module Cstruct = struct
  type t = Cstruct.t
  let t = Type.cstruct
  let merge = Merge.idempotent Type.(option t)
  let pp ppf b = Fmt.string ppf (Cstruct.to_string b)
  let of_string s = Ok (Cstruct.of_string s)
end

type json = [
  | `Null
  | `String of string
  | `Float of float
  | `O of (string * json) list
  | `A of json list
]

module Json = struct
  type t = json

  let t =
    let open Type in
    mu (fun ty ->
    variant "json" (fun null string float obj arr -> function
      | `Null -> null
      | `String s -> string s
      | `Float f -> float f
      | `O o -> obj o
      | `A a -> arr a)
    |~ case0 "null" `Null
    |~ case1 "string" string (fun x -> `String x)
    |~ case1 "float" float (fun x -> `Float x)
    |~ case1 "object" (list (pair string ty)) (fun obj -> `O obj)
    |~ case1 "array" (list ty) (fun arr -> `A arr)
    |> sealv)

  let merge = Merge.idempotent Type.(option t)
  let pp = Type.pp_json t
  let of_string s = Type.decode_json t (Jsonm.decoder (`String s))
end

module Store
    (S: sig
       include S.AO
       module Key: S.HASH with type t = key
       module Val: S.CONTENTS with type t = value
     end) =
struct
  include S

  let read_opt t = function
    | None   -> Lwt.return_none
    | Some k -> find t k

  let add_opt t = function
    | None -> Lwt.return_none
    | Some v -> add t v >>= fun k -> Lwt.return (Some k)

  let merge t =
    Merge.like_lwt Type.(option Key.t) Val.merge (read_opt t) (add_opt t)

end
