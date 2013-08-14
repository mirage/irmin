(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open IrminTypes

module OCamlList = List

module List  (C: CHANNEL) (E: IO with type channel = C.t) = struct

  type t = E.t list

  type channel = C.t

  let pretty t =
    String.concat "\n" (OCamlList.rev (OCamlList.rev_map E.pretty t))

  let to_json t =
    `A (OCamlList.rev (OCamlList.rev_map E.to_json t))

  let of_json = function
    | `A l -> OCamlList.rev (List.rev_map E.of_json l)
    | _    -> failwith "MakeList.of_json"

  cstruct hdr {
      uint32_t keys
    } as big_endian

  let read fd =
    lwt buf = C.read fd sizeof_hdr in
    let keys = Int32.to_int (get_hdr_keys buf) in
    let rec aux acc i =
      if i <= 0 then Lwt.return (OCamlList.rev acc)
      else
        lwt t = E.read fd in
        aux (t :: acc) (i-1) in
    aux [] keys

  let write fd t =
    Lwt_list.iter_s (E.write fd) t

end

module Pair (C: CHANNEL)
    (K: IO with type channel = C.t) (V: IO with type channel = C.t)
= struct

  type t = K.t * V.t

  type channel = C.t

  let pretty (key, value) =
    Printf.sprintf "%s:%s" (K.pretty key) (V.pretty value)

  let to_json (key, value) =
    `O [ ("tag", K.to_json key);
         ("key", V.to_json value)]

  let of_json = function
    | `O l ->
      let key =
        try OCamlList.assoc "tag" l
        with Not_found -> failwith "MakeProduct.of_json: missing tag" in
      let value =
        try OCamlList.assoc "key" l
        with Not_found -> failwith "MakeProduct.of_json: missing key" in
      (K.of_json key, V.of_json value)
    | _ -> failwith "Product.of_json: not an object"

  let read fd =
    lwt tag = K.read fd in
    lwt key = V.read fd in
    Lwt.return (tag, key)

  let write fd (key, value) =
    lwt () = K.write fd key in
    V.write fd value

end

module type STRINGABLE = sig
  type t
  val to_string: t -> string
  val of_string: string -> t
end

module String (C: CHANNEL) (S: STRINGABLE) = struct

  type t = S.t

  type channel = C.t

  let pretty s =
    Printf.sprintf "%S" (S.to_string s)

  let to_json t =
    IrminJSON.of_string (S.to_string t)

  let of_json j =
    S.of_string (IrminJSON.to_string j)

  cstruct hdr {
      uint32_t length
    } as big_endian

  let read fd =
    lwt buf = C.read fd sizeof_hdr in
    let len = Int32.to_int (get_hdr_length buf) in
    lwt str = C.read_string fd len in
    Lwt.return (S.of_string str)

  let write fd t =
    let str = S.to_string t in
    let len = String.length str in
    let buf = Cstruct.create sizeof_hdr in
    set_hdr_length buf (Int32.of_int len);
    lwt () = C.write fd buf in
    C.write_string fd str

end
