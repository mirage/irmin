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

type sha1 = SHA1 of string

module SHA1 = struct

  module T = struct

    type t = sha1

    let compare (SHA1 k1) (SHA1 k2) = String.compare k1 k2

    let hash (SHA1 k) = Hashtbl.hash k

    let equal (SHA1 k1) (SHA1 k2) = String.compare k1 k2 = 0

  end

  include T

  let pretty (SHA1 k) =
    Printf.sprintf "%s" (IrminMisc.hex_encode k)

  let to_json (SHA1 k) =
    IrminJSON.of_string k

  let of_json j =
    SHA1 (IrminJSON.to_string j)

  let of_string str =
    SHA1 (IrminMisc.sha1 str)

  let to_hex (SHA1 str) =
    IrminMisc.hex_encode str

  let of_hex hex =
    SHA1 (IrminMisc.hex_decode hex)

  let concat l =
    let l = List.fold_left (fun acc (SHA1 s) -> s :: acc) [] l in
    let s = String.concat "" (List.sort String.compare l) in
    of_string s

  let key_length = 20

  let length (SHA1 _) = key_length

  let sizeof _ = key_length

  let read buf =
    lwt str = IrminIO.get_string buf key_length in
    Lwt.return (SHA1 str)

  let write buf (SHA1 str) =
    IrminIO.set_string buf str

  module Set = struct

    include Set.Make(T)

    let of_list l =
      List.fold_left (fun acc elt -> add elt acc) empty l

    let to_list s =
      elements s

    let pretty s =
      if is_empty s then "{}"
      else
        "{ "^ String.concat ", " (List.map pretty (to_list s)) ^ " }"

  end

end
