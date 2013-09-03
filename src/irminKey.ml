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

type sha1 = SHA1 of string

module SHA1 = struct

  let debug fmt = IrminMisc.debug "SHA1" fmt

  let key_length = 20

  module T = struct

    type t = sha1

    let compare (SHA1 k1) (SHA1 k2) = String.compare k1 k2

    let hash (SHA1 k) = Hashtbl.hash k

    let equal (SHA1 k1) (SHA1 k2) = String.compare k1 k2 = 0

    let pretty (SHA1 k) =
      Printf.sprintf "%s" (IrminMisc.hex_encode k)

    let to_json (SHA1 k) =
      IrminJSON.of_string k

    let of_json j =
      SHA1 (IrminJSON.to_string j)

    let sizeof _ =
      debug "sizeof";
      key_length

    let get buf =
      let str = IrminIO.get_string buf key_length in
      SHA1 str

    let set buf (SHA1 str) =
      IrminIO.set_string buf str

  end

  module Set = IrminContainer.Set(T)

  module Graph = IrminContainer.Graph(struct
      include T
      module Set = Set
    end)

  include T

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

  let length (SHA1 _) = key_length

end
