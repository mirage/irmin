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

exception Invalid of string

module type S = sig
  include Ir_hum.S
  val digest: Cstruct.t -> t
end

module SHA1 = struct

  module Log = Log.Make(struct let section = "SHA1" end)

  type t = Cstruct.t

  let to_hex t =
    let `Hex h = Hex.of_string (Cstruct.to_string t) in
    h

  let len = 20

  let hex_len = 40

  let of_hex hex =
    if String.length hex = hex_len then
      Cstruct.of_string (Hex.to_string (`Hex hex))
    else
      raise (Invalid hex)

  let to_sexp t = Sexplib.Sexp.Atom (to_hex t)
  let of_sexp s = of_hex (Sexplib.Conv.string_of_sexp s)
  let to_json x = Ezjsonm.of_sexp (to_sexp x)
  let of_json x = of_sexp (Ezjsonm.to_sexp x)

  let size_of t = Bin_prot.Size.bin_size_bigstring (Cstruct.to_bigarray t)

  let write t =
    Tc.Writer.of_bin_prot
      Bin_prot.Write.bin_write_bigstring
      (Cstruct.to_bigarray t)

  let read buf =
    let t =
      Tc.Reader.of_bin_prot Bin_prot.Read.bin_read_bigstring buf
      |> Cstruct.of_bigarray
    in
    if Cstruct.len t <> len then raise (Invalid (Cstruct.to_string t))
    else t

  let hash = Hashtbl.hash
  let equal = (=)
  let compare = Pervasives.compare

  let digest buf = Nocrypto.Hash.SHA1.digest buf
  let to_hum = to_hex
  let of_hum = of_hex
end
