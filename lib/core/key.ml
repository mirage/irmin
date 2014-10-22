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
open Bin_prot.Std

exception Invalid of string
exception Unknown of string

module type S = sig
  include Tc.I0
  val pretty: t -> string
  val of_raw: string -> t
  val to_raw: t -> string
  val compute_from_cstruct: Cstruct.t -> t
  val compute_from_string: string -> t
end

module SHA1 = struct

  module Log = Log.Make(struct let section = "SHA1" end)

  let to_hex t =
    Hex.encode t

  let pretty = to_hex

  let len = 20
  let hex_len = 40

  let of_hex hex =
    if String.length hex = hex_len then
      Hex.decode hex
    else
      raise (Invalid hex)

  module M = Tc.I0(struct
      type t = string with bin_io, compare
      let sexp_of_t t =
        Sexplib.Sexp.Atom (to_hex t)
      let t_of_sexp s =
        of_hex (Sexplib.Conv.string_of_sexp s)
    end)

  include M

  let of_raw str =
    if String.length str = len then str
    else raise (Invalid str)

  let to_raw str =
    str

  let compute_from_cstruct buf =
    Cstruct.to_string (Nocrypto.Hash.SHA1.digest buf)

  let compute_from_string str =
    compute_from_cstruct (Cstruct.of_string str)

end
