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

open Core_kernel.Std

exception Invalid of string
exception Unknown of string

module type S = sig
  include IrminIdent.S
  val of_raw: string -> t
  val to_raw: t -> string
  val of_bytes: Bigstring.t -> t
  val of_bytes': string -> t
end

module SHA1 = struct

  module Log = Log.Make(struct let section = "SHA1" end)

  let to_hex t =
    IrminMisc.hex_encode t

  let of_hex hex =
    IrminMisc.hex_decode hex

  module M = IrminIdent.Make(struct
      type t = string with bin_io, compare
      let sexp_of_t t =
        Sexplib.Sexp.Atom (to_hex t)
      let t_of_sexp s =
        of_hex (Sexplib.Conv.string_of_sexp s)
    end)

  include M

  let len = 20

  let of_raw str =
    if Int.(String.length str = len) then str
    else raise (Invalid str)

  let to_raw str =
    str

  let of_bytes' str =
    Log.debug (lazy "of_bytes'");
    Sha1.(to_bin (string str))

  external update_buffer: Sha1.ctx -> Bigstring.t -> unit = "stub_sha1_update_bigarray"

  let of_bytes buf =
    Log.debug (lazy "of_bytes");
    let ctx = Sha1.init () in
    update_buffer ctx buf;
    Sha1.(to_bin (finalize ctx))

end
