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

module type S = sig
  include IrminBase.S
  exception Invalid of t
  exception Unknown of t
  val of_string: string -> t
  val of_pretty: string -> t
end

module type BINARY = sig
  include S
  val of_bytes: string -> t
  val of_ba: IrminBuffer.ba -> t
  val to_hex: t -> string
  val of_hex: string -> t
end

module SHA1 = struct

  let debug fmt = IrminLog.debug "SHA1" fmt

  include IrminBase.String

  exception Invalid of t

  exception Unknown of t

  let name = "key"

  let len = 20

  let length = Some len

  let create str =
    if String.length str = len then str
    else raise (Invalid str)

  let sizeof _ =
    debug "sizeof";
    len

  let to_hex t =
    IrminMisc.hex_encode t

  let of_hex hex =
    IrminMisc.hex_decode hex

  let to_json t =
    IrminJSON.of_string (to_hex t)

  let of_json j =
    of_hex (IrminJSON.to_string j)

  let pretty = to_hex

  let of_pretty = of_hex

  let get buf =
    debug "get";
    let str = IrminBuffer.get_string buf len in
    debug "--> get %s" (pretty str);
    str

  let set buf t =
    debug "set %s" (pretty t);
    IrminBuffer.set_string buf t

  let of_bytes str =
    debug "of_bytes: %S" str;
    IrminMisc.sha1 str

  let of_ba ba =
    let buf = IrminBuffer.of_ba ba in
    let len = IrminBuffer.length buf in
    let str = IrminBuffer.get_string buf len in
    of_bytes str

end
