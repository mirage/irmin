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
  val create: string -> t
  val of_bytes: string -> t
  val of_buffer: IrminBuffer.t -> t
  val to_hex: t -> string
  val of_hex: string -> t
  val concat: t list -> t
  val length: t -> int
end

module SHA1 = struct

  let debug fmt = IrminLog.debug "SHA1" fmt

  include IrminBase.String

  exception Invalid of t

  exception Unknown of t

  let name = "key"

  let key_length = 20

  let create str =
    if String.length str = key_length then str
    else raise (Invalid str)

  let sizeof _ =
    debug "sizeof";
    key_length

  let get buf =
    debug "get";
    let str = IrminBuffer.get_string buf key_length in
    debug " ... get %s" str;
    str

  let set buf t =
    debug "set %s" (pretty t);
    IrminBuffer.set_string buf t

  let of_bytes str =
    IrminMisc.sha1 str

  let of_buffer buf =
    let len = IrminBuffer.length buf in
    let str = IrminBuffer.get_string buf len in
    of_bytes str

  let to_hex t =
    IrminMisc.hex_encode t

  let of_hex hex =
    IrminMisc.hex_decode hex

  let concat l =
    let l = List.fold_left (fun acc s -> s :: acc) [] l in
    String.concat "" (List.sort String.compare l)

  let length _ = key_length

end
