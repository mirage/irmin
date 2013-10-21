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
  val of_bytes: string -> t
  val of_ba: IrminBuffer.t -> t
  val to_hex: t -> string
  val of_hex: string -> t
  val concat: t list -> t
  val length: t -> int
  module Graph: IrminGraph.S with type Vertex.t = t
end

module SHA1 = struct

  let debug fmt = IrminLog.debug "SHA1" fmt

  module Key = struct

    include IrminBase.PrivateString

    let name = "key"

    let key_length = 20

    let sizeof _ =
      debug "sizeof";
      key_length

    let get buf =
      debug "get";
      let str = IrminBuffer.get_string buf key_length in
      debug " ... get %s" str;
      of_string str

    let set buf t =
      debug "set %s" (pretty t);
      let str = to_string t in
      IrminBuffer.set_string buf str

  end

  module Graph = IrminGraph.Make(Key)

  include Key

  let of_string str =
    Key.of_string (IrminMisc.sha1 str)

  let to_hex key =
    IrminMisc.hex_encode (Key.to_string key)

  let of_hex hex =
    Key.of_string (IrminMisc.hex_decode hex)

  let concat l =
    let l = List.fold_left (fun acc s -> Key.to_string s :: acc) [] l in
    let s = String.concat "" (List.sort String.compare l) in
    of_string s

  let length _ = key_length

end
