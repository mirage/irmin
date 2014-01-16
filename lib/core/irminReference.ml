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

open Lwt

module type S = sig
  include IrminKey.S
  val master: t
end

module Simple = struct

  include IrminBase.String

  let name = "reference"

  let master = "master"

  let of_pretty x = x

  let of_bytes x = x

  let of_bigarray x =
    of_bytes (IrminMisc.string_of_bigarray x)

  (* |-----|---------| *)
  (* | 'R' | PAYLOAD | *)
  (* |-----|---------| *)

  let header = "R"

  let sizeof t =
    1 + sizeof t

  let set buf t =
    Mstruct.set_string buf header;
    set buf t

  let get buf =
    let h = Mstruct.get_string buf 1 in
    if header <> h then None
    else get buf

end

module type STORE = sig
  include IrminStore.RW
  module Key: S with type t = key
  module Value: IrminKey.S with type t = value
end


module Make
    (K: S)
    (V: IrminKey.S)
    (S: IrminStore.RW with type key = K.t and type value = V.t)
= struct

  module Key = K

  module Value = V

  include S

  let list t _ =
    contents t >>= fun l ->
    return (List.map fst l)

end
