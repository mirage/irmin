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
open Core_kernel.Std

module Log = Log.Make(struct let section = "REF" end)

module type S = sig
  include IrminKey.S
  val master: t
end

module Simple = struct
  include IrminPath
  let master = ["refs/heads/master"]
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
    return (List.map ~f:fst l)

end
