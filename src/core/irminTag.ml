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
  val master: t
  val to_string: t -> string
  val of_string: string -> t
end

module Simple = struct
  include IrminBase.String
  let name = "tag"
  let master = "master"
end

module type STORE = sig
  type tag
  type key
  include IrminStore.M with type key := tag and type value := key
  include S with type t := tag
end

module Make (S: IrminStore.M_RAW) (T: S) (K: IrminKey.S with type t = S.value) = struct

  type key = K.t

  type tag = T.t

  module Store = IrminStore.MakeM(S)(T)(K)

  include (Store: IrminStore.M with type key := tag and type value := key)

  include (T: S with type t := tag)

end
