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
  val head: t
  val to_string: t -> string
  val of_string: string -> t
end

module Simple = struct
  include IrminBase.String
  let name = "tag"
  let head = "HEAD"
end

module type STORE = sig
  include S
  type key
  include IrminStore.M with type key := t and type value := key
end

module Make (S: IrminStore.MRAW) (T: S) (K: IrminKey.S with type t = S.value) = struct

  include T

  type key = K.t

  module Store = IrminStore.MakeM(S)(T)(K)

  include (Store: module type of Store with type key := T.t
                                        and type value := K.t)

end
