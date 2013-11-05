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

exception Conflict

module type S = sig
  include IrminBase.S
  val of_bytes: string -> t
  val merge: old:t -> t -> t -> t
end

module Simple  = struct

  let debug fmt = IrminLog.debug "VALUE" fmt

  include IrminBase.String

  let name = "value"

  let create s = s

  let of_bytes s = s

  let merge ~old t1 t2 =
    if compare t1 t2 = 0 then t1
    else if compare old t1 = 0 then t2
    else if compare old t2 = 0 then t1
    else raise Conflict

end

module type STORE = sig
  include IrminStore.A
  include S with type t := value
end

module Make
    (K: IrminBase.S)
    (V: S)
    (S: IrminStore.A with type key = K.t and type value = V.t) =
struct

  include S

  include (V: S with type t := value)

end
