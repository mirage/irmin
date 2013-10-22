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

module type BASE = sig
  include IrminBase.S
  val merge: old:t -> t -> t
end


module Simple  = struct

  let debug fmt = IrminLog.debug "VALUE" fmt

  module S = IrminBase.PrivateString

  include S

  let name = "value"

  type key = K.t

  let dump = to_string

  let create = of_string

  let key v =
    K.of_string (to_string v)

  (* Simple scheme where we keep only the most recently changed
     string *)
  let merge ~old t1 t2 =
    if S.compare t1 t2 = 0 then t1
    else if S.compare old t1 = 0 then t2
    else if S.compare old t2 = 0 then t1
    else raise Conflict

end
